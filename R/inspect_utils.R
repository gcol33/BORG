# Null-coalescing operator (internal)
# Single definition - used across multiple files
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Create a risk list entry
#' @noRd
.new_risk <- function(type, severity, description,
                      affected_indices = integer(0),
                      source_object = NA_character_) {
  list(
    type = type,
    severity = severity,
    description = description,
    affected_indices = affected_indices,
    source_object = source_object
  )
}


#' Create a BorgRisk S4 object from a list of risks
#' @noRd
.make_borg_risk <- function(risks, train_idx = NULL, test_idx = NULL,
                            call = NULL) {
  n_hard <- sum(vapply(risks, function(r) r$severity == "hard_violation",
                       logical(1)))
  n_soft <- sum(vapply(risks, function(r) r$severity == "soft_inflation",
                       logical(1)))

  new("BorgRisk",
    risks = risks,
    n_hard = as.integer(n_hard),
    n_soft = as.integer(n_soft),
    is_valid = n_hard == 0L,
    train_indices = as.integer(train_idx %||% integer(0)),
    test_indices = as.integer(test_idx %||% integer(0)),
    timestamp = Sys.time(),
    call = call %||% match.call(sys.function(-1), sys.call(-1))
  )
}


#' Risk entry for a train-scope leak (Internal)
#'
#' A model/CV object reporting a fitted or resampled size that differs from
#' the training set size indicates it saw data outside the training fold.
#' Returns a one-element risk list, or an empty list when the counts match
#' (within \code{tolerance}), are unavailable, or the observed direction
#' (over/under) is excluded via \code{check_over}/\code{check_under}.
#'
#' \code{type}, \code{severity_over}/\code{severity_under} may be a fixed
#' value or a function of \code{(n_model, n_expected)}, and
#' \code{describe_over}/\code{describe_under} may be a fixed string or a
#' function of \code{(source_label, n_model, n_expected)} -- for call sites
#' whose type, severity, or wording depends on the size of the mismatch
#' (e.g. "used the full dataset" vs "used somewhat more than expected") or
#' differs between "fitted on more than expected" (often a leak) and
#' "fitted on less than expected" (often incomplete, not a leak).
#' \code{affected_indices_over}/\code{affected_indices_under} may likewise be
#' a fixed vector or a function of \code{(n_model, n_expected)}; they default
#' to \code{test_idx}, but call sites that only mean to flag a size mismatch
#' without implicating specific rows (e.g. "some training data may have been
#' excluded") can pass \code{integer(0)}.
#' \code{only_if}, when supplied, is an additional
#' \code{function(n_model, n_expected)} predicate gating whether a risk is
#' reported at all.
#' @noRd
.check_train_scope <- function(n_model, train_idx, test_idx, source_label,
                               type = "model_scope",
                               severity_over = "hard_violation",
                               severity_under = severity_over,
                               describe_over = function(label, n, expected) sprintf(
                                 "%s was fitted on %d observations, but the training set has %d. Possible data leakage.",
                                 label, n, expected
                               ),
                               describe_under = describe_over,
                               affected_indices_over = test_idx,
                               affected_indices_under = affected_indices_over,
                               check_over = TRUE,
                               check_under = TRUE,
                               tolerance = 0,
                               only_if = NULL) {
  n_expected <- length(train_idx)
  if (length(n_model) != 1 || is.na(n_model)) {
    return(list())
  }

  over <- n_model > n_expected * (1 + tolerance)
  under <- n_model < n_expected * (1 - tolerance)
  if (!over && !under) return(list())
  if (over && !check_over) return(list())
  if (under && !check_under) return(list())
  if (!is.null(only_if) && !isTRUE(only_if(n_model, n_expected))) return(list())

  resolve <- function(x) if (is.function(x)) x(n_model, n_expected) else x
  describe <- if (over) describe_over else describe_under
  description <- if (is.function(describe)) describe(source_label, n_model, n_expected) else describe

  list(.new_risk(
    type = resolve(type),
    severity = resolve(if (over) severity_over else severity_under),
    description = description,
    affected_indices = resolve(if (over) affected_indices_over else affected_indices_under),
    source_object = source_label
  ))
}


#' Check if analysis indices leak test data (shared CV leak detection)
#'
#' Core primitive behind every "does this CV fold/resample/split contain
#' indices from a reference set" check in the package: iterates a list of
#' index sets (\code{folds}) and flags any that intersect \code{test_idx}.
#' Returns a list of risk entries (empty if no leak).
#'
#' \code{type}, \code{describe}, and \code{source_object} default to the
#' original "CV fold contains held-out test indices" framing, but can be
#' overridden by call sites checking a different (but structurally
#' identical) intersection -- e.g. a single pair of index sets rather than a
#' list of folds (pass \code{folds} as a one-element list), or the reverse
#' direction (train indices leaking into an assessment set).
#' @noRd
.check_cv_leak <- function(folds, test_idx, source_prefix = "fold",
                           severity = "hard_violation",
                           type = "cv_leak",
                           describe = function(prefix, i, n) sprintf(
                             "%s %d contains %d test indices. Test data is being used in CV.",
                             prefix, i, n
                           ),
                           source_object = function(prefix, i) sprintf("%s[[%d]]", prefix, i)) {
  risks <- list()
  for (i in seq_along(folds)) {
    fold_idx <- folds[[i]]
    if (is.null(fold_idx)) next
    leaked <- intersect(fold_idx, test_idx)
    if (length(leaked) > 0) {
      risks <- c(risks, list(.new_risk(
        type = type,
        severity = severity,
        description = describe(source_prefix, i, length(leaked)),
        affected_indices = leaked,
        source_object = source_object(source_prefix, i)
      )))
    }
  }
  risks
}


#' Detect target leakage via correlation thresholds
#'
#' Shared between inspect_data.R and validate_leakage.R. Returns risk entries
#' for features with |cor| > 0.99 (hard) or 0.95-0.99 (soft) with target.
#' @noRd
.detect_correlation_leakage <- function(features, target, train_idx,
                                        target_col_name = "target") {
  risks <- list()

  for (col in names(features)) {
    feat <- features[[col]]
    if (!is.numeric(feat)) next

    feat_train <- feat[train_idx]
    target_train <- target[train_idx]

    # Skip constant features/targets (sd = 0 would produce NaN correlation)
    if (sd(feat_train, na.rm = TRUE) == 0 || sd(target_train, na.rm = TRUE) == 0) next

    train_cor <- tryCatch(
      cor(feat_train, target_train, use = "pairwise.complete.obs"),
      error = function(e) NA
    )
    if (is.na(train_cor)) next

    abs_cor <- abs(train_cor)

    if (abs_cor > 0.99) {
      risks <- c(risks, list(.new_risk(
        type = "target_leakage_direct",
        severity = "hard_violation",
        description = sprintf(
          "Feature '%s' has correlation %.3f with target '%s'. Likely derived from outcome.",
          col, train_cor, target_col_name
        ),
        source_object = sprintf("data.frame$%s", col)
      )))
    } else if (abs_cor >= 0.95) {
      risks <- c(risks, list(.new_risk(
        type = "target_leakage_proxy",
        severity = "soft_inflation",
        description = sprintf(
          "Feature '%s' has correlation %.3f with target '%s'. May be a proxy for outcome.",
          col, train_cor, target_col_name
        ),
        source_object = sprintf("data.frame$%s", col)
      )))
    }
  }

  risks
}


#' @noRd
.inspect_generic <- function(object, train_idx, test_idx, data, ...) {
 # Generic fallback: no specific inspection available
 list()
}

#' Check if object is a caret trainControl
#' @noRd
.is_trainControl <- function(object) {
  # trainControl is a list with specific fields
  if (!is.list(object)) return(FALSE)

  # Must have these fields to be a trainControl
  required_fields <- c("method", "number", "repeats")
  has_required <- all(required_fields %in% names(object))

  # Method should be a character string for CV type
  valid_method <- has_required &&
    is.character(object$method) &&
    length(object$method) == 1

  has_required && valid_method
}

#' Check if stored parameters diverge from train-only recomputed values
#'
#' Compares stored preprocessing parameters against train-only values using
#' relative tolerance. Returns a risk list entry if divergence detected, or NULL.
#' @noRd
.check_param_leak <- function(stored, recomputed, description, source_object,
                              test_idx, rel_tol = 1e-10) {
  diff_vals <- abs(recomputed - stored)
  max_diff <- max(diff_vals, na.rm = TRUE)

  data_scale <- max(abs(stored), na.rm = TRUE)
  threshold <- if (data_scale > 0) rel_tol * data_scale else rel_tol

  if (max_diff > threshold) {
    list(
      type = "preprocessing_leak",
      severity = "hard_violation",
      description = sprintf(description, max_diff),
      affected_indices = test_idx,
      source_object = source_object
    )
  }
}

#' Point in polygon test (ray casting)
#' @noRd
.point_in_polygon <- function(point, polygon) {
  x <- point[1]
  y <- point[2]
  n <- nrow(polygon)
  inside <- FALSE

  j <- n
  for (i in seq_len(n)) {
    xi <- polygon[i, 1]
    yi <- polygon[i, 2]
    xj <- polygon[j, 1]
    yj <- polygon[j, 2]

    if (((yi > y) != (yj > y)) &&
        (x < (xj - xi) * (y - yi) / (yj - yi) + xi)) {
      inside <- !inside
    }
    j <- i
  }

  inside
}
