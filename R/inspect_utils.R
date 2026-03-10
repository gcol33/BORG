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


#' Check if analysis indices leak test data (shared CV leak detection)
#'
#' Returns a list of risk entries (empty if no leak).
#' @noRd
.check_cv_leak <- function(folds, test_idx, source_prefix = "fold",
                           severity = "hard_violation") {
  risks <- list()
  for (i in seq_along(folds)) {
    fold_idx <- folds[[i]]
    leaked <- intersect(fold_idx, test_idx)
    if (length(leaked) > 0) {
      risks <- c(risks, list(.new_risk(
        type = "cv_leak",
        severity = severity,
        description = sprintf(
          "%s %d contains %d test indices. Test data is being used in CV.",
          source_prefix, i, length(leaked)
        ),
        affected_indices = leaked,
        source_object = sprintf("%s[[%d]]", source_prefix, i)
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
