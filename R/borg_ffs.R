# ===========================================================================
# borg_forward_selection() — Forward feature selection with blocked CV
# ===========================================================================

#' Forward Feature Selection with Spatial/Blocked CV
#'
#' Selects variables using blocked cross-validation instead of random CV,
#' avoiding overfitting to spatial/temporal structure. At each step, adds
#' the variable that most improves the CV metric.
#'
#' Similar to CAST::ffs() but uses BORG's CV infrastructure and supports
#' any dependency structure (spatial, temporal, grouped).
#'
#' @param data Data frame.
#' @param target Character. Response variable name.
#' @param predictors Character vector. Candidate predictor names. If
#'   \code{NULL}, uses all numeric columns except target and coords.
#' @param folds A \code{borg_cv} object or fold list. If \code{NULL},
#'   generated automatically via \code{borg_cv()}.
#' @param coords Character vector of coordinate columns (for auto CV).
#' @param time Character. Time column (for auto CV).
#' @param groups Character. Group column (for auto CV).
#' @param metric Character. \code{"rmse"} (default), \code{"mae"}, or
#'   \code{"rsq"}.
#' @param fit_fun Function. Model fitting function. Default: \code{lm}.
#' @param min_vars Integer. Minimum variables to include. Default: 1.
#' @param verbose Logical. Default: FALSE.
#'
#' @return A list with class \code{"borg_ffs"} containing:
#'   \describe{
#'     \item{selected}{Character vector of selected variable names (in order)}
#'     \item{history}{Data frame: step, variable_added, metric_value, n_vars}
#'     \item{best_metric}{Best CV metric achieved}
#'     \item{all_vars}{All candidate variables}
#'   }
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(
#'   x = runif(100), y = runif(100),
#'   important = rnorm(100), noise1 = rnorm(100), noise2 = rnorm(100)
#' )
#' d$z <- d$important * 2 + rnorm(100, sd = 0.5)
#' ffs <- borg_forward_selection(d, target = "z",
#'                                 predictors = c("important", "noise1", "noise2"),
#'                                 coords = c("x", "y"))
#' ffs$selected
#'
#' @export
borg_forward_selection <- function(data, target, predictors = NULL,
                                     folds = NULL,
                                     coords = NULL, time = NULL, groups = NULL,
                                     metric = c("rmse", "mae", "rsq"),
                                     fit_fun = stats::lm,
                                     min_vars = 1L, verbose = FALSE) {
  metric <- match.arg(metric)
  minimize <- metric %in% c("rmse", "mae")

  # Determine predictors
  exclude <- c(target, coords, time, groups)
  if (is.null(predictors)) {
    predictors <- setdiff(names(data), exclude)
    predictors <- predictors[vapply(data[predictors], is.numeric, logical(1))]
  }

  if (length(predictors) == 0) stop("No candidate predictors")

  # Generate folds if not provided
  if (is.null(folds)) {
    folds <- borg_cv(data, coords = coords, time = time, groups = groups,
                      target = target, verbose = FALSE)
  }
  fold_list <- if (inherits(folds, "borg_cv")) folds$folds else folds

  # Helper: evaluate a variable set
  eval_vars <- function(vars) {
    formula <- stats::as.formula(paste(target, "~", paste(vars, collapse = " + ")))
    vals <- vapply(fold_list, function(fold) {
      m <- tryCatch(fit_fun(formula, data = data[fold$train, , drop = FALSE]),
                     error = function(e) NULL)
      if (is.null(m)) return(NA_real_)
      p <- tryCatch(stats::predict(m, newdata = data[fold$test, , drop = FALSE]),
                     error = function(e) rep(NA_real_, length(fold$test)))
      a <- data[[target]][fold$test]
      switch(metric,
        "rmse" = sqrt(mean((a - p)^2, na.rm = TRUE)),
        "mae" = mean(abs(a - p), na.rm = TRUE),
        "rsq" = {
          ss <- sum((a - p)^2, na.rm = TRUE)
          st <- sum((a - mean(a, na.rm = TRUE))^2, na.rm = TRUE)
          if (st == 0) NA_real_ else 1 - ss / st
        }
      )
    }, numeric(1))
    mean(vals, na.rm = TRUE)
  }

  # Forward selection
  selected <- character(0)
  remaining <- predictors
  history <- list()
  best_overall <- if (minimize) Inf else -Inf

  for (step in seq_along(predictors)) {
    if (length(remaining) == 0) break

    # Evaluate each candidate addition
    candidate_scores <- vapply(remaining, function(var) {
      eval_vars(c(selected, var))
    }, numeric(1))

    best_idx <- if (minimize) which.min(candidate_scores) else which.max(candidate_scores)
    best_var <- remaining[best_idx]
    best_score <- candidate_scores[best_idx]

    # Check if improvement over previous step
    improved <- if (step == 1) TRUE
                else if (minimize) best_score < best_overall
                else best_score > best_overall

    if (!improved && length(selected) >= min_vars) break

    selected <- c(selected, best_var)
    remaining <- setdiff(remaining, best_var)
    best_overall <- best_score

    history[[step]] <- data.frame(
      step = step,
      variable_added = best_var,
      metric_value = best_score,
      n_vars = length(selected),
      stringsAsFactors = FALSE
    )

    if (verbose) {
      message(sprintf("Step %d: +%s -> %s = %.4f (%d vars)",
                       step, best_var, toupper(metric), best_score, length(selected)))
    }
  }

  result <- list(
    selected = selected,
    history = do.call(rbind, history),
    best_metric = best_overall,
    metric = metric,
    all_vars = predictors
  )
  class(result) <- c("borg_ffs", "list")
  result
}


#' @export
print.borg_ffs <- function(x, ...) {
  cat("BORG Forward Feature Selection\n")
  cat(sprintf("  Selected: %s\n", paste(x$selected, collapse = " + ")))
  cat(sprintf("  Best %s: %.4f\n", toupper(x$metric), x$best_metric))
  cat(sprintf("  %d of %d variables selected\n", length(x$selected), length(x$all_vars)))
  invisible(x)
}
