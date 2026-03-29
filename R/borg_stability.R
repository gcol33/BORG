# ===========================================================================
# borg_stability() — Fold stability analysis across repeated CV
# ===========================================================================

#' Analyze CV Fold Stability Across Repeats
#'
#' Quantifies how much CV results and fold assignments change across
#' repeated runs. Uses a \code{borg_cv} object with \code{repeats > 1}.
#'
#' @param object A \code{borg_cv} object with \code{repeats > 1}
#'   (i.e., \code{object$all_folds} is not NULL).
#' @param data Data frame for computing per-repeat performance.
#' @param formula Model formula.
#' @param metric Character. \code{"rmse"} (default), \code{"mae"}, or \code{"rsq"}.
#' @param fit_fun Function. Model fitting function. Default: \code{lm}.
#'
#' @return A list with class \code{"borg_stability"} containing:
#'   \describe{
#'     \item{metric_stability}{Data frame: repeat, mean_metric, sd_metric}
#'     \item{metric_cv}{Coefficient of variation of mean metric across repeats}
#'     \item{assignment_stability}{Mean Jaccard similarity of test sets between repeats}
#'     \item{summary}{Character assessment: "stable", "moderate", or "unstable"}
#'   }
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
#' cv <- borg_cv(d, coords = c("x", "y"), target = "z", repeats = 5)
#' stab <- borg_stability(cv, d, z ~ x + y)
#' stab
#'
#' @export
borg_stability <- function(object, data, formula,
                             metric = c("rmse", "mae", "rsq"),
                             fit_fun = stats::lm) {
  metric <- match.arg(metric)

  if (is.null(object$all_folds)) {
    stop("borg_stability requires repeats > 1. Use borg_cv(..., repeats = N)")
  }

  all_folds <- object$all_folds
  n_repeats <- length(all_folds)
  target_var <- all.vars(formula)[1]

  # Per-repeat performance
  repeat_metrics <- lapply(seq_len(n_repeats), function(r) {
    folds <- all_folds[[r]]
    fold_vals <- vapply(folds, function(fold) {
      train_data <- data[fold$train, , drop = FALSE]
      test_data <- data[fold$test, , drop = FALSE]
      model <- tryCatch(fit_fun(formula, data = train_data), error = function(e) NULL)
      if (is.null(model)) return(NA_real_)
      preds <- tryCatch(stats::predict(model, newdata = test_data),
                         error = function(e) rep(NA_real_, nrow(test_data)))
      actual <- test_data[[target_var]]
      switch(metric,
        "rmse" = sqrt(mean((actual - preds)^2, na.rm = TRUE)),
        "mae" = mean(abs(actual - preds), na.rm = TRUE),
        "rsq" = {
          ss_res <- sum((actual - preds)^2, na.rm = TRUE)
          ss_tot <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
          if (ss_tot == 0) NA_real_ else 1 - ss_res / ss_tot
        }
      )
    }, numeric(1))

    data.frame(
      repeat_id = r,
      mean_metric = mean(fold_vals, na.rm = TRUE),
      sd_metric = stats::sd(fold_vals, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  metric_df <- do.call(rbind, repeat_metrics)
  metric_cv <- stats::sd(metric_df$mean_metric) / mean(metric_df$mean_metric)

  # Fold assignment stability (Jaccard similarity between repeats)
  n_folds <- length(all_folds[[1]])
  jaccard_vals <- numeric(0)

  if (n_repeats >= 2) {
    for (r1 in seq_len(n_repeats - 1)) {
      for (r2 in (r1 + 1):n_repeats) {
        for (f in seq_len(n_folds)) {
          t1 <- all_folds[[r1]][[f]]$test
          t2 <- all_folds[[r2]][[f]]$test
          intersection <- length(intersect(t1, t2))
          union_size <- length(union(t1, t2))
          if (union_size > 0) {
            jaccard_vals <- c(jaccard_vals, intersection / union_size)
          }
        }
      }
    }
  }

  mean_jaccard <- if (length(jaccard_vals) > 0) mean(jaccard_vals) else NA_real_

  # Assessment
  assessment <- if (metric_cv < 0.05) "stable"
                else if (metric_cv < 0.15) "moderate"
                else "unstable"

  result <- list(
    metric_stability = metric_df,
    metric_cv = metric_cv,
    assignment_stability = mean_jaccard,
    summary = assessment,
    metric = metric,
    n_repeats = n_repeats
  )
  class(result) <- c("borg_stability", "list")
  result
}


#' @export
print.borg_stability <- function(x, ...) {
  cat("BORG Fold Stability Analysis\n")
  cat("============================\n\n")
  cat(sprintf("Repeats: %d\n", x$n_repeats))
  cat(sprintf("Metric CV: %.1f%% (%s)\n", x$metric_cv * 100, x$summary))
  if (!is.na(x$assignment_stability)) {
    cat(sprintf("Fold Jaccard similarity: %.3f\n", x$assignment_stability))
  }
  cat(sprintf("\nPer-repeat %s:\n", toupper(x$metric)))
  for (i in seq_len(nrow(x$metric_stability))) {
    r <- x$metric_stability[i, ]
    cat(sprintf("  Repeat %d: %.4f (SD %.4f)\n", r$repeat_id, r$mean_metric, r$sd_metric))
  }
  invisible(x)
}
