# ===========================================================================
# borg_ensemble() — Ensemble predictions from CV fold models
# ===========================================================================

#' Ensemble Predictions from CV Fold Models
#'
#' Combines predictions from models fitted on each CV fold into a
#' weighted ensemble. Each fold's model predicts on the full dataset
#' (or new data), and predictions are averaged with optional
#' performance-based weighting.
#'
#' @param data Data frame used for training.
#' @param folds A \code{borg_cv} object or fold list.
#' @param formula Model formula.
#' @param newdata Optional data frame for prediction. If \code{NULL},
#'   predicts on \code{data}.
#' @param fit_fun Function. Default: \code{lm}.
#' @param weight_by Character. Weighting scheme:
#'   \code{"equal"} (default) or \code{"performance"} (inverse error weight).
#' @param metric Character. Metric for performance weighting. Default: \code{"rmse"}.
#'
#' @return A list with class \code{"borg_ensemble"} containing:
#'   \describe{
#'     \item{prediction}{Numeric vector of ensemble predictions}
#'     \item{uncertainty}{Per-observation SD across fold predictions}
#'     \item{weights}{Fold weights used}
#'     \item{n_models}{Number of contributing models}
#'   }
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
#' cv <- borg_cv(d, coords = c("x", "y"), target = "z")
#' ens <- borg_ensemble(d, cv, z ~ x + y)
#' cor(d$z, ens$prediction)
#'
#' @export
borg_ensemble <- function(data, folds, formula, newdata = NULL,
                            fit_fun = stats::lm,
                            weight_by = c("equal", "performance"),
                            metric = c("rmse", "mae")) {
  weight_by <- match.arg(weight_by)
  metric <- match.arg(metric)
  fold_list <- if (inherits(folds, "borg_cv")) folds$folds else folds

  pred_data <- if (!is.null(newdata)) newdata else data
  n_pred <- nrow(pred_data)
  n_folds <- length(fold_list)
  target_var <- all.vars(formula)[1]

  # Fit models and predict
  pred_matrix <- matrix(NA_real_, nrow = n_pred, ncol = n_folds)
  fold_errors <- numeric(n_folds)

  for (i in seq_len(n_folds)) {
    fold <- fold_list[[i]]
    train_data <- data[fold$train, , drop = FALSE]

    model <- tryCatch(fit_fun(formula, data = train_data), error = function(e) NULL)
    if (is.null(model)) next

    pred_matrix[, i] <- tryCatch(
      stats::predict(model, newdata = pred_data),
      error = function(e) rep(NA_real_, n_pred)
    )

    # Fold performance (for weighting)
    test_data <- data[fold$test, , drop = FALSE]
    test_preds <- tryCatch(stats::predict(model, newdata = test_data),
                            error = function(e) rep(NA_real_, nrow(test_data)))
    actual <- test_data[[target_var]]
    fold_errors[i] <- switch(metric,
      "rmse" = sqrt(mean((actual - test_preds)^2, na.rm = TRUE)),
      "mae" = mean(abs(actual - test_preds), na.rm = TRUE)
    )
  }

  # Compute weights
  valid_folds <- !is.na(fold_errors) & fold_errors > 0
  if (weight_by == "performance" && any(valid_folds)) {
    w <- rep(0, n_folds)
    w[valid_folds] <- 1 / fold_errors[valid_folds]
    w <- w / sum(w)
  } else {
    w <- rep(1 / n_folds, n_folds)
  }

  # Weighted average
  prediction <- as.numeric(pred_matrix %*% w)
  uncertainty <- apply(pred_matrix, 1, stats::sd, na.rm = TRUE)

  result <- list(
    prediction = prediction,
    uncertainty = uncertainty,
    weights = w,
    n_models = sum(!is.na(fold_errors))
  )
  class(result) <- c("borg_ensemble", "list")
  result
}


#' @export
print.borg_ensemble <- function(x, ...) {
  cat("BORG Ensemble Prediction\n")
  cat(sprintf("  %d models combined\n", x$n_models))
  cat(sprintf("  Mean uncertainty (SD): %.4f\n", mean(x$uncertainty, na.rm = TRUE)))
  cat(sprintf("  Prediction range: [%.3f, %.3f]\n",
              min(x$prediction, na.rm = TRUE), max(x$prediction, na.rm = TRUE)))
  invisible(x)
}
