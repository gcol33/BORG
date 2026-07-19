# ===========================================================================
# Classification metrics for spatial CV evaluation
# ===========================================================================

#' Compute a Model Evaluation Metric (Internal)
#'
#' Single metric dispatcher for the package: rmse, mae, rsq, auc, tss,
#' kappa, sensitivity, specificity, and accuracy. Used by every CV
#' comparison and inspection function that scores predictions
#' (\code{borg_compare_cv}, \code{borg_compare_models},
#' \code{borg_fold_performance}, \code{borg_workflow},
#' \code{borg_transferability}, \code{borg_fairness}, \code{borg_importance}).
#'
#' \code{predicted} is normally a numeric prediction or class-1 probability,
#' thresholded at \code{threshold} for the classification metrics. When
#' \code{metric == "accuracy"} and \code{predicted} is not numeric (e.g. a
#' factor or character vector of predicted class labels), predictions are
#' compared to \code{actual} directly instead of being thresholded.
#' @noRd
.classify_metric <- function(actual, predicted, metric, threshold = 0.5) {
  if (metric == "accuracy" && !is.numeric(predicted)) {
    return(mean(predicted == actual, na.rm = TRUE))
  }

  switch(metric,
    "auc" = .compute_auc(actual, predicted),
    "tss" = {
      pred_class <- as.integer(predicted >= threshold)
      tp <- sum(pred_class == 1 & actual == 1)
      tn <- sum(pred_class == 0 & actual == 0)
      fp <- sum(pred_class == 1 & actual == 0)
      fn <- sum(pred_class == 0 & actual == 1)
      sens <- if ((tp + fn) > 0) tp / (tp + fn) else 0
      spec <- if ((tn + fp) > 0) tn / (tn + fp) else 0
      sens + spec - 1
    },
    "kappa" = {
      pred_class <- as.integer(predicted >= threshold)
      n <- length(actual)
      tp <- sum(pred_class == 1 & actual == 1)
      tn <- sum(pred_class == 0 & actual == 0)
      fp <- sum(pred_class == 1 & actual == 0)
      fn <- sum(pred_class == 0 & actual == 1)
      po <- (tp + tn) / n
      pe <- ((tp + fp) * (tp + fn) + (tn + fp) * (tn + fn)) / n^2
      if (pe >= 1) 0 else (po - pe) / (1 - pe)
    },
    "sensitivity" = {
      pred_class <- as.integer(predicted >= threshold)
      tp <- sum(pred_class == 1 & actual == 1)
      fn <- sum(pred_class == 0 & actual == 1)
      if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    },
    "specificity" = {
      pred_class <- as.integer(predicted >= threshold)
      tn <- sum(pred_class == 0 & actual == 0)
      fp <- sum(pred_class == 1 & actual == 0)
      if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
    },
    "accuracy" = {
      pred_class <- as.integer(predicted >= threshold)
      mean(pred_class == actual, na.rm = TRUE)
    },
    # Regression fallback
    "rmse" = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    "mae" = mean(abs(actual - predicted), na.rm = TRUE),
    "rsq" = {
      ss <- sum((actual - predicted)^2, na.rm = TRUE)
      st <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
      if (st == 0) NA_real_ else 1 - ss / st
    },
    stop(sprintf("Unknown metric: %s", metric))
  )
}



#' Available Classification Metrics
#'
#' Returns the valid classification metric names.
#'
#' @return Character vector of available classification metrics.
#'
#' @examples
#' borg_metrics()
#'
#' @export
borg_metrics <- function() {
  c(
    # Classification
    "auc", "tss", "kappa", "sensitivity", "specificity", "accuracy",
    # Regression
    "rmse", "mae", "rsq"
  )
}
