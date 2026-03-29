# ===========================================================================
# Classification metrics for spatial CV evaluation
# ===========================================================================

#' Compute Classification Metrics (Internal)
#'
#' Used by borg_fold_performance and borg_workflow when target is binary.
#' @noRd
.classify_metric <- function(actual, predicted_prob, metric, threshold = 0.5) {
  switch(metric,
    "auc" = .compute_auc(actual, predicted_prob),
    "tss" = {
      pred_class <- as.integer(predicted_prob >= threshold)
      tp <- sum(pred_class == 1 & actual == 1)
      tn <- sum(pred_class == 0 & actual == 0)
      fp <- sum(pred_class == 1 & actual == 0)
      fn <- sum(pred_class == 0 & actual == 1)
      sens <- if ((tp + fn) > 0) tp / (tp + fn) else 0
      spec <- if ((tn + fp) > 0) tn / (tn + fp) else 0
      sens + spec - 1
    },
    "kappa" = {
      pred_class <- as.integer(predicted_prob >= threshold)
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
      pred_class <- as.integer(predicted_prob >= threshold)
      tp <- sum(pred_class == 1 & actual == 1)
      fn <- sum(pred_class == 0 & actual == 1)
      if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    },
    "specificity" = {
      pred_class <- as.integer(predicted_prob >= threshold)
      tn <- sum(pred_class == 0 & actual == 0)
      fp <- sum(pred_class == 1 & actual == 0)
      if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
    },
    "accuracy" = {
      pred_class <- as.integer(predicted_prob >= threshold)
      mean(pred_class == actual, na.rm = TRUE)
    },
    # Regression fallback
    "rmse" = sqrt(mean((actual - predicted_prob)^2, na.rm = TRUE)),
    "mae" = mean(abs(actual - predicted_prob), na.rm = TRUE),
    "rsq" = {
      ss <- sum((actual - predicted_prob)^2, na.rm = TRUE)
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
