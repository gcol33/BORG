# ===========================================================================
# borg_fairness() — Leakage x fairness interaction analysis
# ===========================================================================

#' Detect Performance Disparities Across Subgroups Under Blocked CV
#'
#' Compares model performance across spatial, temporal, or categorical
#' subgroups under both random and blocked CV. Leakage can mask
#' disparate performance — a model that looks uniformly good under
#' random CV may show large subgroup-level gaps when evaluated honestly.
#'
#' @param data A data frame with predictors and response.
#' @param model A fitted model object with a \code{predict} method.
#' @param target Character. Response variable name.
#' @param group Character. Column name defining subgroups (e.g., region,
#'   time period, site type). If NULL and \code{coords} is provided,
#'   subgroups are derived from spatial clusters.
#' @param coords Character vector of length 2. Coordinate column names.
#'   Used for spatial subgroup clustering if \code{group} is NULL.
#' @param n_groups Integer. Number of spatial clusters when auto-grouping.
#'   Default: 4.
#' @param folds_blocked A list of train/test folds from
#'   \code{\link{borg_cv}}. If NULL, generated automatically.
#' @param folds_random A list of random CV folds. If NULL, generated
#'   automatically.
#' @param metric Character. Metric to compute: \code{"rmse"}, \code{"mae"},
#'   or \code{"rsq"}. Default: \code{"rmse"}.
#' @param v Integer. Number of folds if generating automatically.
#'   Default: 5.
#' @param ... Additional arguments passed to \code{predict}.
#'
#' @return A list with class \code{"borg_fairness"} containing:
#'   \describe{
#'     \item{subgroup_metrics}{Data frame with columns: group, metric_random,
#'       metric_blocked, n_obs, disparity (blocked - random), and
#'       relative_disparity (percent change).}
#'     \item{overall}{List with overall random and blocked metrics.}
#'     \item{max_disparity}{The largest subgroup-level performance gap
#'       between random and blocked CV.}
#'     \item{worst_group}{The subgroup with the worst blocked CV
#'       performance.}
#'     \item{hidden_by_leakage}{Logical. TRUE if any subgroup's blocked
#'       performance is substantially worse than random CV suggested.}
#'     \item{metric}{The metric used.}
#'   }
#'
#' @details
#' This function evaluates whether leakage disproportionately affects
#' certain data subgroups. A model may have uniform performance under
#' random CV but show large disparities when spatial or temporal
#' independence is enforced.
#'
#' For example, in ecological modelling, a random-CV RMSE of 0.5 across
#' all regions may hide the fact that spatially blocked CV yields RMSE
#' of 0.3 in data-rich regions but 1.2 in undersampled regions.
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' d <- data.frame(x = runif(200), y = runif(200))
#' d$z <- sin(d$x * 3) + rnorm(200, sd = 0.3)
#' model <- lm(z ~ x + y, data = d)
#' fair <- borg_fairness(d, model, target = "z", coords = c("x", "y"))
#' fair
#' }
#'
#' @seealso \code{\link{borg_cv}}, \code{\link{borg_compare_cv}}
#'
#' @export
borg_fairness <- function(data, model, target,
                          group = NULL, coords = NULL,
                          n_groups = 4L,
                          folds_blocked = NULL, folds_random = NULL,
                          metric = c("rmse", "mae", "rsq"),
                          v = 5L, ...) {

  metric <- match.arg(metric)

  if (!target %in% names(data)) {
    stop(sprintf("target '%s' not found in data", target))
  }

  actual <- data[[target]]

  # Determine subgroups
  if (is.null(group)) {
    if (is.null(coords)) {
      stop("Provide group or coords for subgroup definition")
    }
    # Auto-cluster
    coord_mat <- cbind(data[[coords[1]]], data[[coords[2]]])
    km <- stats::kmeans(coord_mat, centers = n_groups, nstart = 10)
    group_ids <- km$cluster
    group_label <- "spatial_cluster"
  } else {
    if (!group %in% names(data)) {
      stop(sprintf("group '%s' not found in data", group))
    }
    group_ids <- data[[group]]
    group_label <- group
  }

  # Generate folds if needed
  if (is.null(folds_blocked)) {
    cv_args <- list(data = data, v = v, target = target)
    if (!is.null(coords)) cv_args$coords <- coords
    cv_blocked <- do.call(borg_cv, cv_args)
    folds_blocked <- cv_blocked$folds
  }

  if (is.null(folds_random)) {
    n <- nrow(data)
    idx <- sample(rep(seq_len(v), length.out = n))
    folds_random <- lapply(seq_len(v), function(i) {
      list(train = which(idx != i), test = which(idx == i))
    })
  }

  # Compute per-observation predictions under each CV
  preds_random <- .cv_predictions(data, model, folds_random, target, ...)
  preds_blocked <- .cv_predictions(data, model, folds_blocked, target, ...)

  # Per-group metrics
  unique_groups <- sort(unique(group_ids))
  results <- lapply(unique_groups, function(g) {
    mask <- group_ids == g
    n_g <- sum(mask)
    met_rand <- .compute_metric(actual[mask], preds_random[mask], metric)
    met_block <- .compute_metric(actual[mask], preds_blocked[mask], metric)

    data.frame(
      group = g,
      metric_random = met_rand,
      metric_blocked = met_block,
      n_obs = n_g,
      disparity = met_block - met_rand,
      stringsAsFactors = FALSE
    )
  })

  subgroup_metrics <- do.call(rbind, results)
  subgroup_metrics$relative_disparity <- ifelse(
    subgroup_metrics$metric_random != 0,
    100 * subgroup_metrics$disparity / abs(subgroup_metrics$metric_random),
    NA_real_
  )

  # Overall metrics
  overall_random <- .compute_metric(actual, preds_random, metric)
  overall_blocked <- .compute_metric(actual, preds_blocked, metric)

  # For error metrics (rmse, mae): higher blocked = worse = hidden by leakage

  # For rsq: lower blocked = worse
  if (metric %in% c("rmse", "mae")) {
    worst_idx <- which.max(subgroup_metrics$metric_blocked)
    hidden <- any(subgroup_metrics$disparity > 0.1 * abs(overall_random), na.rm = TRUE)
  } else {
    worst_idx <- which.min(subgroup_metrics$metric_blocked)
    hidden <- any(subgroup_metrics$disparity < -0.1 * abs(overall_random), na.rm = TRUE)
  }

  result <- list(
    subgroup_metrics = subgroup_metrics,
    overall = list(random = overall_random, blocked = overall_blocked),
    max_disparity = max(abs(subgroup_metrics$disparity), na.rm = TRUE),
    worst_group = subgroup_metrics$group[worst_idx],
    hidden_by_leakage = hidden,
    metric = metric,
    group_label = group_label,
    n_groups = length(unique_groups)
  )

  class(result) <- "borg_fairness"
  result
}


#' @noRd
.cv_predictions <- function(data, model, folds, target, ...) {
  preds <- rep(NA_real_, nrow(data))

  for (fold in folds) {
    train_data <- data[fold$train, , drop = FALSE]
    test_data <- data[fold$test, , drop = FALSE]

    # Refit model on training fold
    refit <- tryCatch(
      stats::update(model, data = train_data),
      error = function(e) NULL
    )

    if (!is.null(refit)) {
      fold_preds <- tryCatch(
        stats::predict(refit, newdata = test_data, ...),
        error = function(e) rep(NA_real_, length(fold$test))
      )
      preds[fold$test] <- fold_preds
    }
  }

  preds
}


#' @export
print.borg_fairness <- function(x, ...) {
  cat("BORG Fairness Analysis\n")
  cat(sprintf("  Metric: %s | Groups: %d (%s)\n",
              toupper(x$metric), x$n_groups, x$group_label))
  cat(sprintf("  Overall %s: %.4f (random) vs %.4f (blocked)\n",
              x$metric, x$overall$random, x$overall$blocked))
  cat(sprintf("  Max subgroup disparity: %.4f\n", x$max_disparity))
  cat(sprintf("  Worst group: %s\n", x$worst_group))
  if (x$hidden_by_leakage) {
    cat("  WARNING: Leakage hides substantial performance gaps between subgroups.\n")
  } else {
    cat("  Performance is relatively consistent across subgroups.\n")
  }
  cat("\n")
  print(x$subgroup_metrics, row.names = FALSE)
  invisible(x)
}
