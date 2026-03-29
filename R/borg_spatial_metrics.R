# ===========================================================================
# Spatial metrics and diagnostics bundle
# ===========================================================================

#' Per-Fold Environmental Similarity (MESS)
#'
#' Computes the Multivariate Environmental Similarity Surface (MESS) for
#' each CV fold, showing how representative each fold's test set is
#' relative to the training set in environmental space.
#'
#' @param data Data frame.
#' @param folds A \code{borg_cv} object or fold list.
#' @param predictors Character vector. If \code{NULL}, all numeric columns used.
#'
#' @return A data frame with per-fold similarity metrics.
#'
#' @references
#' Elith, J., Kearney, M., & Phillips, S. (2010). The art of modelling
#' range-shifting species. \emph{Methods in Ecology and Evolution}, 1(4),
#' 330-342. \doi{10.1111/j.2041-210X.2010.00036.x}
#'
#' @export
borg_fold_similarity <- function(data, folds, predictors = NULL) {
  fold_list <- if (inherits(folds, "borg_cv")) folds$folds else folds

  if (is.null(predictors)) {
    predictors <- names(data)[vapply(data, is.numeric, logical(1))]
  }

  results <- lapply(seq_along(fold_list), function(i) {
    fold <- fold_list[[i]]
    train_data <- data[fold$train, predictors, drop = FALSE]
    test_data <- data[fold$test, predictors, drop = FALSE]

    # MESS: for each predictor, compute similarity as % of training range
    mess_vals <- vapply(predictors, function(var) {
      train_vals <- train_data[[var]]
      test_vals <- test_data[[var]]
      tr_min <- min(train_vals, na.rm = TRUE)
      tr_max <- max(train_vals, na.rm = TRUE)
      tr_range <- tr_max - tr_min
      if (tr_range == 0) return(100)

      # MESS formula: minimum similarity across variables
      f <- stats::ecdf(train_vals)
      fi <- f(test_vals) * 100

      pmin(fi, 100 - fi)  # similarity percentage
    }, numeric(length(fold$test)))

    if (is.matrix(mess_vals)) {
      min_mess <- apply(mess_vals, 1, min)  # per-observation minimum
    } else {
      min_mess <- mess_vals
    }

    data.frame(
      fold = i,
      mean_mess = mean(min_mess, na.rm = TRUE),
      min_mess = min(min_mess, na.rm = TRUE),
      pct_novel = mean(min_mess < 0, na.rm = TRUE) * 100,
      n_test = length(fold$test),
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, results)
  class(result) <- c("borg_fold_similarity", "data.frame")
  result
}


#' Global (Pooled) Cross-Validation Metrics
#'
#' Pools all held-out predictions across folds and computes a single
#' performance metric, avoiding bias from unequal fold sizes.
#'
#' @param data Data frame.
#' @param folds A \code{borg_cv} object or fold list.
#' @param formula Model formula.
#' @param metric Character. \code{"rmse"}, \code{"mae"}, or \code{"rsq"}.
#' @param fit_fun Function. Default: \code{lm}.
#'
#' @return A list with class \code{"borg_global_validation"} containing
#'   the pooled metric and per-fold metrics for comparison.
#'
#' @export
borg_global_validation <- function(data, folds, formula,
                                     metric = c("rmse", "mae", "rsq"),
                                     fit_fun = stats::lm) {
  metric <- match.arg(metric)
  fold_list <- if (inherits(folds, "borg_cv")) folds$folds else folds
  target_var <- all.vars(formula)[1]

  all_actual <- numeric(0)
  all_predicted <- numeric(0)
  per_fold <- numeric(length(fold_list))

  for (i in seq_along(fold_list)) {
    fold <- fold_list[[i]]
    m <- tryCatch(fit_fun(formula, data = data[fold$train, , drop = FALSE]),
                   error = function(e) NULL)
    if (is.null(m)) { per_fold[i] <- NA_real_; next }

    p <- stats::predict(m, newdata = data[fold$test, , drop = FALSE])
    a <- data[[target_var]][fold$test]

    all_actual <- c(all_actual, a)
    all_predicted <- c(all_predicted, p)

    per_fold[i] <- switch(metric,
      "rmse" = sqrt(mean((a - p)^2, na.rm = TRUE)),
      "mae" = mean(abs(a - p), na.rm = TRUE),
      "rsq" = 1 - sum((a - p)^2) / sum((a - mean(a))^2)
    )
  }

  pooled <- switch(metric,
    "rmse" = sqrt(mean((all_actual - all_predicted)^2, na.rm = TRUE)),
    "mae" = mean(abs(all_actual - all_predicted), na.rm = TRUE),
    "rsq" = 1 - sum((all_actual - all_predicted)^2) /
                sum((all_actual - mean(all_actual))^2)
  )

  result <- list(
    pooled = pooled,
    per_fold_mean = mean(per_fold, na.rm = TRUE),
    per_fold = per_fold,
    metric = metric,
    n_pooled = length(all_actual)
  )
  class(result) <- c("borg_global_validation", "list")
  result
}

#' @export
print.borg_global_validation <- function(x, ...) {
  cat("BORG Global (Pooled) Validation\n")
  cat(sprintf("  Pooled %s:    %.4f (%d observations)\n",
              toupper(x$metric), x$pooled, x$n_pooled))
  cat(sprintf("  Per-fold mean: %.4f\n", x$per_fold_mean))
  cat(sprintf("  Difference:    %.4f\n", abs(x$pooled - x$per_fold_mean)))
  invisible(x)
}


#' Local Moran's I for Residuals
#'
#' Computes local Moran's I statistics to identify spatial clusters of
#' high/low residuals (hotspots and coldspots).
#'
#' @param residuals Numeric vector of model residuals.
#' @param x Numeric. X-coordinates.
#' @param y Numeric. Y-coordinates.
#' @param k Integer. Number of nearest neighbors for spatial weights.
#'   Default: 8.
#'
#' @return A data frame with columns: x, y, local_i, p_value, cluster_type.
#'
#' @export
borg_local_moran <- function(residuals, x, y, k = 8L) {
  n <- length(residuals)
  z <- residuals - mean(residuals, na.rm = TRUE)
  s2 <- mean(z^2, na.rm = TRUE)

  # Build k-NN spatial weights
  dist_mat <- compute_distance_matrix(x, y)
  diag(dist_mat) <- Inf

  local_i <- numeric(n)
  p_vals <- numeric(n)

  for (i in seq_len(n)) {
    # k nearest neighbors
    nn_idx <- order(dist_mat[i, ])[seq_len(min(k, n - 1))]
    wij <- rep(1 / k, length(nn_idx))

    # Local Moran's I
    local_i[i] <- (z[i] / s2) * sum(wij * z[nn_idx])

    # Pseudo p-value via permutation (simplified)
    null_i <- vapply(seq_len(99), function(r) {
      z_perm <- sample(z)
      (z_perm[i] / s2) * sum(wij * z_perm[nn_idx])
    }, numeric(1))

    p_vals[i] <- (sum(abs(null_i) >= abs(local_i[i])) + 1) / 100
  }

  # Classify clusters
  cluster_type <- ifelse(
    p_vals > 0.05, "not significant",
    ifelse(z > 0 & local_i > 0, "high-high",
    ifelse(z < 0 & local_i > 0, "low-low",
    ifelse(z > 0 & local_i < 0, "high-low", "low-high")))
  )

  result <- data.frame(
    x = x, y = y,
    local_i = local_i, p_value = p_vals,
    cluster_type = cluster_type,
    stringsAsFactors = FALSE
  )
  class(result) <- c("borg_local_moran", "data.frame")
  result
}


#' Willmott's Index of Agreement
#'
#' Computes Willmott's d (original, refined d1, and modified dr) for
#' spatial model assessment.
#'
#' @param actual Numeric vector of observed values.
#' @param predicted Numeric vector of predicted values.
#'
#' @return A list with \code{d} (original), \code{d1} (refined),
#'   and \code{dr} (modified).
#'
#' @references
#' Willmott, C. J. (1981). On the validation of models.
#' \emph{Physical Geography}, 2(2), 184-194.
#' \doi{10.1080/02723646.1981.10642213}
#'
#' @export
borg_willmott <- function(actual, predicted) {
  n <- length(actual)
  mean_obs <- mean(actual, na.rm = TRUE)

  # Original d
  ss_res <- sum((actual - predicted)^2, na.rm = TRUE)
  ss_pot <- sum((abs(predicted - mean_obs) + abs(actual - mean_obs))^2, na.rm = TRUE)
  d <- if (ss_pot > 0) 1 - ss_res / ss_pot else NA_real_

  # Refined d1 (uses absolute deviations)
  abs_res <- sum(abs(actual - predicted), na.rm = TRUE)
  abs_pot <- 2 * sum(abs(actual - mean_obs), na.rm = TRUE)
  d1 <- if (abs_pot > 0) 1 - abs_res / abs_pot else NA_real_

  # Modified dr
  c_val <- 2
  dr <- if (abs_res <= abs_pot) {
    1 - abs_res / abs_pot
  } else {
    abs_pot / abs_res - 1
  }

  list(d = d, d1 = d1, dr = dr)
}


#' Best Subset Variable Selection with Blocked CV
#'
#' Evaluates all 2^p combinations of predictor variables using blocked
#' CV. Exhaustive search — only feasible for small p (< 15).
#'
#' @param data Data frame.
#' @param target Character. Response variable.
#' @param predictors Character vector. Candidate predictors.
#' @param folds A \code{borg_cv} object or fold list.
#' @param metric Character. Default: \code{"rmse"}.
#' @param fit_fun Function. Default: \code{lm}.
#' @param max_vars Integer. Maximum variables in a subset. Default: all.
#' @param verbose Logical. Default: FALSE.
#'
#' @return A data frame with class \code{"borg_bss"}: variables, n_vars,
#'   metric_value, rank.
#'
#' @export
borg_best_subset <- function(data, target, predictors, folds,
                               metric = c("rmse", "mae", "rsq"),
                               fit_fun = stats::lm,
                               max_vars = NULL, verbose = FALSE) {
  metric <- match.arg(metric)
  minimize <- metric %in% c("rmse", "mae")
  fold_list <- if (inherits(folds, "borg_cv")) folds$folds else folds
  p <- length(predictors)

  if (p > 15) stop("Best subset is limited to <= 15 predictors (2^15 = 32768 combinations)")
  if (is.null(max_vars)) max_vars <- p

  # Generate all subsets
  subsets <- list()
  for (k in seq_len(min(max_vars, p))) {
    combos <- utils::combn(predictors, k, simplify = FALSE)
    subsets <- c(subsets, combos)
  }

  if (verbose) message(sprintf("Evaluating %d subsets...", length(subsets)))

  results <- lapply(seq_along(subsets), function(s) {
    vars <- subsets[[s]]
    formula <- stats::as.formula(paste(target, "~", paste(vars, collapse = " + ")))

    val <- mean(vapply(fold_list, function(fold) {
      m <- tryCatch(fit_fun(formula, data = data[fold$train, ]),
                     error = function(e) NULL)
      if (is.null(m)) return(NA_real_)
      p <- stats::predict(m, newdata = data[fold$test, ])
      a <- data[[target]][fold$test]
      switch(metric,
        "rmse" = sqrt(mean((a - p)^2, na.rm = TRUE)),
        "mae" = mean(abs(a - p), na.rm = TRUE),
        "rsq" = 1 - sum((a - p)^2) / sum((a - mean(a))^2)
      )
    }, numeric(1)), na.rm = TRUE)

    data.frame(
      variables = paste(vars, collapse = " + "),
      n_vars = length(vars),
      metric_value = val,
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, results)
  result <- result[order(if (minimize) result$metric_value else -result$metric_value), ]
  result$rank <- seq_len(nrow(result))
  rownames(result) <- NULL

  attr(result, "metric") <- metric
  attr(result, "best") <- result$variables[1]
  class(result) <- c("borg_bss", "data.frame")
  result
}

#' @export
print.borg_bss <- function(x, ...) {
  cat("BORG Best Subset Selection\n")
  cat(sprintf("  Best: %s\n", attr(x, "best")))
  cat(sprintf("  %s = %.4f\n", toupper(attr(x, "metric")), x$metric_value[1]))
  cat(sprintf("  %d subsets evaluated\n", nrow(x)))
  invisible(x)
}
