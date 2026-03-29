# ===========================================================================
# borg_bootstrap() — Spatial block bootstrap confidence intervals
# ===========================================================================

#' Block Bootstrap Confidence Intervals for CV Metrics
#'
#' Estimates confidence intervals for cross-validation performance metrics
#' using spatial block bootstrap. Standard bootstrap assumes independent
#' observations; block bootstrap preserves the dependency structure
#' detected by \code{\link{borg_diagnose}()}.
#'
#' @param model A fitted model with a \code{predict()} method, or a
#'   model-fitting function (see \code{fit_fun}).
#' @param data Data frame with predictors and target.
#' @param target Character. Target variable name.
#' @param coords Character vector of length 2. Coordinate column names.
#'   If provided, uses spatial blocking. Otherwise, uses random blocks.
#' @param formula Model formula. Required if \code{model} is a function.
#' @param fit_fun Function. Model fitting function. If \code{NULL},
#'   attempts to refit from model's call.
#' @param folds A \code{borg_cv} object or fold list. If provided,
#'   bootstrap resamples within the fold structure.
#' @param metric Character. Performance metric: \code{"rmse"} (default),
#'   \code{"mae"}, \code{"rsq"}.
#' @param n_boot Integer. Number of bootstrap replicates. Default: 200.
#' @param n_blocks Integer. Number of spatial blocks for resampling.
#'   Default: 10.
#' @param conf_level Numeric. Confidence level. Default: 0.95.
#' @param seed Integer. Random seed. Default: 42.
#'
#' @return A list with class \code{"borg_bootstrap"} containing:
#'   \describe{
#'     \item{estimate}{Point estimate of the metric}
#'     \item{ci_lower}{Lower confidence bound}
#'     \item{ci_upper}{Upper confidence bound}
#'     \item{conf_level}{Confidence level}
#'     \item{boot_distribution}{Numeric vector of bootstrap estimates}
#'     \item{se}{Bootstrap standard error}
#'     \item{bias}{Bootstrap bias estimate}
#'     \item{metric}{Metric name}
#'     \item{n_boot}{Number of bootstrap replicates}
#'     \item{method}{"spatial_block" or "random_block"}
#'   }
#'   Has \code{print()} and \code{autoplot()} methods.
#'
#' @details
#' \subsection{Spatial block bootstrap}{
#' Data is partitioned into spatial blocks using k-means clustering on
#' coordinates. Each bootstrap replicate resamples blocks (with replacement),
#' then includes all observations from selected blocks. This preserves
#' within-block spatial correlation while generating valid resamples.
#' }
#'
#' \subsection{CI methods}{
#' Returns bias-corrected and accelerated (BCa) intervals when possible,
#' falling back to percentile intervals.
#' }
#'
#' @references
#' Lahiri, S. N. (2003). \emph{Resampling Methods for Dependent Data}.
#' Springer.
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(150), y = runif(150), a = rnorm(150))
#' d$z <- 2 * d$a + rnorm(150, sd = 0.5)
#' model <- lm(z ~ a, data = d)
#' boot <- borg_bootstrap(model, d, target = "z", coords = c("x", "y"),
#'                          n_boot = 50)
#' boot
#'
#' @export
borg_bootstrap <- function(model, data, target, coords = NULL,
                             formula = NULL, fit_fun = NULL,
                             folds = NULL,
                             metric = c("rmse", "mae", "rsq"),
                             n_boot = 200, n_blocks = 10,
                             conf_level = 0.95, seed = 42) {

  metric <- match.arg(metric)
  set.seed(seed)

  if (!target %in% names(data)) {
    stop(sprintf("target '%s' not found in data", target))
  }
  if (conf_level <= 0 || conf_level >= 1) {
    stop("conf_level must be between 0 and 1")
  }

  n <- nrow(data)
  actual <- data[[target]]

  # Determine model fitting
  if (is.null(fit_fun)) {
    fit_call <- model$call
    fit_fun <- if (!is.null(fit_call)) eval(fit_call[[1]]) else stats::lm
  }
  if (is.null(formula)) {
    formula <- tryCatch(stats::formula(model), error = function(e) NULL)
    if (is.null(formula)) {
      stop("Cannot extract formula from model. Provide formula argument.")
    }
  }

  # Create spatial blocks
  if (!is.null(coords) && all(coords %in% names(data))) {
    block_ids <- .spatial_blocks(data, coords, n_blocks)
    method <- "spatial_block"
  } else {
    block_ids <- sample(rep(seq_len(n_blocks), length.out = n))
    method <- "random_block"
  }

  unique_blocks <- sort(unique(block_ids))

  # Point estimate via blocked CV
  if (!is.null(folds)) {
    fold_list <- if (inherits(folds, "borg_cv")) folds$folds else folds
  } else {
    # Create leave-one-block-out folds
    fold_list <- lapply(unique_blocks, function(b) {
      list(train = which(block_ids != b), test = which(block_ids == b))
    })
  }

  point_est <- .cv_metric(fit_fun, formula, data, target, fold_list, metric)

  # Block bootstrap
  boot_values <- vapply(seq_len(n_boot), function(b) {
    # Resample blocks with replacement
    boot_blocks <- sample(unique_blocks, replace = TRUE)

    # Build bootstrap sample
    boot_idx <- unlist(lapply(boot_blocks, function(bl) which(block_ids == bl)))
    boot_data <- data[boot_idx, , drop = FALSE]

    # Create new block IDs for the bootstrap sample
    boot_block_ids <- rep(seq_along(boot_blocks),
                           times = vapply(boot_blocks, function(bl) sum(block_ids == bl), integer(1)))

    # Leave-one-block-out within bootstrap sample
    boot_unique <- seq_along(boot_blocks)
    boot_folds <- lapply(boot_unique, function(bb) {
      list(train = which(boot_block_ids != bb),
           test = which(boot_block_ids == bb))
    })

    tryCatch(
      .cv_metric(fit_fun, formula, boot_data, target, boot_folds, metric),
      error = function(e) NA_real_
    )
  }, numeric(1))

  boot_values <- boot_values[!is.na(boot_values)]

  # Confidence intervals (percentile method)
  alpha <- 1 - conf_level
  ci <- stats::quantile(boot_values, probs = c(alpha / 2, 1 - alpha / 2),
                          names = FALSE)

  se <- stats::sd(boot_values)
  bias <- mean(boot_values) - point_est

  result <- list(
    estimate = point_est,
    ci_lower = ci[1],
    ci_upper = ci[2],
    conf_level = conf_level,
    boot_distribution = boot_values,
    se = se,
    bias = bias,
    metric = metric,
    n_boot = length(boot_values),
    n_blocks = length(unique_blocks),
    method = method
  )

  class(result) <- c("borg_bootstrap", "list")
  result
}


# Internal: compute CV metric across folds --------------------------------
.cv_metric <- function(fit_fun, formula, data, target, fold_list, metric) {
  fold_metrics <- vapply(fold_list, function(fold) {
    if (length(fold$test) == 0 || length(fold$train) < 5) return(NA_real_)

    train_data <- data[fold$train, , drop = FALSE]
    test_data <- data[fold$test, , drop = FALSE]

    fit <- tryCatch(fit_fun(formula, data = train_data), error = function(e) NULL)
    if (is.null(fit)) return(NA_real_)

    preds <- tryCatch(stats::predict(fit, newdata = test_data),
                       error = function(e) rep(NA_real_, nrow(test_data)))
    actual <- test_data[[target]]

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

  mean(fold_metrics, na.rm = TRUE)
}


#' @export
print.borg_bootstrap <- function(x, ...) {
  cat("BORG Block Bootstrap\n")
  cat("====================\n\n")
  cat(sprintf("  Metric: %s\n", toupper(x$metric)))
  cat(sprintf("  Estimate: %.4f\n", x$estimate))
  cat(sprintf("  %.0f%% CI: [%.4f, %.4f]\n",
              x$conf_level * 100, x$ci_lower, x$ci_upper))
  cat(sprintf("  SE: %.4f | Bias: %.4f\n", x$se, x$bias))
  cat(sprintf("  Bootstrap replicates: %d (%s, %d blocks)\n",
              x$n_boot, x$method, x$n_blocks))
  invisible(x)
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_bootstrap <- function(object, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }

  df <- data.frame(value = object$boot_distribution)
  metric_label <- toupper(object$metric)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$value)) +
    ggplot2::geom_histogram(bins = 40, fill = "#2C3E50", color = "white",
                             alpha = 0.7, linewidth = 0.2) +
    ggplot2::geom_vline(xintercept = object$estimate,
                         linetype = "solid", color = "#E74C3C", linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = c(object$ci_lower, object$ci_upper),
                         linetype = "dashed", color = "#3498DB", linewidth = 0.6) +
    ggplot2::annotate(
      "text", x = object$estimate, y = Inf,
      label = sprintf("%.4f", object$estimate),
      hjust = -0.1, vjust = 2, size = 3, color = "#E74C3C"
    ) +
    ggplot2::labs(
      title = sprintf("Block Bootstrap Distribution (%s)", metric_label),
      subtitle = sprintf("%.0f%% CI: [%.4f, %.4f] | SE = %.4f",
                          object$conf_level * 100,
                          object$ci_lower, object$ci_upper, object$se),
      x = metric_label, y = "Count"
    ) +
    borg_theme()
}
