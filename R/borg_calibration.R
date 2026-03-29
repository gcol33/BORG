# ===========================================================================
# borg_calibration() — Model calibration diagnostics
# ===========================================================================

#' Model Calibration Diagnostics
#'
#' Assesses whether a model's predictions are well-calibrated. For
#' classification, checks if predicted probabilities match observed
#' frequencies. For regression, checks if predicted quantiles have
#' correct coverage. Optionally uses spatial-aware binning to avoid
#' autocorrelation artifacts in calibration curves.
#'
#' @param predicted Numeric vector. Predicted values (probabilities for
#'   classification, point predictions for regression).
#' @param actual Numeric or factor vector. Observed outcomes (0/1 for
#'   classification, continuous for regression).
#' @param type Character. \code{"classification"} (default if actual is
#'   0/1 or factor) or \code{"regression"}.
#' @param n_bins Integer. Number of bins for calibration curve. Default: 10.
#' @param coords Data frame or matrix with coordinate columns. If provided,
#'   uses spatial-aware binning (ensures bins are not spatially clustered).
#' @param strategy Character. Binning strategy: \code{"uniform"} (equal-width,
#'   default), \code{"quantile"} (equal-count), or \code{"spatial"} (spatial
#'   blocks as bins, requires \code{coords}).
#'
#' @return A list with class \code{"borg_calibration"} containing:
#'   \describe{
#'     \item{calibration_curve}{Data frame with columns: bin_midpoint,
#'       observed_freq, predicted_mean, n, ci_lower, ci_upper}
#'     \item{ece}{Expected Calibration Error (weighted mean |observed - predicted|)}
#'     \item{mce}{Maximum Calibration Error (worst bin)}
#'     \item{brier_score}{Brier score (classification) or calibration MSE (regression)}
#'     \item{type}{Model type}
#'     \item{reliability_slope}{Slope of observed ~ predicted regression
#'       (1.0 = perfect calibration)}
#'     \item{reliability_intercept}{Intercept (0.0 = no bias)}
#'     \item{n_bins}{Number of bins used}
#'     \item{assessment}{Character: "well_calibrated", "moderate", or "poorly_calibrated"}
#'   }
#'   Has \code{print()} and \code{autoplot()} methods.
#'
#' @examples
#' # Classification
#' set.seed(42)
#' probs <- runif(500)
#' outcomes <- rbinom(500, 1, probs^0.8)  # slightly miscalibrated
#' cal <- borg_calibration(probs, outcomes)
#' cal
#'
#' # Regression
#' x <- rnorm(200)
#' y <- 2 * x + rnorm(200, sd = 0.5)
#' preds <- 2.1 * x  # slightly biased
#' cal_reg <- borg_calibration(preds, y, type = "regression")
#' cal_reg
#'
#' @export
borg_calibration <- function(predicted, actual, type = NULL,
                              n_bins = 10, coords = NULL,
                              strategy = c("uniform", "quantile", "spatial")) {

  strategy <- match.arg(strategy)

  if (length(predicted) != length(actual)) {
    stop(sprintf("predicted (%d) and actual (%d) must have the same length",
                 length(predicted), length(actual)))
  }

  # Auto-detect type
  if (is.null(type)) {
    if (is.factor(actual) || all(actual %in% c(0, 1))) {
      type <- "classification"
    } else {
      type <- "regression"
    }
  }

  n <- length(predicted)

  if (type == "classification") {
    result <- .calibration_classification(predicted, actual, n_bins,
                                           coords, strategy)
  } else {
    result <- .calibration_regression(predicted, actual, n_bins,
                                       coords, strategy)
  }

  result$type <- type
  result$n <- n
  class(result) <- c("borg_calibration", "list")
  result
}


# Internal: classification calibration -----------------------------------
.calibration_classification <- function(predicted, actual, n_bins,
                                         coords, strategy) {
  actual <- as.numeric(as.character(actual))

  # Bin assignments
  bins <- .make_bins(predicted, n_bins, strategy, coords)
  unique_bins <- sort(unique(bins))

  # Calibration curve
  cal_data <- lapply(unique_bins, function(b) {
    idx <- which(bins == b)
    if (length(idx) < 2) return(NULL)

    obs_freq <- mean(actual[idx])
    pred_mean <- mean(predicted[idx])
    n_bin <- length(idx)

    # Wilson confidence interval for proportion
    z <- stats::qnorm(0.975)
    denom <- 1 + z^2 / n_bin
    center <- (obs_freq + z^2 / (2 * n_bin)) / denom
    margin <- z * sqrt((obs_freq * (1 - obs_freq) + z^2 / (4 * n_bin)) / n_bin) / denom

    data.frame(
      bin_midpoint = pred_mean,
      observed_freq = obs_freq,
      predicted_mean = pred_mean,
      n = n_bin,
      ci_lower = max(0, center - margin),
      ci_upper = min(1, center + margin),
      stringsAsFactors = FALSE
    )
  })

  cal_df <- do.call(rbind, cal_data[!vapply(cal_data, is.null, logical(1))])

  # Metrics
  weights <- cal_df$n / sum(cal_df$n)
  ece <- sum(weights * abs(cal_df$observed_freq - cal_df$predicted_mean))
  mce <- max(abs(cal_df$observed_freq - cal_df$predicted_mean))
  brier <- mean((predicted - actual)^2)

  # Reliability diagram regression
  rel_fit <- stats::lm(observed_freq ~ predicted_mean, data = cal_df,
                         weights = cal_df$n)
  rel_coef <- stats::coef(rel_fit)

  # Assessment
  assessment <- if (ece < 0.02) "well_calibrated"
                else if (ece < 0.08) "moderate"
                else "poorly_calibrated"

  list(
    calibration_curve = cal_df,
    ece = ece,
    mce = mce,
    brier_score = brier,
    reliability_slope = unname(rel_coef[2]),
    reliability_intercept = unname(rel_coef[1]),
    n_bins = nrow(cal_df),
    assessment = assessment
  )
}


# Internal: regression calibration ----------------------------------------
.calibration_regression <- function(predicted, actual, n_bins,
                                     coords, strategy) {
  residuals <- actual - predicted

  # Compute observed coverage at various nominal levels
  nominal_levels <- seq(0.1, 0.9, by = 0.1)
  residual_sd <- stats::sd(residuals)

  coverage_data <- lapply(nominal_levels, function(p) {
    z <- stats::qnorm(0.5 + p / 2)
    in_interval <- abs(residuals) <= z * residual_sd
    data.frame(
      nominal = p,
      observed = mean(in_interval),
      stringsAsFactors = FALSE
    )
  })

  coverage_df <- do.call(rbind, coverage_data)

  # Calibration curve: bin by predicted value, compute mean residual
  bins <- .make_bins(predicted, n_bins, strategy, coords)
  unique_bins <- sort(unique(bins))

  cal_data <- lapply(unique_bins, function(b) {
    idx <- which(bins == b)
    if (length(idx) < 2) return(NULL)
    data.frame(
      bin_midpoint = mean(predicted[idx]),
      observed_freq = mean(actual[idx]),
      predicted_mean = mean(predicted[idx]),
      n = length(idx),
      ci_lower = mean(actual[idx]) - 1.96 * stats::sd(actual[idx]) / sqrt(length(idx)),
      ci_upper = mean(actual[idx]) + 1.96 * stats::sd(actual[idx]) / sqrt(length(idx)),
      stringsAsFactors = FALSE
    )
  })

  cal_df <- do.call(rbind, cal_data[!vapply(cal_data, is.null, logical(1))])

  # Metrics
  ece <- mean(abs(coverage_df$observed - coverage_df$nominal))
  mce <- max(abs(coverage_df$observed - coverage_df$nominal))
  cal_mse <- mean(residuals^2)

  rel_fit <- stats::lm(observed_freq ~ predicted_mean, data = cal_df,
                         weights = cal_df$n)
  rel_coef <- stats::coef(rel_fit)

  assessment <- if (ece < 0.03) "well_calibrated"
                else if (ece < 0.10) "moderate"
                else "poorly_calibrated"

  list(
    calibration_curve = cal_df,
    coverage_curve = coverage_df,
    ece = ece,
    mce = mce,
    brier_score = cal_mse,
    reliability_slope = unname(rel_coef[2]),
    reliability_intercept = unname(rel_coef[1]),
    n_bins = nrow(cal_df),
    assessment = assessment
  )
}


# Internal: binning helper ------------------------------------------------
.make_bins <- function(values, n_bins, strategy, coords) {
  n <- length(values)

  if (strategy == "spatial" && !is.null(coords)) {
    coord_mat <- as.matrix(coords)
    n_blocks <- min(n_bins, nrow(coord_mat) %/% 3)
    if (n_blocks < 2) n_blocks <- 2
    km <- stats::kmeans(coord_mat, centers = n_blocks, nstart = 5)
    return(km$cluster)
  }

  if (strategy == "quantile") {
    breaks <- stats::quantile(values, probs = seq(0, 1, length.out = n_bins + 1))
    breaks <- unique(breaks)
    bins <- findInterval(values, breaks, all.inside = TRUE)
  } else {
    # Uniform bins
    range_val <- range(values, na.rm = TRUE)
    breaks <- seq(range_val[1], range_val[2], length.out = n_bins + 1)
    bins <- findInterval(values, breaks, all.inside = TRUE)
  }
  bins
}


#' @export
print.borg_calibration <- function(x, ...) {
  cat("BORG Calibration Diagnostics\n")
  cat("============================\n\n")
  cat(sprintf("  Type: %s\n", x$type))
  cat(sprintf("  N: %d | Bins: %d\n", x$n, x$n_bins))
  cat(sprintf("  Expected Calibration Error (ECE): %.4f\n", x$ece))
  cat(sprintf("  Maximum Calibration Error (MCE): %.4f\n", x$mce))
  if (x$type == "classification") {
    cat(sprintf("  Brier score: %.4f\n", x$brier_score))
  } else {
    cat(sprintf("  Calibration MSE: %.4f\n", x$brier_score))
  }
  cat(sprintf("  Reliability slope: %.3f (ideal: 1.000)\n", x$reliability_slope))
  cat(sprintf("  Reliability intercept: %.3f (ideal: 0.000)\n", x$reliability_intercept))
  cat(sprintf("  Assessment: %s\n", x$assessment))
  invisible(x)
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_calibration <- function(object, type = c("reliability", "coverage"),
                                       ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }
  type <- match.arg(type)

  if (type == "coverage" && !is.null(object$coverage_curve)) {
    # Regression: coverage plot
    df <- object$coverage_curve
    ggplot2::ggplot(df, ggplot2::aes(x = .data$nominal, y = .data$observed)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                            color = "gray50") +
      ggplot2::geom_point(color = "#2C3E50", size = 3) +
      ggplot2::geom_line(color = "#2C3E50", linewidth = 0.8) +
      ggplot2::labs(
        title = "Calibration: Coverage Plot",
        subtitle = sprintf("ECE = %.4f | %s", object$ece, object$assessment),
        x = "Nominal coverage", y = "Observed coverage"
      ) +
      ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
      borg_theme()
  } else {
    # Reliability diagram
    df <- object$calibration_curve
    ggplot2::ggplot(df, ggplot2::aes(x = .data$predicted_mean,
                                      y = .data$observed_freq)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                            color = "gray50") +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
        fill = "#3498DB", alpha = 0.2
      ) +
      ggplot2::geom_point(ggplot2::aes(size = .data$n),
                           color = "#2C3E50") +
      ggplot2::geom_line(color = "#2C3E50", linewidth = 0.8) +
      ggplot2::scale_size_continuous(range = c(2, 8), guide = "none") +
      ggplot2::labs(
        title = "Reliability Diagram",
        subtitle = sprintf("ECE = %.4f | slope = %.3f | %s",
                            object$ece, object$reliability_slope,
                            object$assessment),
        x = "Mean predicted", y = "Observed frequency"
      ) +
      borg_theme()
  }
}
