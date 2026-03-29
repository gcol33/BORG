# ===========================================================================
# borg_error_profile() — DI-to-error calibration
# ===========================================================================

#' Calibrate Dissimilarity Index to Prediction Error
#'
#' Models the relationship between the Dissimilarity Index (DI) and
#' prediction error using isotonic regression. Enables pixel-level
#' uncertainty estimation: for any new prediction point, its DI can
#' be mapped to an expected error.
#'
#' @param data Data frame of training data.
#' @param folds A \code{borg_cv} object or fold list.
#' @param formula Model formula.
#' @param predictors Character vector. Predictor columns for DI computation.
#'   If \code{NULL}, auto-detected.
#' @param metric Character. \code{"rmse"} or \code{"mae"}.
#' @param fit_fun Function. Default: \code{lm}.
#' @param n_bins Integer. Number of DI bins. Default: 10.
#'
#' @return A list with class \code{"borg_error_profile"} containing:
#'   \describe{
#'     \item{profile}{Data frame: di_bin, mean_di, mean_error, n_obs}
#'     \item{raw}{Data frame: di, error (per observation)}
#'     \item{iso_fit}{Isotonic regression fit for predicting error from DI}
#'   }
#'   Has an \code{autoplot()} method and a \code{predict()} method.
#'
#' @references
#' Meyer, H., & Pebesma, E. (2021). Predicting into unknown space?
#' Estimating the area of applicability of spatial prediction models.
#' \emph{Methods in Ecology and Evolution}, 12(9), 1620-1633.
#' \doi{10.1111/2041-210X.13650}
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(150, 0, 100), y = runif(150, 0, 100),
#'                  a = rnorm(150), b = rnorm(150))
#' d$z <- d$a * 2 + rnorm(150, sd = 0.5)
#' cv <- borg_cv(d, coords = c("x", "y"), target = "z")
#' ep <- borg_error_profile(d, cv, z ~ a + b, predictors = c("a", "b"))
#' ep
#'
#' @export
borg_error_profile <- function(data, folds, formula, predictors = NULL,
                                  metric = c("rmse", "mae"),
                                  fit_fun = stats::lm, n_bins = 10) {
  metric <- match.arg(metric)
  fold_list <- if (inherits(folds, "borg_cv")) folds$folds else folds
  target_var <- all.vars(formula)[1]

  if (is.null(predictors)) {
    rhs_vars <- all.vars(formula)[-1]
    predictors <- rhs_vars[vapply(data[rhs_vars], is.numeric, logical(1))]
  }

  # Collect per-observation DI and error from CV
  obs_di <- numeric(0)
  obs_error <- numeric(0)

  for (fold in fold_list) {
    train_data <- data[fold$train, , drop = FALSE]
    test_data <- data[fold$test, , drop = FALSE]

    # Compute DI for test observations
    di_vals <- as.numeric(borg_di(train_data, test_data, predictors = predictors))

    # Compute prediction error
    model <- tryCatch(fit_fun(formula, data = train_data), error = function(e) NULL)
    if (is.null(model)) next
    preds <- tryCatch(stats::predict(model, newdata = test_data),
                       error = function(e) rep(NA_real_, nrow(test_data)))
    actual <- test_data[[target_var]]
    errors <- abs(actual - preds)

    obs_di <- c(obs_di, di_vals)
    obs_error <- c(obs_error, errors)
  }

  raw <- data.frame(di = obs_di, error = obs_error, stringsAsFactors = FALSE)
  raw <- raw[complete.cases(raw), ]

  # Bin by DI
  if (nrow(raw) < n_bins * 3) n_bins <- max(3, nrow(raw) %/% 5)
  raw$bin <- cut(raw$di, breaks = n_bins, include.lowest = TRUE, labels = FALSE)

  profile <- stats::aggregate(
    cbind(mean_di = di, mean_error = error) ~ bin, data = raw, FUN = mean
  )
  profile$n_obs <- as.integer(table(raw$bin))

  # Isotonic regression: error should be non-decreasing with DI
  iso <- stats::isoreg(raw$di, raw$error)

  result <- list(
    profile = profile,
    raw = raw[, c("di", "error")],
    iso_fit = iso,
    metric = metric
  )
  class(result) <- c("borg_error_profile", "list")
  result
}


#' @export
predict.borg_error_profile <- function(object, newdata, ...) {
  # newdata should be a numeric vector of DI values
  di_vals <- if (is.data.frame(newdata)) newdata$di else as.numeric(newdata)
  stats::approx(object$iso_fit$x, object$iso_fit$yf,
                 xout = di_vals, rule = 2)$y
}


#' @export
print.borg_error_profile <- function(x, ...) {
  cat("BORG Error Profile (DI-to-Error Calibration)\n")
  cat(sprintf("  %d observations, %d bins\n", nrow(x$raw), nrow(x$profile)))
  cat(sprintf("  DI range: [%.3f, %.3f]\n",
              min(x$raw$di), max(x$raw$di)))
  cat(sprintf("  Error range: [%.3f, %.3f]\n",
              min(x$raw$error), max(x$raw$error)))
  invisible(x)
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_error_profile <- function(object, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")

  raw <- object$raw
  iso <- object$iso_fit

  # Isotonic fit line
  iso_df <- data.frame(
    di = sort(iso$x),
    error = iso$yf[order(iso$x)]
  )

  ggplot2::ggplot(raw, ggplot2::aes(x = .data$di, y = .data$error)) +
    ggplot2::geom_point(alpha = 0.15, size = 0.8, color = "gray40") +
    ggplot2::geom_line(data = iso_df,
                        ggplot2::aes(x = .data$di, y = .data$error),
                        color = "#C0392B", linewidth = 1.2) +
    ggplot2::labs(
      title = "DI-to-Error Calibration",
      subtitle = "Red line = isotonic regression (use for pixel-level uncertainty)",
      x = "Dissimilarity Index", y = "Absolute Error"
    ) +
    borg_theme()
}
