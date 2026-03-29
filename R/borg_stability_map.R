# ===========================================================================
# borg_stability_map() — Spatial prediction stability map
# ===========================================================================

#' Prediction Stability Map
#'
#' Creates a continuous spatial map of prediction variance across
#' cross-validation folds. Unlike \code{\link{borg_stability}()} which
#' returns summary statistics, this function produces per-location
#' stability scores showing where predictions are consistent versus
#' volatile across different train/test partitions.
#'
#' @param model A fitted model with a \code{predict()} method, or a
#'   model-fitting function.
#' @param data Data frame with predictors, target, and coordinates.
#' @param new Data frame of prediction locations. If \code{NULL}, uses
#'   \code{data}.
#' @param target Character. Target variable name.
#' @param coords Character vector of length 2. Coordinate column names.
#' @param formula Model formula. Required if \code{model} is a function.
#' @param fit_fun Function. Model fitting function. If \code{NULL},
#'   attempts to refit from model's call.
#' @param folds A \code{borg_cv} or \code{borg_disc_cv} object, or a
#'   fold list. If \code{NULL}, generates spatial CV folds automatically.
#' @param v Integer. Number of folds if generating automatically.
#'   Default: 10.
#' @param seed Integer. Random seed. Default: 42.
#'
#' @return A data frame with class \code{"borg_stability_map"} containing:
#'   \describe{
#'     \item{pred_mean}{Mean prediction across folds}
#'     \item{pred_sd}{Standard deviation of predictions across folds}
#'     \item{pred_cv}{Coefficient of variation (|sd/mean|)}
#'     \item{pred_range}{Range of predictions (max - min)}
#'     \item{n_folds_predicted}{Number of folds where this point was
#'       in the test set (or could be predicted)}
#'     \item{x, y}{Coordinates}
#'   }
#'   Has \code{print()} and \code{autoplot()} methods.
#'
#' @details
#' \subsection{How it works}{
#' For each CV fold:
#' \enumerate{
#'   \item Refit the model on the training set.
#'   \item Predict on all locations (not just test set).
#'   \item Store per-location predictions.
#' }
#' Then compute per-location statistics across folds. Locations with
#' high \code{pred_sd} are unstable — the prediction changes substantially
#' depending on which data is included in training.
#' }
#'
#' \subsection{Interpretation}{
#' High instability at a location can indicate:
#' \itemize{
#'   \item The location is in a data-sparse region (near AOA boundary)
#'   \item The model is overfitting to nearby training points
#'   \item The underlying relationship changes spatially (non-stationarity)
#' }
#' }
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(100, 0, 50), y = runif(100, 0, 50))
#' d$z <- sin(d$x / 5) + rnorm(100, sd = 0.5)
#' model <- lm(z ~ x + y, data = d)
#' sm <- borg_stability_map(model, d, target = "z", coords = c("x", "y"),
#'                            v = 5)
#' sm
#'
#' @export
borg_stability_map <- function(model, data, new = NULL, target, coords,
                                 formula = NULL, fit_fun = NULL,
                                 folds = NULL, v = 10, seed = 42) {

  set.seed(seed)

  if (length(coords) != 2 || !all(coords %in% names(data))) {
    stop("coords must be a character vector of length 2 naming columns in data")
  }
  if (!target %in% names(data)) {
    stop(sprintf("target '%s' not found in data", target))
  }

  pred_data <- if (!is.null(new)) new else data
  n_pred <- nrow(pred_data)

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

  # Generate folds if needed
  if (is.null(folds)) {
    folds <- borg_cv(data, coords = coords, target = target, v = v)
  }
  fold_list <- if (inherits(folds, "borg_cv")) folds$folds else folds

  # Collect predictions from each fold
  pred_matrix <- matrix(NA_real_, nrow = n_pred, ncol = length(fold_list))

  for (f_idx in seq_along(fold_list)) {
    fold <- fold_list[[f_idx]]
    train_data <- data[fold$train, , drop = FALSE]

    refit <- tryCatch(
      fit_fun(formula, data = train_data),
      error = function(e) NULL
    )
    if (is.null(refit)) next

    preds <- tryCatch(
      stats::predict(refit, newdata = pred_data),
      error = function(e) rep(NA_real_, n_pred)
    )
    pred_matrix[, f_idx] <- preds
  }

  # Compute per-location statistics
  pred_mean <- apply(pred_matrix, 1, mean, na.rm = TRUE)
  pred_sd <- apply(pred_matrix, 1, stats::sd, na.rm = TRUE)
  pred_range <- apply(pred_matrix, 1, function(x) {
    x <- x[!is.na(x)]
    if (length(x) < 2) NA_real_ else max(x) - min(x)
  })
  pred_cv <- ifelse(abs(pred_mean) > 1e-10, abs(pred_sd / pred_mean), NA_real_)
  n_folds_predicted <- apply(pred_matrix, 1, function(x) sum(!is.na(x)))

  result <- data.frame(
    pred_mean = pred_mean,
    pred_sd = pred_sd,
    pred_cv = pred_cv,
    pred_range = pred_range,
    n_folds_predicted = n_folds_predicted,
    stringsAsFactors = FALSE
  )

  # Add coordinates
  if (all(coords %in% names(pred_data))) {
    result$x <- pred_data[[coords[1]]]
    result$y <- pred_data[[coords[2]]]
  }

  attr(result, "coord_names") <- coords
  attr(result, "n_folds") <- length(fold_list)
  attr(result, "pred_matrix") <- pred_matrix
  class(result) <- c("borg_stability_map", "data.frame")

  result
}


#' @export
print.borg_stability_map <- function(x, ...) {
  n_folds <- attr(x, "n_folds") %||% NA
  cat("BORG Prediction Stability Map\n")
  cat("=============================\n\n")
  cat(sprintf("  Locations: %d | Folds: %d\n", nrow(x), n_folds))
  cat(sprintf("  Prediction SD:    mean = %.4f | median = %.4f | max = %.4f\n",
              mean(x$pred_sd, na.rm = TRUE),
              stats::median(x$pred_sd, na.rm = TRUE),
              max(x$pred_sd, na.rm = TRUE)))
  cat(sprintf("  Prediction range: mean = %.4f | max = %.4f\n",
              mean(x$pred_range, na.rm = TRUE),
              max(x$pred_range, na.rm = TRUE)))

  # Identify most unstable locations
  top_unstable <- head(order(-x$pred_sd), 5)
  if (length(top_unstable) > 0 && "x" %in% names(x)) {
    cat("\n  Most unstable locations:\n")
    for (i in top_unstable) {
      cat(sprintf("    [%.1f, %.1f] SD = %.4f\n",
                  x$x[i], x$y[i], x$pred_sd[i]))
    }
  }
  invisible(x)
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_stability_map <- function(object,
                                          metric = c("pred_sd", "pred_cv", "pred_range"),
                                          ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }

  metric <- match.arg(metric)
  has_coords <- "x" %in% names(object)
  coord_names <- attr(object, "coord_names") %||% c("x", "y")
  n_folds <- attr(object, "n_folds") %||% NA

  metric_labels <- c(
    pred_sd = "Prediction SD",
    pred_cv = "Coefficient of Variation",
    pred_range = "Prediction Range"
  )

  if (has_coords) {
    ggplot2::ggplot(object, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_point(
        ggplot2::aes(color = .data[[metric]]),
        size = 1.5, alpha = 0.8
      ) +
      ggplot2::scale_color_viridis_c(option = "inferno") +
      ggplot2::coord_equal() +
      ggplot2::labs(
        title = "Prediction Stability Map",
        subtitle = sprintf("%s across %d folds", metric_labels[metric], n_folds),
        x = coord_names[1], y = coord_names[2],
        color = metric_labels[metric]
      ) +
      borg_theme()
  } else {
    ggplot2::ggplot(object, ggplot2::aes(x = .data[[metric]])) +
      ggplot2::geom_histogram(bins = 30, fill = "#2C3E50", color = "white",
                               alpha = 0.7, linewidth = 0.2) +
      ggplot2::labs(
        title = "Prediction Stability Distribution",
        subtitle = sprintf("%s across %d folds", metric_labels[metric], n_folds),
        x = metric_labels[metric], y = "Count"
      ) +
      borg_theme()
  }
}
