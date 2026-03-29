# ===========================================================================
# borg_multiscale() — Multi-scale spatial performance assessment
# ===========================================================================

#' Multi-Scale Performance Assessment
#'
#' @description
#' Evaluates model accuracy at increasing spatial aggregation scales.
#' Predictions and observations are averaged within grid cells of
#' increasing size, and performance is computed at each scale.
#' Reveals scale-dependent error patterns.
#'
#' @param data Data frame with coordinates and target variable.
#' @param predictions Numeric vector of predicted values (same length as
#'   \code{nrow(data)}).
#' @param target Character. Target variable name.
#' @param coords Character vector of length 2. Coordinate columns.
#' @param scales Numeric vector. Grid cell sizes to evaluate.
#'   If \code{NULL}, auto-generated from 5 percent to 50 percent of extent in 8 steps.
#' @param metric Character. \code{"rmse"} (default), \code{"mae"}, or
#'   \code{"rsq"}.
#'
#' @return A data frame with class \code{"borg_multiscale"} containing:
#'   \describe{
#'     \item{scale}{Grid cell size}
#'     \item{metric_value}{Performance at this scale}
#'     \item{n_cells}{Number of occupied grid cells}
#'   }
#'   Has an \code{autoplot()} method.
#'
#' @references
#' Riemann, R., Wilson, B. T., Lister, A., & Parks, S. (2010).
#' An effective assessment protocol for continuous geospatial datasets
#' of forest characteristics using USFS Forest Inventory and Analysis
#' (FIA) data. \emph{Remote Sensing of Environment}, 114(10), 2337-2352.
#' \doi{10.1016/j.rse.2010.05.010}
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(200, 0, 100), y = runif(200, 0, 100),
#'                  z = rnorm(200))
#' preds <- d$z + rnorm(200, sd = 0.5)
#' ms <- borg_multiscale(d, preds, target = "z", coords = c("x", "y"))
#' ms
#'
#' @export
borg_multiscale <- function(data, predictions, target, coords,
                              scales = NULL,
                              metric = c("rmse", "mae", "rsq")) {
  metric <- match.arg(metric)

  x <- data[[coords[1]]]
  y <- data[[coords[2]]]
  actual <- data[[target]]
  n <- length(actual)

  # Auto-generate scales
  if (is.null(scales)) {
    x_ext <- diff(range(x, na.rm = TRUE))
    y_ext <- diff(range(y, na.rm = TRUE))
    max_ext <- max(x_ext, y_ext)
    scales <- seq(max_ext * 0.05, max_ext * 0.5, length.out = 8)
  }

  # Point-level baseline
  point_metric <- switch(metric,
    "rmse" = sqrt(mean((actual - predictions)^2, na.rm = TRUE)),
    "mae" = mean(abs(actual - predictions), na.rm = TRUE),
    "rsq" = {
      ss <- sum((actual - predictions)^2, na.rm = TRUE)
      st <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
      if (st == 0) NA_real_ else 1 - ss / st
    }
  )

  results <- lapply(scales, function(s) {
    # Assign points to grid cells
    cell_x <- floor(x / s)
    cell_y <- floor(y / s)
    cell_id <- paste(cell_x, cell_y, sep = "_")

    # Aggregate within cells
    cell_actual <- tapply(actual, cell_id, mean, na.rm = TRUE)
    cell_pred <- tapply(predictions, cell_id, mean, na.rm = TRUE)

    val <- switch(metric,
      "rmse" = sqrt(mean((cell_actual - cell_pred)^2, na.rm = TRUE)),
      "mae" = mean(abs(cell_actual - cell_pred), na.rm = TRUE),
      "rsq" = {
        ss <- sum((cell_actual - cell_pred)^2, na.rm = TRUE)
        st <- sum((cell_actual - mean(cell_actual, na.rm = TRUE))^2, na.rm = TRUE)
        if (st == 0) NA_real_ else 1 - ss / st
      }
    )

    data.frame(scale = s, metric_value = val,
                n_cells = length(unique(cell_id)), stringsAsFactors = FALSE)
  })

  result <- rbind(
    data.frame(scale = 0, metric_value = point_metric, n_cells = n,
                stringsAsFactors = FALSE),
    do.call(rbind, results)
  )

  attr(result, "metric") <- metric
  class(result) <- c("borg_multiscale", "data.frame")
  result
}


#' @export
print.borg_multiscale <- function(x, ...) {
  met <- attr(x, "metric")
  cat("BORG Multi-Scale Assessment\n")
  cat(sprintf("  Metric: %s\n", toupper(met)))
  cat(sprintf("  Scales: %d (%.1f to %.1f)\n",
              nrow(x) - 1, min(x$scale[x$scale > 0]), max(x$scale)))
  cat(sprintf("  Point-level: %.4f\n", x$metric_value[1]))
  cat(sprintf("  Coarsest:    %.4f\n", x$metric_value[nrow(x)]))
  invisible(x)
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_multiscale <- function(object, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")

  met <- toupper(attr(object, "metric") %||% "METRIC")

  ggplot2::ggplot(object, ggplot2::aes(x = .data$scale, y = .data$metric_value)) +
    ggplot2::geom_line(color = "#2C3E50", linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(size = .data$n_cells),
                         color = "#2C3E50", alpha = 0.7) +
    ggplot2::scale_size_continuous(range = c(2, 6), guide = "none") +
    ggplot2::labs(
      title = "Multi-Scale Performance",
      subtitle = sprintf("%s at increasing spatial aggregation", met),
      x = "Grid cell size", y = met
    ) +
    borg_theme()
}
