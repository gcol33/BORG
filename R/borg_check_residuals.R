# ===========================================================================
# borg_check_residuals() — Post-fit residual autocorrelation check
# ===========================================================================

#' Check Residual Spatial Autocorrelation
#'
#' After fitting a model, checks whether residuals still exhibit spatial
#' autocorrelation. If they do, the model has not fully captured the
#' spatial process and predictions may be biased.
#'
#' @param model A fitted model with a \code{residuals()} method, or a
#'   numeric vector of residuals.
#' @param data Data frame with coordinate columns (needed if \code{model}
#'   is a numeric vector).
#' @param coords Character vector of length 2. Coordinate column names.
#' @param alpha Numeric. Significance level for Moran's I test. Default: 0.05.
#'
#' @return A list with class \code{"borg_residual_check"} containing:
#'   \describe{
#'     \item{morans_i}{Moran's I of residuals}
#'     \item{p_value}{P-value from Moran's I test}
#'     \item{significant}{Logical. Whether residual autocorrelation is significant}
#'     \item{variogram}{Residual variogram data frame (if computed)}
#'     \item{assessment}{Character. "clean", "mild", or "strong"}
#'   }
#'   Has an \code{autoplot()} method showing the residual variogram.
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(x = runif(100, 0, 100), y = runif(100, 0, 100))
#' d$z <- sin(d$x / 10) + rnorm(100, sd = 0.5)
#' model <- lm(z ~ x + y, data = d)
#' check <- borg_check_residuals(model, d, coords = c("x", "y"))
#' check
#'
#' @export
borg_check_residuals <- function(model, data = NULL, coords = NULL,
                                    alpha = 0.05) {

  # Extract residuals
  if (is.numeric(model)) {
    resids <- model
    if (is.null(data) || is.null(coords)) {
      stop("data and coords required when model is a numeric residual vector")
    }
  } else {
    resids <- tryCatch(stats::residuals(model), error = function(e) {
      stop("Cannot extract residuals from model. Pass a numeric vector instead.")
    })
    if (is.null(data)) {
      data <- tryCatch(stats::model.frame(model), error = function(e) NULL)
    }
  }

  if (is.null(data) || is.null(coords)) {
    stop("data and coords are required")
  }

  coord_info <- extract_coords(data, coords)
  x <- coord_info$x
  y <- coord_info$y

  # Subset if needed
  n <- min(length(resids), length(x))
  if (n > 2000) {
    idx <- sample(n, 2000)
    resids_sub <- resids[idx]
    x_sub <- x[idx]
    y_sub <- y[idx]
  } else {
    resids_sub <- resids[seq_len(n)]
    x_sub <- x[seq_len(n)]
    y_sub <- y[seq_len(n)]
  }

  # Compute distance matrix and Moran's I
  dist_mat <- compute_distance_matrix(x_sub, y_sub)
  morans <- compute_morans_i(resids_sub, dist_mat)

  # Compute residual variogram
  variogram <- compute_empirical_variogram(resids_sub, dist_mat)

  significant <- morans$p < alpha && morans$I > 0.05
  assessment <- if (!significant) "clean"
                else if (morans$I < 0.3) "mild"
                else "strong"

  result <- list(
    morans_i = morans$I,
    p_value = morans$p,
    significant = significant,
    variogram = variogram,
    sill = if (!is.null(variogram)) max(variogram$semivariance) else NA_real_,
    assessment = assessment,
    n = n
  )
  class(result) <- c("borg_residual_check", "list")
  result
}


#' @export
print.borg_residual_check <- function(x, ...) {
  cat("BORG Residual Autocorrelation Check\n")
  cat(sprintf("  Moran's I: %.4f (p = %.4g)\n", x$morans_i, x$p_value))
  cat(sprintf("  Assessment: %s\n", toupper(x$assessment)))
  if (x$significant) {
    cat("  WARNING: Residuals have significant spatial autocorrelation.\n")
    cat("  The model has not fully captured the spatial process.\n")
  } else {
    cat("  Residuals show no significant spatial structure.\n")
  }
  invisible(x)
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_residual_check <- function(object, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }

  vario <- object$variogram
  if (is.null(vario) || nrow(vario) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::labs(title = "No variogram data"))
  }

  sill <- object$sill

  p <- ggplot2::ggplot(vario, ggplot2::aes(x = .data$distance, y = .data$semivariance)) +
    ggplot2::geom_line(color = "#2C3E50", alpha = 0.3, linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(size = .data$n_pairs),
                         color = "#2C3E50", alpha = 0.8) +
    ggplot2::scale_size_continuous(range = c(1.5, 5), guide = "none")

  if (!is.na(sill)) {
    p <- p + ggplot2::geom_hline(yintercept = sill, linetype = "dashed",
                                   color = "gray50", linewidth = 0.5)
  }

  assessment_color <- switch(object$assessment,
    "clean" = "#27AE60", "mild" = "#F39C12", "strong" = "#C0392B"
  )

  p +
    ggplot2::labs(
      title = "Residual Variogram",
      subtitle = sprintf("Moran's I = %.3f (p = %.3g) | %s",
                          object$morans_i, object$p_value,
                          toupper(object$assessment)),
      x = "Distance", y = "Semivariance"
    ) +
    borg_theme() +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(color = assessment_color)
    )
}
