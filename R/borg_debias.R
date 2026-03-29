# ===========================================================================
# borg_debias() — Spatial+ debiasing (Dupont et al. 2022)
# ===========================================================================

#' Spatial+ Debiasing for Spatial Confounding
#'
#' Implements the Spatial+ approach of Dupont et al. (2022) to address
#' spatial confounding. Spatially structured predictors share variance
#' with the spatial process, inflating their coefficients. Spatial+
#' removes the spatial component from each predictor by regressing it
#' on smooth spatial coordinates, then uses the residuals as debiased
#' predictors.
#'
#' @param data Data frame with predictors and coordinates.
#' @param predictors Character vector. Predictor column names to debias.
#'   If \code{NULL}, uses all numeric columns except \code{coords} and
#'   \code{target}.
#' @param coords Character vector of length 2. Coordinate column names.
#' @param target Character. Target variable name (excluded from debiasing).
#' @param method Character. Spatial smoothing method:
#'   \code{"gam_approx"} (default) uses polynomial basis (no mgcv dependency),
#'   \code{"tps"} uses thin plate spline approximation.
#' @param df Integer. Degrees of freedom for the spatial smooth. Default: 6.
#'   Higher values capture more spatial structure but risk removing real signal.
#' @param keep_original Logical. If TRUE, returns both original and debiased
#'   columns (debiased columns have suffix \code{_debiased}). Default: FALSE.
#'
#' @return A list with class \code{"borg_debias"} containing:
#'   \describe{
#'     \item{data}{Debiased data frame (original columns replaced or augmented)}
#'     \item{spatial_r2}{Named numeric vector. R-squared of spatial smooth
#'       for each predictor (how much spatial structure was removed)}
#'     \item{predictors}{Debiased predictor column names}
#'     \item{method}{Smoothing method used}
#'     \item{df}{Degrees of freedom}
#'     \item{assessment}{Character: "minimal" (less than 10 percent spatial
#'       variance), "moderate" (10-40 percent), or "substantial" (over 40
#'       percent)}
#'   }
#'   Has \code{print()} and \code{autoplot()} methods.
#'
#' @details
#' \subsection{When to use}{
#' Use when your predictors have spatial structure (e.g. climate variables
#' that vary smoothly over space). If \code{borg_diagnose()} detects
#' spatial autocorrelation in both residuals and predictors, spatial
#' confounding is likely. Debiasing is especially important for
#' inference (coefficient interpretation) rather than prediction.
#' }
#'
#' \subsection{How it works}{
#' For each predictor \eqn{X_j}:
#' \enumerate{
#'   \item Fit \eqn{X_j \sim f(s_1, s_2)} where \eqn{f} is a smooth
#'     function of coordinates.
#'   \item Replace \eqn{X_j} with the residuals \eqn{X_j - \hat{f}(s_1, s_2)}.
#' }
#' The residuals contain only the non-spatial variation in \eqn{X_j}.
#' }
#'
#' @references
#' Dupont, E., Wood, S. N., & Augustin, N. H. (2022). Spatial+: A novel
#' approach to spatial confounding. \emph{Biometrics}, 78(4), 1279-1290.
#' \doi{10.1111/biom.13656}
#'
#' @examples
#' set.seed(42)
#' d <- data.frame(
#'   x = runif(200, 0, 100), y = runif(200, 0, 100),
#'   temp = NA, elev = rnorm(200)
#' )
#' # temp has spatial structure
#' d$temp <- sin(d$x / 20) + cos(d$y / 20) + rnorm(200, sd = 0.3)
#' d$z <- 0.5 * d$temp + d$elev + rnorm(200, sd = 0.5)
#'
#' db <- borg_debias(d, coords = c("x", "y"), target = "z")
#' db
#'
#' @export
borg_debias <- function(data, predictors = NULL, coords,
                          target = NULL, method = c("gam_approx", "tps"),
                          df = 6, keep_original = FALSE) {

  method <- match.arg(method)

  if (length(coords) != 2 || !all(coords %in% names(data))) {
    stop("coords must be a character vector of length 2 naming columns in data")
  }

  exclude <- c(coords, target)
  if (is.null(predictors)) {
    predictors <- setdiff(names(data), exclude)
    predictors <- predictors[vapply(data[predictors], is.numeric, logical(1))]
  }

  if (length(predictors) == 0) {
    stop("No numeric predictor columns to debias")
  }

  s1 <- data[[coords[1]]]
  s2 <- data[[coords[2]]]

  # Build spatial basis
  basis <- .spatial_basis(s1, s2, df = df, method = method)

  # Debias each predictor
  spatial_r2 <- setNames(numeric(length(predictors)), predictors)
  debiased <- data

  for (var in predictors) {
    y <- data[[var]]
    complete <- !is.na(y)

    if (sum(complete) < df + 2) {
      spatial_r2[var] <- 0
      next
    }

    fit <- stats::lm(y[complete] ~ basis[complete, , drop = FALSE])
    fitted_vals <- rep(NA_real_, length(y))
    fitted_vals[complete] <- stats::fitted(fit)
    resids <- y - fitted_vals

    # R-squared: how much variance is spatial
    ss_total <- sum((y[complete] - mean(y[complete]))^2)
    ss_model <- sum((stats::fitted(fit) - mean(y[complete]))^2)
    spatial_r2[var] <- if (ss_total > 0) ss_model / ss_total else 0

    if (keep_original) {
      debiased[[paste0(var, "_debiased")]] <- resids
    } else {
      debiased[[var]] <- resids
    }
  }

  debiased_predictors <- if (keep_original) {
    paste0(predictors, "_debiased")
  } else {
    predictors
  }

  # Assessment
  mean_r2 <- mean(spatial_r2)
  assessment <- if (mean_r2 < 0.10) "minimal"
                else if (mean_r2 < 0.40) "moderate"
                else "substantial"

  result <- list(
    data = debiased,
    spatial_r2 = spatial_r2,
    predictors = debiased_predictors,
    original_predictors = predictors,
    method = method,
    df = df,
    assessment = assessment
  )

  class(result) <- c("borg_debias", "list")
  result
}


# Internal: build spatial basis matrix ------------------------------------
.spatial_basis <- function(s1, s2, df, method) {
  # Standardize coordinates
  s1_s <- (s1 - mean(s1, na.rm = TRUE)) / (stats::sd(s1, na.rm = TRUE) + 1e-10)
  s2_s <- (s2 - mean(s2, na.rm = TRUE)) / (stats::sd(s2, na.rm = TRUE) + 1e-10)

  if (method == "tps") {
    # Thin plate spline basis approximation using radial basis functions
    n <- length(s1)
    n_knots <- min(df, n %/% 5, 20)
    if (n_knots < 3) n_knots <- 3

    # Place knots via k-means
    coord_mat <- cbind(s1_s, s2_s)
    complete <- complete.cases(coord_mat)
    km <- stats::kmeans(coord_mat[complete, ], centers = n_knots, nstart = 5)
    knots <- km$centers

    # Radial basis: r^2 * log(r) for TPS
    basis <- matrix(0, n, n_knots + 3)
    basis[, 1] <- 1
    basis[, 2] <- s1_s
    basis[, 3] <- s2_s
    for (k in seq_len(n_knots)) {
      r2 <- (s1_s - knots[k, 1])^2 + (s2_s - knots[k, 2])^2
      r2[r2 == 0] <- 1e-10
      basis[, k + 3] <- r2 * log(r2) / 2
    }
    basis[is.na(basis)] <- 0
    basis

  } else {
    # Polynomial basis (no external dependency)
    # Generate polynomial terms up to specified df
    max_degree <- max(2, floor(sqrt(df)))
    terms <- list()
    for (i in 0:max_degree) {
      for (j in 0:max_degree) {
        if (i + j > 0 && i + j <= max_degree) {
          terms[[length(terms) + 1]] <- s1_s^i * s2_s^j
        }
      }
    }
    # Trim to df columns
    if (length(terms) > df) terms <- terms[seq_len(df)]
    basis <- do.call(cbind, terms)
    basis[is.na(basis)] <- 0
    basis
  }
}


#' @export
print.borg_debias <- function(x, ...) {
  cat("BORG Spatial+ Debiasing\n")
  cat("=======================\n\n")
  cat(sprintf("  Method: %s (df = %d)\n", x$method, x$df))
  cat(sprintf("  Spatial confounding: %s\n\n", x$assessment))

  cat("Per-predictor spatial R-squared (variance removed):\n")
  r2 <- sort(x$spatial_r2, decreasing = TRUE)
  for (var in names(r2)) {
    bar <- paste(rep("#", max(1, round(r2[var] * 30))), collapse = "")
    cat(sprintf("  %-20s %.1f%%  %s\n", var, r2[var] * 100, bar))
  }
  invisible(x)
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_debias <- function(object, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }

  df <- data.frame(
    variable = names(object$spatial_r2),
    spatial_r2 = unname(object$spatial_r2),
    stringsAsFactors = FALSE
  )
  df <- df[order(df$spatial_r2), ]
  df$variable <- factor(df$variable, levels = df$variable)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$variable, y = .data$spatial_r2)) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = .data$variable, y = 0, yend = .data$spatial_r2),
      linewidth = 0.8, color = "#2C3E50"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$spatial_r2),
      size = 3
    ) +
    ggplot2::scale_color_gradient(low = "#27AE60", high = "#E74C3C",
                                   guide = "none") +
    ggplot2::geom_hline(yintercept = c(0.1, 0.4), linetype = "dashed",
                         color = "gray60", linewidth = 0.3) +
    ggplot2::annotate("text", x = 0.5, y = c(0.1, 0.4),
                       label = c("minimal", "substantial"),
                       hjust = -0.1, vjust = -0.5, size = 2.5,
                       color = "gray50") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Spatial Variance Removed (Spatial+)",
      subtitle = sprintf("Mean R\u00b2 = %.1f%% | %s confounding",
                          mean(object$spatial_r2) * 100, object$assessment),
      x = NULL, y = "Spatial R\u00b2"
    ) +
    borg_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}
