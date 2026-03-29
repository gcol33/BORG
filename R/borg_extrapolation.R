# ===========================================================================
# borg_extrapolation() — Detect prediction into unsampled feature space
# ===========================================================================

#' Detect Model Extrapolation
#'
#' Checks whether prediction locations fall outside the environmental
#' envelope of training data. Uses per-variable range checks and
#' multivariate Mahalanobis distance.
#'
#' @param train Data frame of training data.
#' @param new Data frame of prediction data (same predictor columns).
#' @param predictors Character vector. Predictor column names. If
#'   \code{NULL}, uses all shared numeric columns.
#' @param coords Character vector of length 2. Coordinate columns in
#'   \code{new} for spatial mapping. Optional.
#'
#' @return A data frame with class \code{"borg_extrapolation"} containing:
#'   \describe{
#'     \item{mahal_dist}{Mahalanobis distance to training centroid}
#'     \item{n_vars_outside}{Number of variables outside training range}
#'     \item{vars_outside}{Comma-separated names of out-of-range variables}
#'     \item{extrapolating}{Logical. TRUE if any variable is out of range}
#'     \item{x, y}{Coordinates (if provided)}
#'   }
#'
#' @examples
#' set.seed(42)
#' train <- data.frame(a = rnorm(80), b = rnorm(80))
#' new <- data.frame(a = c(rnorm(10), rnorm(10, mean = 5)),
#'                    b = c(rnorm(10), rnorm(10, mean = 5)))
#' ext <- borg_extrapolation(train, new)
#' table(ext$extrapolating)
#'
#' @export
borg_extrapolation <- function(train, new, predictors = NULL, coords = NULL) {

  if (is.null(predictors)) {
    shared <- intersect(names(train), names(new))
    predictors <- shared[vapply(train[shared], is.numeric, logical(1))]
    # Exclude coords
    if (!is.null(coords)) predictors <- setdiff(predictors, coords)
  }

  if (length(predictors) == 0) stop("No numeric predictor columns found")

  train_mat <- as.matrix(train[, predictors, drop = FALSE])
  new_mat <- as.matrix(new[, predictors, drop = FALSE])

  # Per-variable range check
  train_ranges <- apply(train_mat, 2, range, na.rm = TRUE)

  n_outside <- integer(nrow(new_mat))
  vars_outside <- character(nrow(new_mat))

  for (i in seq_len(nrow(new_mat))) {
    below <- new_mat[i, ] < train_ranges[1, ]
    above <- new_mat[i, ] > train_ranges[2, ]
    out <- below | above
    out[is.na(out)] <- FALSE
    n_outside[i] <- sum(out)
    if (any(out)) {
      vars_outside[i] <- paste(predictors[out], collapse = ", ")
    }
  }

  # Mahalanobis distance
  train_center <- colMeans(train_mat, na.rm = TRUE)
  train_cov <- stats::cov(train_mat, use = "pairwise.complete.obs")

  # Regularize covariance if singular
  if (any(is.na(train_cov)) || det(train_cov) < .Machine$double.eps) {
    train_cov <- train_cov + diag(1e-6, ncol(train_cov))
  }

  mahal <- tryCatch(
    stats::mahalanobis(new_mat, center = train_center, cov = train_cov),
    error = function(e) rep(NA_real_, nrow(new_mat))
  )

  result <- data.frame(
    mahal_dist = mahal,
    n_vars_outside = n_outside,
    vars_outside = vars_outside,
    extrapolating = n_outside > 0,
    stringsAsFactors = FALSE
  )

  if (!is.null(coords) && all(coords %in% names(new))) {
    result$x <- new[[coords[1]]]
    result$y <- new[[coords[2]]]
  }

  attr(result, "predictors") <- predictors
  attr(result, "coord_names") <- coords
  attr(result, "train_ranges") <- train_ranges
  class(result) <- c("borg_extrapolation", "data.frame")

  result
}


#' @export
print.borg_extrapolation <- function(x, ...) {
  n <- nrow(x)
  n_ext <- sum(x$extrapolating)
  cat("BORG Extrapolation Check\n")
  cat(sprintf("  %d / %d points extrapolating (%.1f%%)\n",
              n_ext, n, 100 * n_ext / n))
  if (n_ext > 0) {
    # Most common out-of-range variables
    all_vars <- unlist(strsplit(x$vars_outside[x$extrapolating], ", "))
    tab <- sort(table(all_vars), decreasing = TRUE)
    cat("  Most common out-of-range variables:\n")
    for (v in head(names(tab), 5)) {
      cat(sprintf("    %s: %d points\n", v, tab[v]))
    }
  }
  invisible(x)
}


#' @exportS3Method ggplot2::autoplot
autoplot.borg_extrapolation <- function(object, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }

  has_coords <- "x" %in% names(object)
  coord_names <- attr(object, "coord_names") %||% c("x", "y")

  if (has_coords) {
    ggplot2::ggplot(object, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_point(
        ggplot2::aes(color = .data$n_vars_outside, shape = .data$extrapolating),
        size = 1.5, alpha = 0.8
      ) +
      ggplot2::scale_color_viridis_c(option = "rocket", direction = -1) +
      ggplot2::scale_shape_manual(
        values = c("TRUE" = 4, "FALSE" = 16),
        labels = c("TRUE" = "Extrapolating", "FALSE" = "Interpolating")
      ) +
      ggplot2::coord_equal() +
      ggplot2::labs(
        title = "Extrapolation Map",
        subtitle = sprintf("%.1f%% extrapolating", 100 * mean(object$extrapolating)),
        x = coord_names[1], y = coord_names[2],
        color = "Vars out\nof range", shape = NULL
      ) +
      borg_theme()
  } else {
    ggplot2::ggplot(object, ggplot2::aes(x = .data$mahal_dist)) +
      ggplot2::geom_histogram(
        ggplot2::aes(fill = .data$extrapolating),
        bins = 30, alpha = 0.7, color = "white", linewidth = 0.2
      ) +
      ggplot2::scale_fill_manual(
        values = c("TRUE" = "#E74C3C", "FALSE" = "#27AE60"),
        labels = c("TRUE" = "Extrapolating", "FALSE" = "Interpolating")
      ) +
      ggplot2::labs(
        title = "Mahalanobis Distance Distribution",
        subtitle = sprintf("%.1f%% outside training envelope",
                            100 * mean(object$extrapolating)),
        x = "Mahalanobis distance", y = "Count", fill = NULL
      ) +
      borg_theme()
  }
}
