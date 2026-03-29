# ===========================================================================
# borg_thin() — Spatial thinning to reduce clustering bias
# ===========================================================================

#' Spatially Thin Occurrence Data
#'
#' Reduces spatial clustering by ensuring a minimum distance between all
#' retained observations. Uses iterative nearest-neighbor removal: at each
#' step, the point with the smallest nearest-neighbor distance is removed
#' until all pairwise distances exceed \code{min_dist}.
#'
#' @param data A data frame, \code{sf} object, or \code{terra::SpatVector}.
#' @param coords Character vector of length 2. Coordinate column names.
#'   Required for data frames; ignored for sf/SpatVector.
#' @param min_dist Numeric. Minimum distance between retained points.
#'   Units match the coordinate system (degrees for lat/lon, meters for
#'   projected CRS). If \code{NULL}, uses 10 percent of the nearest-neighbor
#'   interquartile range as a default.
#' @param verbose Logical. Print thinning progress. Default: FALSE.
#'
#' @return The input data filtered to retained rows. Has attribute
#'   \code{"thinned_idx"} with the original row indices of kept observations,
#'   and \code{"n_removed"} with the count of removed points.
#'
#' @details
#' Spatial thinning is standard practice in species distribution modelling
#' to reduce sampling bias. Clustered occurrences inflate apparent model
#' performance and bias spatial CV fold sizes.
#'
#' For geographic coordinates (lat/lon), distances are computed using the
#' Haversine formula (meters). For projected coordinates, Euclidean distance
#' is used.
#'
#' @examples
#' # Thin clustered points to 5-unit minimum spacing
#' set.seed(42)
#' d <- data.frame(
#'   x = c(rnorm(50, 0, 1), rnorm(50, 10, 1)),
#'   y = c(rnorm(50, 0, 1), rnorm(50, 10, 1)),
#'   species = "A"
#' )
#' d_thin <- borg_thin(d, coords = c("x", "y"), min_dist = 1)
#' nrow(d_thin)  # fewer rows
#'
#' @export
borg_thin <- function(data, coords = NULL, min_dist = NULL, verbose = FALSE) {

  # Extract coordinates
  crs <- NULL
  if (inherits(data, "SpatVector")) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("Package 'terra' required for SpatVector inputs")
    }
    coord_info <- extract_coords(data)
    crs <- coord_info$crs
  } else if (inherits(data, "sf")) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Package 'sf' required for sf inputs")
    }
    coord_info <- extract_coords(data)
    crs <- coord_info$crs
  } else {
    if (is.null(coords) || length(coords) < 2) {
      stop("'coords' must be a character vector of length 2 for data.frame inputs")
    }
    coord_info <- extract_coords(data, coords)
  }

  x <- coord_info$x
  y <- coord_info$y
  n <- length(x)

  if (n < 3) return(data)

  # Compute distance matrix
  use_geo <- !is.null(crs) && is_geographic_crs(crs)
  if (use_geo) {
    dist_mat <- haversine_distance_matrix(x, y)
  } else {
    dist_mat <- compute_distance_matrix(x, y)
  }

  # Auto-detect min_dist if not provided
  if (is.null(min_dist)) {
    diag(dist_mat) <- Inf
    nn_dists <- apply(dist_mat, 1, min)
    min_dist <- stats::quantile(nn_dists, 0.25) * 2
    diag(dist_mat) <- 0
    if (verbose) message(sprintf("Auto min_dist: %.2f", min_dist))
  }

  # Iterative nearest-neighbor thinning
  keep <- rep(TRUE, n)
  diag(dist_mat) <- Inf  # exclude self

  repeat {
    active <- which(keep)
    if (length(active) < 2) break

    # Find nearest-neighbor distances among active points
    sub_mat <- dist_mat[active, active, drop = FALSE]
    nn_dists <- apply(sub_mat, 1, min)

    # Check if all above threshold
    min_nn <- min(nn_dists)
    if (min_nn >= min_dist) break

    # Remove the point with the smallest NN distance
    # Tie-break: remove the one whose second-NN is also smallest
    worst <- which.min(nn_dists)
    keep[active[worst]] <- FALSE
  }

  n_removed <- sum(!keep)
  kept_idx <- which(keep)

  if (verbose) {
    message(sprintf("Thinned: %d -> %d points (%d removed, min_dist = %.2f)",
                     n, length(kept_idx), n_removed, min_dist))
  }

  # Filter data
  if (inherits(data, "SpatVector")) {
    result <- data[kept_idx, ]
  } else {
    result <- data[kept_idx, , drop = FALSE]
    rownames(result) <- NULL
  }

  attr(result, "thinned_idx") <- kept_idx
  attr(result, "n_removed") <- n_removed

  result
}
