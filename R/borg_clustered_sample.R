# ===========================================================================
# borg_clustered_sample() — Clustered spatial sample design
# ===========================================================================

#' Draw a Clustered Spatial Sample
#'
#' Generates spatially clustered sample points using a parent-offspring
#' process: cluster centres are placed uniformly across the domain, and each
#' sample point is drawn near a randomly assigned centre. Clustered designs
#' are the common ecological sampling pattern and produce the strong spatial
#' dependence that random cross-validation understates, making this useful for
#' simulations, teaching, and stress-testing evaluation schemes.
#'
#' @param area The sampling domain. One of: a numeric vector
#'   \code{c(xmin, xmax, ymin, ymax)}; a data frame or matrix of coordinates
#'   (its bounding box is used); or an \code{sf} polygon or \code{SpatVector}
#'   polygon, in which case points are constrained to lie inside it.
#' @param n Integer. Total number of sample points to draw.
#' @param n_clusters Integer. Number of cluster centres (parents). Default: 5.
#' @param radius Numeric. Cluster spread: the maximum offset of a point from
#'   its centre, in coordinate units. If \code{NULL}, set to 5\% of the domain
#'   diagonal.
#' @param seed Integer or \code{NULL}. Random seed for reproducibility.
#'   Default: \code{NULL} (no seeding).
#'
#' @return A data frame with columns \code{x}, \code{y}, and \code{cluster}
#'   (the integer id of the assigned centre). Attributes \code{n_clusters} and
#'   \code{radius} record the design. Drops directly into
#'   \code{\link{borg_diagnose}()} and \code{\link{borg_cv}()} via
#'   \code{coords = c("x", "y")}.
#'
#' @examples
#' set.seed(1)
#' s <- borg_clustered_sample(c(0, 100, 0, 100), n = 120, n_clusters = 6)
#' nrow(s)
#' length(unique(s$cluster))
#'
#' @seealso \code{\link{borg_simulate}}, \code{\link{borg_diagnose}}
#' @export
borg_clustered_sample <- function(area, n, n_clusters = 5, radius = NULL,
                                  seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  n <- as.integer(n)
  n_clusters <- as.integer(n_clusters)
  if (n < 1L) stop("'n' must be a positive integer")
  if (n_clusters < 1L) stop("'n_clusters' must be a positive integer")

  dom <- resolve_sample_domain(area)
  bbox <- dom$bbox           # c(xmin, xmax, ymin, ymax)
  inside <- dom$inside       # function(x, y) -> logical, or NULL for a rectangle

  if (is.null(radius)) {
    diag_len <- sqrt((bbox[2] - bbox[1])^2 + (bbox[4] - bbox[3])^2)
    radius <- 0.05 * diag_len
  }

  # Parent centres, uniformly across the domain
  parents <- draw_uniform_in_domain(n_clusters, bbox, inside)

  # Assign points to parents (as balanced as n allows)
  assign <- sample(rep_len(seq_len(n_clusters), n))

  # Offspring: uniform within a disc of the given radius around each parent
  px <- numeric(n)
  py <- numeric(n)
  for (i in seq_len(n)) {
    p <- assign[i]
    px[i] <- parents$x[p]
    py[i] <- parents$y[p]
    for (attempt in seq_len(50L)) {
      ang <- stats::runif(1, 0, 2 * pi)
      rad <- radius * sqrt(stats::runif(1))
      cx <- parents$x[p] + rad * cos(ang)
      cy <- parents$y[p] + rad * sin(ang)
      in_box <- cx >= bbox[1] && cx <= bbox[2] && cy >= bbox[3] && cy <= bbox[4]
      if (in_box && (is.null(inside) || isTRUE(inside(cx, cy)))) {
        px[i] <- cx
        py[i] <- cy
        break
      }
    }
  }

  out <- data.frame(x = px, y = py, cluster = assign)
  attr(out, "n_clusters") <- n_clusters
  attr(out, "radius") <- radius
  out
}


#' Resolve a Sampling Domain to a Bounding Box and Inside-Test (Internal)
#'
#' @return list(bbox = c(xmin, xmax, ymin, ymax), inside = function|NULL).
#' @noRd
resolve_sample_domain <- function(area) {
  if (inherits(area, "sf") || inherits(area, "sfc")) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Package 'sf' is required to sample within an sf polygon")
    }
    bb <- sf::st_bbox(area)
    poly <- sf::st_geometry(area)
    inside <- function(x, y) {
      pt <- sf::st_sfc(sf::st_point(c(x, y)), crs = sf::st_crs(area))
      length(unlist(sf::st_intersects(pt, poly))) > 0
    }
    return(list(bbox = c(bb["xmin"], bb["xmax"], bb["ymin"], bb["ymax"]),
                inside = inside))
  }

  if (inherits(area, "SpatVector")) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("Package 'terra' is required to sample within a SpatVector polygon")
    }
    e <- as.vector(terra::ext(area))   # xmin, xmax, ymin, ymax
    inside <- function(x, y) {
      pt <- terra::vect(cbind(x, y), crs = terra::crs(area))
      isTRUE(terra::relate(pt, area, "intersects")[1, 1])
    }
    return(list(bbox = c(e[1], e[2], e[3], e[4]), inside = inside))
  }

  if (is.data.frame(area) || is.matrix(area)) {
    m <- as.matrix(area[, 1:2, drop = FALSE])
    return(list(bbox = c(min(m[, 1]), max(m[, 1]), min(m[, 2]), max(m[, 2])),
                inside = NULL))
  }

  if (is.numeric(area) && length(area) == 4) {
    if (area[2] <= area[1] || area[4] <= area[3]) {
      stop("Numeric 'area' must be c(xmin, xmax, ymin, ymax) with xmax > xmin and ymax > ymin")
    }
    return(list(bbox = as.numeric(area), inside = NULL))
  }

  stop("'area' must be c(xmin, xmax, ymin, ymax), a coordinate data frame/matrix, an sf polygon, or a SpatVector")
}


#' Draw Uniform Points Within a Domain (Internal)
#'
#' Rejection sampling against the domain's inside-test; falls back to the
#' bounding box when no polygon constraint is supplied.
#' @noRd
draw_uniform_in_domain <- function(n, bbox, inside) {
  x <- numeric(n)
  y <- numeric(n)
  for (i in seq_len(n)) {
    for (attempt in seq_len(200L)) {
      cx <- stats::runif(1, bbox[1], bbox[2])
      cy <- stats::runif(1, bbox[3], bbox[4])
      if (is.null(inside) || isTRUE(inside(cx, cy))) {
        x[i] <- cx
        y[i] <- cy
        break
      }
    }
  }
  list(x = x, y = y)
}
