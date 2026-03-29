# ===========================================================================
# borg_extract() — Bridge between raster SDM workflows and BORG
# ===========================================================================

#' Extract Raster Values at Point Locations for BORG
#'
#' Convenience function that extracts environmental raster values at species
#' occurrence (or other point) locations and returns a BORG-ready data frame
#' with coordinates attached.
#'
#' This bridges the standard SDM workflow (rasters + points) with BORG's
#' data.frame interface. The returned data frame can be passed directly to
#' \code{\link{borg}()}, \code{\link{borg_cv}()}, or \code{\link{borg_diagnose}()}.
#'
#' @param raster A \code{terra::SpatRaster} of environmental layers (predictors).
#' @param points Occurrence locations. One of:
#'   \itemize{
#'     \item A \code{terra::SpatVector} of point geometries
#'     \item An \code{sf} object with point geometries
#'     \item A \code{data.frame} with coordinate columns (requires \code{coords})
#'     \item A two-column matrix of coordinates (x/lon, y/lat)
#'   }
#' @param coords Character vector of length 2 giving coordinate column names.
#'   Required when \code{points} is a data.frame. Ignored for sf/SpatVector.
#' @param na.rm Logical. If \code{TRUE} (default), rows with any \code{NA} in
#'   extracted raster values are removed. Set to \code{FALSE} to keep all rows.
#' @param coord_names Character vector of length 2. Column names for the
#'   coordinates in the output. Default: \code{c("x", "y")}.
#'
#' @return A data frame with columns for coordinates (named by
#'   \code{coord_names}), one column per raster layer (environmental
#'   variables), and any additional columns from the input \code{points} data.
#'   The returned data frame has a \code{"borg_coords"} attribute storing the
#'   coordinate column names, so that \code{borg()} can auto-detect them.
#'
#' @details
#' Requires the \pkg{terra} package. If \code{points} is an \code{sf} object,
#' the \pkg{sf} package is also required.
#'
#' The function performs the equivalent of:
#' \preformatted{
#' env_data <- terra::extract(raster, points, ID = FALSE)
#' coords <- terra::crds(points)
#' cbind(coords, env_data)
#' }
#' but handles edge cases (NA removal, coordinate injection, CRS checks)
#' and attaches metadata so downstream BORG functions can auto-detect
#' spatial structure.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("terra", quietly = TRUE)) {
#'   # Create example raster and points
#'   r <- terra::rast(nrows = 50, ncols = 50,
#'                    xmin = 0, xmax = 100, ymin = 0, ymax = 100)
#'   terra::values(r) <- runif(terra::ncell(r))
#'   names(r) <- "bio1"
#'
#'   pts <- terra::vect(
#'     cbind(x = runif(100, 0, 100), y = runif(100, 0, 100)),
#'     crs = terra::crs(r)
#'   )
#'
#'   # Extract and run BORG
#'   d <- borg_extract(r, pts)
#'   result <- borg(d, coords = c("x", "y"), target = "bio1")
#' }
#' }
#'
#' @export
borg_extract <- function(raster,
                          points,
                          coords = NULL,
                          na.rm = TRUE,
                          coord_names = c("x", "y")) {

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' required for borg_extract(). ",
         "Install with: install.packages('terra')")
  }

  # Validate raster
  if (!inherits(raster, "SpatRaster")) {
    stop(sprintf("'raster' must be a terra::SpatRaster, got %s", class(raster)[1]))
  }

  # Convert points to SpatVector for extraction
  if (inherits(points, "SpatVector")) {
    pts_vect <- points
    pts_data <- as.data.frame(terra::values(points))
  } else if (inherits(points, "sf")) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Package 'sf' required for sf inputs. ",
           "Install with: install.packages('sf')")
    }
    pts_vect <- terra::vect(points)
    pts_data <- sf::st_drop_geometry(points)
  } else if (is.matrix(points)) {
    if (ncol(points) < 2) {
      stop("Matrix 'points' must have at least 2 columns (x, y)")
    }
    pts_vect <- terra::vect(points[, 1:2, drop = FALSE],
                             crs = terra::crs(raster))
    pts_data <- data.frame()
  } else if (is.data.frame(points)) {
    if (is.null(coords) || length(coords) < 2) {
      stop("'coords' must specify 2 column names when 'points' is a data.frame")
    }
    missing <- setdiff(coords, names(points))
    if (length(missing) > 0) {
      stop(sprintf("Coordinate columns not found: %s", paste(missing, collapse = ", ")))
    }
    xy <- as.matrix(points[, coords, drop = FALSE])
    pts_vect <- terra::vect(xy, crs = terra::crs(raster))
    pts_data <- points[, setdiff(names(points), coords), drop = FALSE]
  } else {
    stop(sprintf("'points' must be SpatVector, sf, data.frame, or matrix, got %s",
                 class(points)[1]))
  }

  # Extract raster values
  env_vals <- terra::extract(raster, pts_vect, ID = FALSE)

  # Get coordinates
  xy <- terra::crds(pts_vect)

  # Build output data frame
  result <- data.frame(xy)
  names(result) <- coord_names

  # Add raster values
  result <- cbind(result, env_vals)

  # Add any original point attributes
  if (ncol(pts_data) > 0) {
    result <- cbind(result, pts_data)
  }

  # Remove rows with NA in raster values
  if (na.rm) {
    raster_cols <- names(env_vals)
    complete <- complete.cases(result[, raster_cols, drop = FALSE])
    n_removed <- sum(!complete)
    if (n_removed > 0) {
      message(sprintf("Removed %d points with NA raster values (%d remaining)",
                       n_removed, sum(complete)))
      result <- result[complete, , drop = FALSE]
      rownames(result) <- NULL
    }
  }

  # Attach coordinate metadata for BORG auto-detection
  attr(result, "borg_coords") <- coord_names

  result
}
