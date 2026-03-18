# Spatial data abstraction layer
# Unified coordinate extraction from data.frame, sf, and SpatVector objects

#' Extract Coordinates from Various Spatial Data Types (Internal)
#'
#' Dispatches on class: data.frame uses named columns, sf uses st_coordinates(),
#' SpatVector uses terra::crds(). Returns a standardized list.
#'
#' @noRd
extract_coords <- function(data, coords = NULL) {
  if (inherits(data, "SpatVector")) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("Package 'terra' required for SpatVector inputs. ",
           "Install with: install.packages('terra')")
    }
    xy <- terra::crds(data)
    crs_val <- terra::crs(data)
    return(list(
      x = xy[, 1],
      y = xy[, 2],
      crs = if (nzchar(crs_val)) crs_val else NULL,
      coord_names = c("x", "y")
    ))
  }

  if (inherits(data, "sf")) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Package 'sf' required for sf inputs. ",
           "Install with: install.packages('sf')")
    }
    xy <- sf::st_coordinates(sf::st_geometry(data))
    crs_val <- sf::st_crs(data)
    return(list(
      x = xy[, 1],
      y = xy[, 2],
      crs = if (!is.na(crs_val)) crs_val else NULL,
      coord_names = c("X", "Y")
    ))
  }

  # Plain data.frame

  if (is.null(coords) || length(coords) < 2) {
    stop("'coords' must be a character vector of length 2 for data.frame inputs")
  }
  missing_cols <- setdiff(coords, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Coordinate columns not found: %s",
                 paste(missing_cols, collapse = ", ")))
  }
  list(
    x = data[[coords[1]]],
    y = data[[coords[2]]],
    crs = NULL,
    coord_names = coords
  )
}


#' Coerce sf or SpatVector to Plain Data Frame (Internal)
#' @noRd
extract_data_frame <- function(data) {
  if (inherits(data, "SpatVector")) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("Package 'terra' required for SpatVector inputs. ",
           "Install with: install.packages('terra')")
    }
    return(as.data.frame(terra::values(data)))
  }

  if (inherits(data, "sf")) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Package 'sf' required for sf inputs. ",
           "Install with: install.packages('sf')")
    }
    return(sf::st_drop_geometry(data))
  }

  as.data.frame(data)
}


#' Compute Distance Matrix with CRS Awareness (Internal)
#'
#' Uses Haversine for geographic CRS (lat/lon), Euclidean otherwise.
#'
#' @noRd
compute_distance_matrix_geo <- function(x, y, crs = NULL) {
  if (!is.null(crs) && is_geographic_crs(crs)) {
    return(haversine_distance_matrix(x, y))
  }
  compute_distance_matrix(x, y)
}


#' Check if CRS is Geographic (lat/lon) (Internal)
#' @noRd
is_geographic_crs <- function(crs) {
  if (is.null(crs)) return(FALSE)

  if (!requireNamespace("sf", quietly = TRUE)) return(FALSE)

  tryCatch({
    crs_obj <- sf::st_crs(crs)
    if (is.na(crs_obj)) return(FALSE)
    sf::st_is_longlat(crs_obj)
  }, error = function(e) FALSE)
}


#' Haversine Distance Matrix (Internal)
#'
#' Computes pairwise great-circle distances in meters.
#' x = longitude (degrees), y = latitude (degrees).
#'
#' @noRd
haversine_distance_matrix <- function(lon, lat) {
  n <- length(lon)
  R <- 6371000  # Earth radius in meters

  lon_rad <- lon * pi / 180
  lat_rad <- lat * pi / 180

  outer(1:n, 1:n, function(i, j) {
    dlat <- lat_rad[j] - lat_rad[i]
    dlon <- lon_rad[j] - lon_rad[i]
    a <- sin(dlat / 2)^2 + cos(lat_rad[i]) * cos(lat_rad[j]) * sin(dlon / 2)^2
    2 * R * asin(sqrt(a))
  })
}
