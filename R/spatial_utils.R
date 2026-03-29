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


#' Apply Spatial Buffer Exclusion to CV Folds (Internal)
#'
#' Removes training observations within `buffer` distance of any test point.
#' Uses CRS-aware distance (Haversine for geographic, Euclidean otherwise).
#'
#' @noRd
apply_spatial_buffer <- function(folds, x_coord, y_coord, buffer,
                                  crs = NULL, verbose = FALSE) {
  use_geo <- !is.null(crs) && is_geographic_crs(crs)
  n_excluded_total <- 0L

  buffered_folds <- lapply(seq_along(folds), function(i) {
    fold <- folds[[i]]
    test_x <- x_coord[fold$test]
    test_y <- y_coord[fold$test]

    train_keep <- vapply(fold$train, function(j) {
      if (use_geo) {
        lon_j <- x_coord[j] * pi / 180
        lat_j <- y_coord[j] * pi / 180
        lon_t <- test_x * pi / 180
        lat_t <- test_y * pi / 180
        dlat <- lat_t - lat_j
        dlon <- lon_t - lon_j
        a <- sin(dlat / 2)^2 + cos(lat_j) * cos(lat_t) * sin(dlon / 2)^2
        dists <- 2 * 6371000 * asin(sqrt(a))
      } else {
        dists <- sqrt((x_coord[j] - test_x)^2 + (y_coord[j] - test_y)^2)
      }
      min(dists) > buffer
    }, logical(1))

    n_removed <- sum(!train_keep)
    n_excluded_total <<- n_excluded_total + n_removed

    if (verbose && n_removed > 0) {
      message(sprintf("  Fold %d: excluded %d training obs within buffer (%.1f%%)",
                       i, n_removed, 100 * n_removed / length(fold$train)))
    }

    n_remaining <- sum(train_keep)
    if (n_remaining < length(fold$test)) {
      warning(sprintf(
        "Fold %d: buffer excluded %d of %d training obs (%d remaining). Consider reducing buffer.",
        i, n_removed, length(fold$train), n_remaining
      ))
    }

    list(train = fold$train[train_keep], test = fold$test)
  })

  if (verbose) {
    message(sprintf("Spatial buffer (%.1f): excluded %d training obs across %d folds",
                     buffer, n_excluded_total, length(folds)))
  }

  # Preserve attributes from original folds
  for (a in setdiff(names(attributes(folds)), c("names", "class"))) {
    attr(buffered_folds, a) <- attr(folds, a)
  }
  attr(buffered_folds, "buffer_meta") <- list(
    buffer = buffer, n_excluded = n_excluded_total
  )

  buffered_folds
}
