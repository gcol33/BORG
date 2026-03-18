# ===========================================================================
# sf conversion utilities for spatial CV visualization
# ===========================================================================

#' Convert Data to sf POINT Geometry (Internal)
#'
#' Accepts data.frame (with coords), sf, or SpatVector. Returns sf.
#'
#' @noRd
to_sf <- function(data, coords = NULL, crs = NULL) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' required for spatial visualization. ",
         "Install with: install.packages('sf')")
  }

  if (inherits(data, "sf")) return(data)

  if (inherits(data, "SpatVector")) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("Package 'terra' required for SpatVector conversion. ",
           "Install with: install.packages('terra')")
    }
    return(sf::st_as_sf(data))
  }

  # data.frame path

  if (is.null(coords) || length(coords) < 2) {
    stop("'coords' required to convert data.frame to sf")
  }
  sf::st_as_sf(data, coords = coords, crs = crs)
}


#' Create sf Data Frame for a Single Fold (Internal)
#'
#' Adds a 'role' column ("train"/"test") based on fold indices.
#'
#' @noRd
fold_sf <- function(data_sf, folds, fold_id) {
  n <- nrow(data_sf)
  role <- rep("excluded", n)
  role[folds[[fold_id]]$train] <- "train"
  role[folds[[fold_id]]$test] <- "test"
  data_sf$role <- factor(role, levels = c("train", "test", "excluded"))
  data_sf$fold <- fold_id

  data_sf
}


#' Stack All Folds into Long-Format sf (Internal)
#'
#' Returns sf data.frame with 'fold' (integer) and 'role' columns.
#'
#' @noRd
all_folds_sf <- function(data_sf, folds) {
  rows <- lapply(seq_along(folds), function(i) fold_sf(data_sf, folds, i))
  do.call(rbind, rows)
}
