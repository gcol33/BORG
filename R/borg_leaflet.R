# ===========================================================================
# borg_leaflet() — Interactive spatial fold visualization
# ===========================================================================

#' Interactive Leaflet Map of Spatial CV Folds
#'
#' Creates an interactive leaflet map showing train/test point assignments
#' across CV folds. Requires geographic coordinates (lat/lon).
#'
#' @param object A \code{borg_result} or \code{borg_cv} object with spatial folds.
#' @param data Data frame with coordinate columns. Required.
#' @param coords Character vector of length 2. Coordinate column names
#'   (longitude first, latitude second).
#' @param fold Integer or \code{"all"}. Which fold to display. Default: 1.
#'   Use \code{"all"} for a layer-group selector across all folds.
#'
#' @return A \code{leaflet} htmlwidget.
#'
#' @details
#' Requires the \pkg{leaflet} package. Coordinates must be in WGS84
#' (longitude/latitude). Points are color-coded: blue = train, red = test.
#' Click points for popups with index and fold assignment.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("leaflet", quietly = TRUE)) {
#'   set.seed(42)
#'   d <- data.frame(
#'     lon = runif(100, 10, 20),
#'     lat = runif(100, 45, 55),
#'     z = rnorm(100)
#'   )
#'   result <- borg(d, coords = c("lon", "lat"), target = "z")
#'   borg_leaflet(result, data = d, coords = c("lon", "lat"))
#' }
#' }
#'
#' @export
borg_leaflet <- function(object, data, coords, fold = 1) {

  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' required for borg_leaflet(). ",
         "Install with: install.packages('leaflet')")
  }

  # Extract folds
  folds <- if (inherits(object, "borg_cv")) {
    object$folds
  } else if (inherits(object, "borg_result")) {
    object$folds
  } else {
    stop("object must be borg_result or borg_cv")
  }

  if (is.null(data) || is.null(coords)) {
    stop("'data' and 'coords' are required for leaflet maps")
  }

  coord_info <- extract_coords(data, coords)
  lon <- coord_info$x
  lat <- coord_info$y

  train_color <- "#1B4F72"
  test_color <- "#C0392B"

  if (identical(fold, "all")) {
    # Layer groups for all folds
    m <- leaflet::leaflet() |>
      leaflet::addTiles()

    for (i in seq_along(folds)) {
      group_name <- sprintf("Fold %d", i)
      train_idx <- folds[[i]]$train
      test_idx <- folds[[i]]$test

      m <- m |>
        leaflet::addCircleMarkers(
          lng = lon[train_idx], lat = lat[train_idx],
          radius = 4, color = train_color, fillOpacity = 0.6,
          stroke = FALSE, group = group_name,
          popup = sprintf("Index: %d<br>Role: Train<br>Fold: %d", train_idx, i)
        ) |>
        leaflet::addCircleMarkers(
          lng = lon[test_idx], lat = lat[test_idx],
          radius = 5, color = test_color, fillOpacity = 0.8,
          stroke = TRUE, weight = 1, group = group_name,
          popup = sprintf("Index: %d<br>Role: Test<br>Fold: %d", test_idx, i)
        )
    }

    fold_names <- sprintf("Fold %d", seq_along(folds))
    m <- m |>
      leaflet::addLayersControl(
        overlayGroups = fold_names,
        options = leaflet::layersControlOptions(collapsed = FALSE)
      ) |>
      leaflet::hideGroup(fold_names[-1])  # Show only first fold initially

  } else {
    # Single fold
    fold <- as.integer(fold)
    if (fold > length(folds)) {
      stop(sprintf("Fold %d not available (only %d folds)", fold, length(folds)))
    }

    train_idx <- folds[[fold]]$train
    test_idx <- folds[[fold]]$test

    m <- leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::addCircleMarkers(
        lng = lon[train_idx], lat = lat[train_idx],
        radius = 4, color = train_color, fillOpacity = 0.6,
        stroke = FALSE,
        popup = sprintf("Index: %d<br>Role: Train", train_idx)
      ) |>
      leaflet::addCircleMarkers(
        lng = lon[test_idx], lat = lat[test_idx],
        radius = 5, color = test_color, fillOpacity = 0.8,
        stroke = TRUE, weight = 1,
        popup = sprintf("Index: %d<br>Role: Test", test_idx)
      ) |>
      leaflet::addLegend(
        position = "bottomright",
        colors = c(train_color, test_color),
        labels = c(sprintf("Train (n=%d)", length(train_idx)),
                    sprintf("Test (n=%d)", length(test_idx))),
        title = sprintf("Fold %d", fold)
      )
  }

  m
}
