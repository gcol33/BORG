# Interactive Leaflet Map of Spatial CV Folds

Creates an interactive leaflet map showing train/test point assignments
across CV folds. Requires geographic coordinates (lat/lon).

## Usage

``` r
borg_leaflet(object, data, coords, fold = 1)
```

## Arguments

- object:

  A `borg_result` or `borg_cv` object with spatial folds.

- data:

  Data frame with coordinate columns. Required.

- coords:

  Character vector of length 2. Coordinate column names (longitude
  first, latitude second).

- fold:

  Integer or `"all"`. Which fold to display. Default: 1. Use `"all"` for
  a layer-group selector across all folds.

## Value

A `leaflet` htmlwidget.

## Details

Requires the leaflet package. Coordinates must be in WGS84
(longitude/latitude). Points are color-coded: blue = train, red = test.
Click points for popups with index and fold assignment.

## Examples

``` r
# \donttest{
if (requireNamespace("leaflet", quietly = TRUE)) {
  set.seed(42)
  d <- data.frame(
    lon = runif(100, 10, 20),
    lat = runif(100, 45, 55),
    z = rnorm(100)
  )
  result <- borg(d, coords = c("lon", "lat"), target = "z")
  borg_leaflet(result, data = d, coords = c("lon", "lat"))
}
# }
```
