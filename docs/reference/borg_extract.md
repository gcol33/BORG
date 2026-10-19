# Extract Raster Values at Point Locations for BORG

Convenience function that extracts environmental raster values at
species occurrence (or other point) locations and returns a BORG-ready
data frame with coordinates attached.

## Usage

``` r
borg_extract(
  raster,
  points,
  coords = NULL,
  na.rm = TRUE,
  coord_names = c("x", "y")
)
```

## Arguments

- raster:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  of environmental layers (predictors).

- points:

  Occurrence locations. One of:

  - A
    [`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
    of point geometries

  - An `sf` object with point geometries

  - A `data.frame` with coordinate columns (requires `coords`)

  - A two-column matrix of coordinates (x/lon, y/lat)

- coords:

  Character vector of length 2 giving coordinate column names. Required
  when `points` is a data.frame. Ignored for sf/SpatVector.

- na.rm:

  Logical. If `TRUE` (default), rows with any `NA` in extracted raster
  values are removed. Set to `FALSE` to keep all rows.

- coord_names:

  Character vector of length 2. Column names for the coordinates in the
  output. Default: `c("x", "y")`.

## Value

A data frame with columns for coordinates (named by `coord_names`), one
column per raster layer (environmental variables), and any additional
columns from the input `points` data. The returned data frame has a
`"borg_coords"` attribute storing the coordinate column names, so that
[`borg()`](https://gillescolling.com/BORG/reference/borg.md) can
auto-detect them.

## Details

This bridges the standard SDM workflow (rasters + points) with BORG's
data.frame interface. The returned data frame can be passed directly to
[`borg()`](https://gillescolling.com/BORG/reference/borg.md),
[`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md), or
[`borg_diagnose()`](https://gillescolling.com/BORG/reference/borg_diagnose.md).

Requires the terra package. If `points` is an `sf` object, the sf
package is also required.

The function performs the equivalent of:


    env_data <- terra::extract(raster, points, ID = FALSE)
    coords <- terra::crds(points)
    cbind(coords, env_data)

but handles edge cases (NA removal, coordinate injection, CRS checks)
and attaches metadata so downstream BORG functions can auto-detect
spatial structure.

## Examples

``` r
# \donttest{
if (requireNamespace("terra", quietly = TRUE)) {
  # Create example raster and points
  r <- terra::rast(nrows = 50, ncols = 50,
                   xmin = 0, xmax = 100, ymin = 0, ymax = 100)
  terra::values(r) <- runif(terra::ncell(r))
  names(r) <- "bio1"

  pts <- terra::vect(
    cbind(x = runif(100, 0, 100), y = runif(100, 0, 100)),
    crs = terra::crs(r)
  )

  # Extract and run BORG
  d <- borg_extract(r, pts)
  result <- borg(d, coords = c("x", "y"), target = "bio1")
}
# }
```
