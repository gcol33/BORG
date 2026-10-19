# Spatially Thin Occurrence Data

Reduces spatial clustering by ensuring a minimum distance between all
retained observations. Uses iterative nearest-neighbor removal: at each
step, the point with the smallest nearest-neighbor distance is removed
until all pairwise distances exceed `min_dist`.

## Usage

``` r
borg_thin(data, coords = NULL, min_dist = NULL, verbose = FALSE)
```

## Arguments

- data:

  A data frame, `sf` object, or
  [`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html).

- coords:

  Character vector of length 2. Coordinate column names. Required for
  data frames; ignored for sf/SpatVector.

- min_dist:

  Numeric. Minimum distance between retained points. Units match the
  coordinate system (degrees for lat/lon, meters for projected CRS). If
  `NULL`, uses 10 percent of the nearest-neighbor interquartile range as
  a default.

- verbose:

  Logical. Print thinning progress. Default: FALSE.

## Value

The input data filtered to retained rows. Has attribute `"thinned_idx"`
with the original row indices of kept observations, and `"n_removed"`
with the count of removed points.

## Details

Spatial thinning is standard practice in species distribution modelling
to reduce sampling bias. Clustered occurrences inflate apparent model
performance and bias spatial CV fold sizes.

For geographic coordinates (lat/lon), distances are computed using the
Haversine formula (meters). For projected coordinates, Euclidean
distance is used.

## Examples

``` r
# Thin clustered points to 5-unit minimum spacing
set.seed(42)
d <- data.frame(
  x = c(rnorm(50, 0, 1), rnorm(50, 10, 1)),
  y = c(rnorm(50, 0, 1), rnorm(50, 10, 1)),
  species = "A"
)
d_thin <- borg_thin(d, coords = c("x", "y"), min_dist = 1)
nrow(d_thin)  # fewer rows
```
