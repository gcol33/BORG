# Spatial Block Cross-Validation (rsample-compatible)

Creates spatial block CV folds that plug directly into
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
and
[`tune::fit_resamples()`](https://tune.tidymodels.org/reference/fit_resamples.html).

## Usage

``` r
borg_spatial_cv(
  data,
  coords,
  target = NULL,
  v = 5,
  buffer = NULL,
  repeats = 1L,
  ...
)
```

## Arguments

- data:

  A data frame, `sf` object, or
  [`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html).

- coords:

  Character vector of length 2. Coordinate column names.

- target:

  Character. Target variable for autocorrelation diagnosis.

- v:

  Integer. Number of folds. Default: 5.

- buffer:

  Numeric. Optional spatial buffer distance for exclusion.

- repeats:

  Integer. Number of repeated fold sets. Default: 1.

- ...:

  Additional arguments passed to
  [`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md).

## Value

A `borg_rset` object (subclass of `rset`) compatible with tidymodels.

## Examples

``` r
# \donttest{
if (requireNamespace("rsample", quietly = TRUE)) {
  set.seed(42)
  d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
  folds <- borg_spatial_cv(d, coords = c("x", "y"), target = "z")
}
# }
```
