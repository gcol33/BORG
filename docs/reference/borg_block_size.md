# Optimize Spatial Block Size

Tests multiple block sizes and selects the one that minimizes residual
spatial autocorrelation between CV folds. For each candidate size,
generates spatial block folds and computes the mean Moran's I of
residuals within test sets.

## Usage

``` r
borg_block_size(
  data,
  coords,
  target,
  v = 5,
  n_sizes = 10,
  range = NULL,
  formula = NULL,
  crs = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  Data frame with coordinate and target columns.

- coords:

  Character vector of length 2. Coordinate column names.

- target:

  Character. Target variable name.

- v:

  Integer. Number of folds. Default: 5.

- n_sizes:

  Integer. Number of candidate block sizes to test. Default: 10.

- range:

  Numeric vector of length 2. Min and max block sizes to test. If
  `NULL`, automatically determined from the variogram range estimate
  (0.5x to 3x range).

- formula:

  Optional model formula for computing residual autocorrelation. If
  `NULL`, uses the raw target values.

- crs:

  Optional coordinate reference system (an `sf` CRS, EPSG code, or PROJ
  string). When it is geographic (lon/lat), distances use the Haversine
  formula; otherwise planar Euclidean.

- verbose:

  Logical. Default: FALSE.

## Value

A list with class `"borg_block_opt"` containing:

- optimal:

  Optimal block size

- results:

  Data frame with columns: block_size, mean_morans_i, mean_test_size,
  n_empty_folds

- range_estimate:

  Variogram-based range estimate

Has an `autoplot()` method showing the optimization curve.

## Examples

``` r
set.seed(42)
d <- data.frame(
  x = runif(200, 0, 100), y = runif(200, 0, 100),
  z = rnorm(200)
)
opt <- borg_block_size(d, coords = c("x", "y"), target = "z")
opt$optimal
```
