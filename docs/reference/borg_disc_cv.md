# Leave-Disc-Out Cross-Validation

Spatial cross-validation with circular exclusion buffers around test
points. For each fold, all training points within a specified radius of
any test point are removed, creating a spatial gap that eliminates
autocorrelation leakage. More principled than rectangular or k-means
blocking for point data.

## Usage

``` r
borg_disc_cv(
  data,
  coords,
  target = NULL,
  radius = NULL,
  v = 5,
  min_train = 0.5,
  seed = 42,
  output = c("list", "rsample"),
  verbose = FALSE
)
```

## Arguments

- data:

  A data frame with coordinate and predictor columns.

- coords:

  Character vector of length 2. Coordinate column names.

- target:

  Character. Target variable name (for diagnostics).

- radius:

  Numeric. Exclusion buffer radius in coordinate units. If `NULL`,
  automatically set to the autocorrelation range estimated by
  [`borg_diagnose()`](https://gillescolling.com/BORG/reference/borg_diagnose.md).

- v:

  Integer. Number of folds. Default: 5.

- min_train:

  Numeric. Minimum fraction of data that must remain in training after
  exclusion. Folds that violate this are dropped. Default: 0.5.

- seed:

  Integer. Random seed. Default: 42.

- output:

  Character. Output format: `"list"` (default), `"rsample"`.

- verbose:

  Logical. Print diagnostic messages. Default: FALSE.

## Value

A list with class `"borg_disc_cv"` containing:

- folds:

  List of train/test index vectors

- radius:

  Exclusion radius used

- n_excluded:

  Number of training points excluded per fold (in the buffer zone)

- effective_training:

  Fraction of data available for training per fold (after exclusion)

- strategy:

  "leave_disc_out"

- params:

  List of parameters used

Compatible with other BORG functions that accept fold lists.

## Details

### How it works

1.  Partition data into `v` spatial folds using k-means on coordinates
    (same as `borg_cv`).

2.  For each fold, identify the test points.

3.  Remove from training all points within `radius` of any test point.
    These excluded points are neither train nor test — they form a
    buffer zone.

4.  If the remaining training set is too small (`< min_train * n`), the
    fold is dropped with a warning.

### Choosing the radius

The radius should match the autocorrelation range of the spatial
process.
[`borg_diagnose()`](https://gillescolling.com/BORG/reference/borg_diagnose.md)
estimates this via variogram analysis. Setting `radius = NULL` uses this
estimate automatically.

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(200, 0, 100), y = runif(200, 0, 100))
d$z <- sin(d$x / 10) + rnorm(200, sd = 0.5)
cv <- borg_disc_cv(d, coords = c("x", "y"), target = "z", radius = 15)
cv
```
