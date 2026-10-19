# Block Bootstrap Confidence Intervals for CV Metrics

Estimates confidence intervals for cross-validation performance metrics
using spatial block bootstrap. Standard bootstrap assumes independent
observations; block bootstrap preserves the dependency structure
detected by
[`borg_diagnose()`](https://gillescolling.com/BORG/reference/borg_diagnose.md).

## Usage

``` r
borg_bootstrap(
  model,
  data,
  target,
  coords = NULL,
  formula = NULL,
  fit_fun = NULL,
  folds = NULL,
  metric = c("rmse", "mae", "rsq"),
  n_boot = 200,
  n_blocks = 10,
  conf_level = 0.95,
  seed = 42
)
```

## Arguments

- model:

  A fitted model with a
  [`predict()`](https://rdrr.io/r/stats/predict.html) method, or a
  model-fitting function (see `fit_fun`).

- data:

  Data frame with predictors and target.

- target:

  Character. Target variable name.

- coords:

  Character vector of length 2. Coordinate column names. If provided,
  uses spatial blocking. Otherwise, uses random blocks.

- formula:

  Model formula. Required if `model` is a function.

- fit_fun:

  Function. Model fitting function. If `NULL`, attempts to refit from
  model's call.

- folds:

  A `borg_cv` object or fold list. If provided, bootstrap resamples
  within the fold structure.

- metric:

  Character. Performance metric: `"rmse"` (default), `"mae"`, `"rsq"`.

- n_boot:

  Integer. Number of bootstrap replicates. Default: 200.

- n_blocks:

  Integer. Number of spatial blocks for resampling. Default: 10.

- conf_level:

  Numeric. Confidence level. Default: 0.95.

- seed:

  Integer. Random seed. Default: 42.

## Value

A list with class `"borg_bootstrap"` containing:

- estimate:

  Point estimate of the metric

- ci_lower:

  Lower confidence bound

- ci_upper:

  Upper confidence bound

- conf_level:

  Confidence level

- boot_distribution:

  Numeric vector of bootstrap estimates

- se:

  Bootstrap standard error

- bias:

  Bootstrap bias estimate

- metric:

  Metric name

- n_boot:

  Number of bootstrap replicates

- method:

  "spatial_block" or "random_block"

Has [`print()`](https://rdrr.io/r/base/print.html) and `autoplot()`
methods.

## Details

### Spatial block bootstrap

Data is partitioned into spatial blocks using k-means clustering on
coordinates. Each bootstrap replicate resamples blocks (with
replacement), then includes all observations from selected blocks. This
preserves within-block spatial correlation while generating valid
resamples.

### CI methods

Returns bias-corrected and accelerated (BCa) intervals when possible,
falling back to percentile intervals.

## References

Lahiri, S. N. (2003). *Resampling Methods for Dependent Data*. Springer.

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(150), y = runif(150), a = rnorm(150))
d$z <- 2 * d$a + rnorm(150, sd = 0.5)
model <- lm(z ~ a, data = d)
boot <- borg_bootstrap(model, d, target = "z", coords = c("x", "y"),
                         n_boot = 50)
boot
```
