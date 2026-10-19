# Calibrate Dissimilarity Index to Prediction Error

Models the relationship between the Dissimilarity Index (DI) and
prediction error using isotonic regression. Enables pixel-level
uncertainty estimation: for any new prediction point, its DI can be
mapped to an expected error.

## Usage

``` r
borg_error_profile(
  data,
  folds,
  formula,
  predictors = NULL,
  metric = c("rmse", "mae"),
  fit_fun = stats::lm,
  n_bins = 10
)
```

## Arguments

- data:

  Data frame of training data.

- folds:

  A `borg_cv` object or fold list.

- formula:

  Model formula.

- predictors:

  Character vector. Predictor columns for DI computation. If `NULL`,
  auto-detected.

- metric:

  Character. `"rmse"` or `"mae"`.

- fit_fun:

  Function. Default: `lm`.

- n_bins:

  Integer. Number of DI bins. Default: 10.

## Value

A list with class `"borg_error_profile"` containing:

- profile:

  Data frame: di_bin, mean_di, mean_error, n_obs

- raw:

  Data frame: di, error (per observation)

- iso_fit:

  Isotonic regression fit for predicting error from DI

Has an `autoplot()` method and a
[`predict()`](https://rdrr.io/r/stats/predict.html) method.

## References

Meyer, H., & Pebesma, E. (2021). Predicting into unknown space?
Estimating the area of applicability of spatial prediction models.
*Methods in Ecology and Evolution*, 12(9), 1620-1633.
[doi:10.1111/2041-210X.13650](https://doi.org/10.1111/2041-210X.13650)

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(150, 0, 100), y = runif(150, 0, 100),
                 a = rnorm(150), b = rnorm(150))
d$z <- d$a * 2 + rnorm(150, sd = 0.5)
cv <- borg_cv(d, coords = c("x", "y"), target = "z")
ep <- borg_error_profile(d, cv, z ~ a + b, predictors = c("a", "b"))
ep
```
