# Spatial Prediction Uncertainty Map

Collects predictions from all CV folds at each test location, computes
per-location mean and standard deviation, and provides a spatial
uncertainty map.

## Usage

``` r
borg_prediction_map(data, folds, formula, coords, fit_fun = stats::lm)
```

## Arguments

- data:

  Data frame with predictor and coordinate columns.

- folds:

  A `borg_cv` object or fold list.

- formula:

  Model formula.

- coords:

  Character vector of length 2. Coordinate column names.

- fit_fun:

  Function. Default: `lm`.

## Value

A data frame with class `"borg_pred_map"` containing:

- x, y:

  Coordinates

- pred_mean:

  Mean prediction across folds where this obs was in test set

- pred_sd:

  SD of predictions (uncertainty)

- actual:

  Observed target value

- residual_mean:

  Mean residual

- n_folds_tested:

  Number of folds where this obs appeared in test set

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
cv <- borg_cv(d, coords = c("x", "y"), target = "z")
pm <- borg_prediction_map(d, cv, z ~ x + y, coords = c("x", "y"))
head(pm)
```
