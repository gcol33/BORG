# Prediction Stability Map

Creates a continuous spatial map of prediction variance across
cross-validation folds. Unlike
[`borg_stability()`](https://gillescolling.com/BORG/reference/borg_stability.md)
which returns summary statistics, this function produces per-location
stability scores showing where predictions are consistent versus
volatile across different train/test partitions.

## Usage

``` r
borg_stability_map(
  model,
  data,
  new = NULL,
  target,
  coords,
  formula = NULL,
  fit_fun = NULL,
  folds = NULL,
  v = 10,
  seed = 42
)
```

## Arguments

- model:

  A fitted model with a
  [`predict()`](https://rdrr.io/r/stats/predict.html) method, or a
  model-fitting function.

- data:

  Data frame with predictors, target, and coordinates.

- new:

  Data frame of prediction locations. If `NULL`, uses `data`.

- target:

  Character. Target variable name.

- coords:

  Character vector of length 2. Coordinate column names.

- formula:

  Model formula. Required if `model` is a function.

- fit_fun:

  Function. Model fitting function. If `NULL`, attempts to refit from
  model's call.

- folds:

  A `borg_cv` or `borg_disc_cv` object, or a fold list. If `NULL`,
  generates spatial CV folds automatically.

- v:

  Integer. Number of folds if generating automatically. Default: 10.

- seed:

  Integer. Random seed. Default: 42.

## Value

A data frame with class `"borg_stability_map"` containing:

- pred_mean:

  Mean prediction across folds

- pred_sd:

  Standard deviation of predictions across folds

- pred_cv:

  Coefficient of variation (\|sd/mean\|)

- pred_range:

  Range of predictions (max - min)

- n_folds_predicted:

  Number of folds where this point was in the test set (or could be
  predicted)

- x, y:

  Coordinates

Has [`print()`](https://rdrr.io/r/base/print.html) and `autoplot()`
methods.

## Details

### How it works

For each CV fold:

1.  Refit the model on the training set.

2.  Predict on all locations (not just test set).

3.  Store per-location predictions.

Then compute per-location statistics across folds. Locations with high
`pred_sd` are unstable — the prediction changes substantially depending
on which data is included in training.

### Interpretation

High instability at a location can indicate:

- The location is in a data-sparse region (near AOA boundary)

- The model is overfitting to nearby training points

- The underlying relationship changes spatially (non-stationarity)

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(100, 0, 50), y = runif(100, 0, 50))
d$z <- sin(d$x / 5) + rnorm(100, sd = 0.5)
model <- lm(z ~ x + y, data = d)
sm <- borg_stability_map(model, d, target = "z", coords = c("x", "y"),
                           v = 5)
sm
```
