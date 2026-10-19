# Evaluate Per-Fold Model Performance

Fits a model on each fold's training set and evaluates on its test set,
returning per-fold metrics with spatial centroids for geographic
performance mapping.

## Usage

``` r
borg_fold_performance(
  data,
  folds,
  formula,
  coords = NULL,
  metric = c("rmse", "mae", "rsq", "auc", "tss", "kappa", "sensitivity", "specificity",
    "accuracy"),
  fit_fun = stats::lm,
  parallel = FALSE
)
```

## Arguments

- data:

  Data frame with predictor and target columns.

- folds:

  A `borg_cv` object, or a list of fold lists (each with `$train` and
  `$test` index vectors).

- formula:

  A model formula (e.g. `y ~ x1 + x2`).

- coords:

  Character vector of length 2. Coordinate column names for computing
  fold centroids. Optional.

- metric:

  Character. Performance metric: `"rmse"` (default), `"mae"`, or
  `"rsq"`.

- fit_fun:

  Function to fit a model. Default: `lm`. Must accept `formula` and
  `data` arguments and return an object with a
  [`predict()`](https://rdrr.io/r/stats/predict.html) method.

- parallel:

  Logical. If `TRUE` and the future.apply package is installed, fold
  evaluation runs in parallel. Default: `FALSE`.

## Value

A data frame with columns:

- fold:

  Fold index

- metric:

  Metric name

- value:

  Metric value

- n_train:

  Training set size

- n_test:

  Test set size

- centroid_x, centroid_y:

  Spatial centroid of test set (if coords provided)

Has class `"borg_fold_perf"` with an `autoplot()` method.

## Examples

``` r
set.seed(42)
d <- data.frame(
  x = runif(200, 0, 100), y = runif(200, 0, 100),
  z = rnorm(200)
)
cv <- borg_cv(d, coords = c("x", "y"), target = "z")
perf <- borg_fold_performance(d, cv, z ~ x + y, coords = c("x", "y"))
perf
```
