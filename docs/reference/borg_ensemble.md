# Ensemble Predictions from CV Fold Models

Combines predictions from models fitted on each CV fold into a weighted
ensemble. Each fold's model predicts on the full dataset (or new data),
and predictions are averaged with optional performance-based weighting.

## Usage

``` r
borg_ensemble(
  data,
  folds,
  formula,
  newdata = NULL,
  fit_fun = stats::lm,
  weight_by = c("equal", "performance"),
  metric = c("rmse", "mae")
)
```

## Arguments

- data:

  Data frame used for training.

- folds:

  A `borg_cv` object or fold list.

- formula:

  Model formula.

- newdata:

  Optional data frame for prediction. If `NULL`, predicts on `data`.

- fit_fun:

  Function. Default: `lm`.

- weight_by:

  Character. Weighting scheme: `"equal"` (default) or `"performance"`
  (inverse error weight).

- metric:

  Character. Metric for performance weighting. Default: `"rmse"`.

## Value

A list with class `"borg_ensemble"` containing:

- prediction:

  Numeric vector of ensemble predictions

- uncertainty:

  Per-observation SD across fold predictions

- weights:

  Fold weights used

- n_models:

  Number of contributing models

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
cv <- borg_cv(d, coords = c("x", "y"), target = "z")
ens <- borg_ensemble(d, cv, z ~ x + y)
cor(d$z, ens$prediction)
```
