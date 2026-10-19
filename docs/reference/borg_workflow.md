# Create a BORG Validation Workflow

Wraps the full model evaluation pipeline into a single trackable object:
diagnose data dependencies, generate appropriate CV folds, fit a model
per fold, and validate each split for leakage.

## Usage

``` r
borg_workflow(
  data,
  formula,
  coords = NULL,
  time = NULL,
  groups = NULL,
  v = 5,
  fit_fun = stats::lm,
  metric = c("rmse", "mae", "rsq"),
  buffer = NULL,
  parallel = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- data:

  Data frame with predictor and target columns.

- formula:

  Model formula (e.g. `y ~ x1 + x2`).

- coords:

  Character vector of coordinate column names.

- time:

  Character. Time column name.

- groups:

  Character. Group column name.

- v:

  Integer. Number of folds. Default: 5.

- fit_fun:

  Function to fit a model. Default: `lm`. Must accept `formula` and
  `data` and return an object with
  [`predict()`](https://rdrr.io/r/stats/predict.html).

- metric:

  Character. Performance metric. Default: `"rmse"`.

- buffer:

  Numeric. Optional spatial buffer.

- parallel:

  Logical. If `TRUE` and future.apply is installed, fold fitting and
  evaluation run in parallel. Default: `FALSE`.

- verbose:

  Logical. Default: FALSE.

- ...:

  Additional arguments passed to
  [`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md).

## Value

A `borg_workflow` object (list) containing:

- diagnosis:

  BorgDiagnosis object

- cv:

  borg_cv object with folds

- models:

  List of fitted models (one per fold)

- predictions:

  List of prediction vectors (one per fold)

- performance:

  borg_fold_perf data frame

- risks:

  List of BorgRisk objects (one per fold)

- data:

  Original data

- formula:

  Model formula

- params:

  Workflow parameters

## Examples

``` r
set.seed(42)
d <- data.frame(
  x = runif(100, 0, 100), y = runif(100, 0, 100),
  z = rnorm(100)
)
wf <- borg_workflow(d, z ~ x + y, coords = c("x", "y"))
wf
```
