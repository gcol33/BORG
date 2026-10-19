# Forward Feature Selection with Spatial/Blocked CV

Selects variables using blocked cross-validation instead of random CV,
avoiding overfitting to spatial/temporal structure. At each step, adds
the variable that most improves the CV metric.

## Usage

``` r
borg_forward_selection(
  data,
  target,
  predictors = NULL,
  folds = NULL,
  coords = NULL,
  time = NULL,
  groups = NULL,
  metric = c("rmse", "mae", "rsq"),
  fit_fun = stats::lm,
  min_vars = 1L,
  verbose = FALSE
)
```

## Arguments

- data:

  Data frame.

- target:

  Character. Response variable name.

- predictors:

  Character vector. Candidate predictor names. If `NULL`, uses all
  numeric columns except target and coords.

- folds:

  A `borg_cv` object or fold list. If `NULL`, generated automatically
  via
  [`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md).

- coords:

  Character vector of coordinate columns (for auto CV).

- time:

  Character. Time column (for auto CV).

- groups:

  Character. Group column (for auto CV).

- metric:

  Character. `"rmse"` (default), `"mae"`, or `"rsq"`.

- fit_fun:

  Function. Model fitting function. Default: `lm`.

- min_vars:

  Integer. Minimum variables to include. Default: 1.

- verbose:

  Logical. Default: FALSE.

## Value

A list with class `"borg_ffs"` containing:

- selected:

  Character vector of selected variable names (in order)

- history:

  Data frame: step, variable_added, metric_value, n_vars

- best_metric:

  Best CV metric achieved

- all_vars:

  All candidate variables

## Details

Similar to CAST::ffs() but uses BORG's CV infrastructure and supports
any dependency structure (spatial, temporal, grouped).

## Examples

``` r
set.seed(42)
d <- data.frame(
  x = runif(100), y = runif(100),
  important = rnorm(100), noise1 = rnorm(100), noise2 = rnorm(100)
)
d$z <- d$important * 2 + rnorm(100, sd = 0.5)
ffs <- borg_forward_selection(d, target = "z",
                                predictors = c("important", "noise1", "noise2"),
                                coords = c("x", "y"))
ffs$selected
```
