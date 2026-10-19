# Analyze CV Fold Stability Across Repeats

Quantifies how much CV results and fold assignments change across
repeated runs. Uses a `borg_cv` object with `repeats > 1`.

## Usage

``` r
borg_stability(
  object,
  data,
  formula,
  metric = c("rmse", "mae", "rsq"),
  fit_fun = stats::lm
)
```

## Arguments

- object:

  A `borg_cv` object with `repeats > 1` (i.e., `object$all_folds` is not
  NULL).

- data:

  Data frame for computing per-repeat performance.

- formula:

  Model formula.

- metric:

  Character. `"rmse"` (default), `"mae"`, or `"rsq"`.

- fit_fun:

  Function. Model fitting function. Default: `lm`.

## Value

A list with class `"borg_stability"` containing:

- metric_stability:

  Data frame: repeat, mean_metric, sd_metric

- metric_cv:

  Coefficient of variation of mean metric across repeats

- assignment_stability:

  Mean Jaccard similarity of test sets between repeats

- summary:

  Character assessment: "stable", "moderate", or "unstable"

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
cv <- borg_cv(d, coords = c("x", "y"), target = "z", repeats = 5)
stab <- borg_stability(cv, d, z ~ x + y)
stab
```
