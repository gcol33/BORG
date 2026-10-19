# Global (Pooled) Cross-Validation Metrics

Pools all held-out predictions across folds and computes a single
performance metric, avoiding bias from unequal fold sizes.

## Usage

``` r
borg_global_validation(
  data,
  folds,
  formula,
  metric = c("rmse", "mae", "rsq"),
  fit_fun = stats::lm
)
```

## Arguments

- data:

  Data frame.

- folds:

  A `borg_cv` object or fold list.

- formula:

  Model formula.

- metric:

  Character. `"rmse"`, `"mae"`, or `"rsq"`.

- fit_fun:

  Function. Default: `lm`.

## Value

A list with class `"borg_global_validation"` containing the pooled
metric and per-fold metrics for comparison.
