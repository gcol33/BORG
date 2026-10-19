# Best Subset Variable Selection with Blocked CV

Evaluates all 2^p combinations of predictor variables using blocked CV.
Exhaustive search — only feasible for small p (\< 15).

## Usage

``` r
borg_best_subset(
  data,
  target,
  predictors,
  folds,
  metric = c("rmse", "mae", "rsq"),
  fit_fun = stats::lm,
  max_vars = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  Data frame.

- target:

  Character. Response variable.

- predictors:

  Character vector. Candidate predictors.

- folds:

  A `borg_cv` object or fold list.

- metric:

  Character. Default: `"rmse"`.

- fit_fun:

  Function. Default: `lm`.

- max_vars:

  Integer. Maximum variables in a subset. Default: all.

- verbose:

  Logical. Default: FALSE.

## Value

A data frame with class `"borg_bss"`: variables, n_vars, metric_value,
rank.
