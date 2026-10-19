# Quick Leakage Check (Pipe-Friendly)

Single-verb entry point that runs the full BORG pipeline: diagnose
dependencies, generate valid CV, validate the split, and return a tidy
summary. Designed for use in pipelines.

## Usage

``` r
borg_check(
  data,
  model = NULL,
  target = NULL,
  coords = NULL,
  time = NULL,
  groups = NULL,
  train_idx = NULL,
  test_idx = NULL,
  v = 5L,
  verbose = FALSE
)
```

## Arguments

- data:

  A data frame.

- model:

  A fitted model object (optional). If provided, predictions are
  evaluated under blocked CV.

- target:

  Character. Response variable name.

- coords:

  Character vector of length 2. Coordinate column names (optional, for
  spatial data).

- time:

  Character. Time column name (optional, for temporal data).

- groups:

  Character. Grouping column name (optional).

- train_idx:

  Integer vector. Training indices (optional, for validating an existing
  split).

- test_idx:

  Integer vector. Test indices (optional).

- v:

  Integer. Number of CV folds. Default: 5.

- verbose:

  Logical. Default: FALSE.

## Value

A data frame with class `"borg_check"` containing one row per detected
risk (or zero rows if clean), with columns:

- risk_type:

  Character. Type of risk detected.

- severity:

  Character. `"hard_inflation"`, `"soft_inflation"`, or `"info"`.

- description:

  Character. Plain-language description.

- n_affected:

  Integer. Number of observations affected.

- source:

  Character. Object/step that triggered the risk.

Also has attributes: `diagnosis`, `cv`, `risks` (the full objects for
further inspection).

## Details

`borg_check()` is intentionally simple: one call, one result. For finer
control, use
[`borg_diagnose()`](https://gillescolling.com/BORG/reference/borg_diagnose.md),
[`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md), and
[`borg_validate()`](https://gillescolling.com/BORG/reference/borg_validate.md)
individually.

## See also

[`borg`](https://gillescolling.com/BORG/reference/borg.md),
[`borg_diagnose`](https://gillescolling.com/BORG/reference/borg_diagnose.md),
[`borg_explain_risk`](https://gillescolling.com/BORG/reference/borg_explain_risk.md)

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))

# Pipe-friendly
result <- borg_check(d, target = "z", coords = c("x", "y"))
result
nrow(result)  # 0 if clean

# Check an existing split
result <- borg_check(d, target = "z", coords = c("x", "y"),
                     train_idx = 1:70, test_idx = 71:100)
```
