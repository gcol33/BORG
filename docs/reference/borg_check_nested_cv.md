# Check Nested CV for Leakage

Validates a nested cross-validation setup for data leakage between outer
and inner CV loops, and detects strategy mismatches where the inner CV
uses random resampling despite data dependencies.

## Usage

``` r
borg_check_nested_cv(
  inner_resamples,
  outer_train_idx,
  outer_test_idx,
  data,
  coords = NULL,
  time = NULL,
  groups = NULL
)
```

## Arguments

- inner_resamples:

  The inner resampling object. One of:

  - A [`caret::train`](https://rdrr.io/pkg/caret/man/train.html) object

  - A `tune_results` object from tidymodels

  - A character string naming the inner CV method (e.g. "cv",
    "vfold_cv")

- outer_train_idx:

  Integer vector. Indices of the outer training set.

- outer_test_idx:

  Integer vector. Indices of the outer test set.

- data:

  Data frame used for modeling.

- coords:

  Character vector of coordinate column names (for spatial check).

- time:

  Character. Time column name (for temporal check).

- groups:

  Character. Group column name (for clustered check).

## Value

A [`BorgRisk`](https://gillescolling.com/BORG/reference/BorgRisk.md)
object with any detected risks.

## Examples

``` r
# Check if inner random CV is appropriate given grouped data
d <- data.frame(
  site = rep(1:20, each = 10),
  x = rnorm(200),
  y = rep(rnorm(20), each = 10) + rnorm(200, sd = 0.5)
)
result <- borg_check_nested_cv(
  inner_resamples = "cv",
  outer_train_idx = 1:160,
  outer_test_idx = 161:200,
  data = d,
  groups = "site"
)
```
