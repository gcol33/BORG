# Guard Model Evaluation Against Information Reuse

`borg_guard()` wraps an evaluation workflow and enforces valid data
handling. It intercepts operations that would leak information from test
to training data and either blocks them (hard violations) or rewrites
them (soft violations).

## Usage

``` r
borg_guard(
  data,
  train_idx,
  test_idx,
  mode = c("strict", "warn", "rewrite"),
  temporal_col = NULL,
  spatial_cols = NULL,
  group_col = NULL
)
```

## Arguments

- data:

  A data.frame containing the full dataset.

- train_idx:

  Integer vector of training row indices.

- test_idx:

  Integer vector of test row indices.

- mode:

  Character string specifying enforcement mode:

  - `"strict"`: Block all violations, refuse to proceed

  - `"warn"`: Warn on violations but continue

  - `"rewrite"`: Automatically fix violations where possible

- temporal_col:

  Optional character string naming a timestamp column. If provided,
  enables temporal ordering validation.

- spatial_cols:

  Optional character vector naming coordinate columns (e.g.,
  `c("longitude", "latitude")`). If provided, enables spatial
  autocorrelation checks.

- group_col:

  Optional character string naming a grouping column. If provided,
  enables group-level isolation checks.

## Value

A `borg_context` object that can be used to wrap preprocessing and
evaluation calls. This object tracks all operations and validates them
against the train/test split.

## Details

`borg_guard()` creates a guarded context for model evaluation. Within
this context, BORG:

1.  Validates the initial train/test split

2.  Monitors preprocessing operations for data leakage

3.  Enforces temporal ordering (if `temporal_col` specified)

4.  Validates spatial block separation (if `spatial_cols` specified)

5.  Ensures group isolation (if `group_col` specified)

## Enforcement Modes

- strict:

  Most conservative. Any detected violation causes an immediate error.
  Use for production pipelines where validity is critical.

- warn:

  Permissive mode. Violations generate warnings but evaluation proceeds.
  Use for exploratory analysis or legacy code auditing.

- rewrite:

  Automatic correction mode. BORG attempts to fix violations (e.g.,
  refitting preprocessing on train-only). Use when migrating existing
  pipelines.

## See also

[`borg_inspect`](https://gillescolling.com/BORG/reference/borg_inspect.md)
for object-level inspection,
[`borg_validate`](https://gillescolling.com/BORG/reference/borg_validate.md)
for post-hoc workflow validation.

## Examples

``` r
# The canonical failure: preprocessing before splitting
# This workflow produces inflated performance estimates:
#
#   data_scaled <- scale(full_data)
#   train <- data_scaled[1:800, ]
#   test <- data_scaled[801:1000, ]
#
# BORG blocks this pattern.

# Valid split (no overlap)
data <- data.frame(x = rnorm(100), y = rnorm(100))
ctx <- borg_guard(data, train_idx = 1:70, test_idx = 71:100)
print(ctx)

# Invalid split (overlap) - errors immediately
if (FALSE) { # \dontrun{
ctx <- borg_guard(data, train_idx = 1:60, test_idx = 50:100)
# Error: BORG HARD VIOLATION: train_idx and test_idx overlap
} # }

# Grouped data - ensures no group appears in both splits
data$patient <- rep(1:10, each = 10)
ctx <- borg_guard(
  data,
  train_idx = 1:50,
  test_idx = 51:100,
  group_col = "patient"
)

# Temporal data - validates chronological ordering
data$date <- seq.Date(as.Date("2020-01-01"), by = "day", length.out = 100)
ctx <- borg_guard(
  data,
  train_idx = 1:70,
  test_idx = 71:100,
  temporal_col = "date"
)
```
