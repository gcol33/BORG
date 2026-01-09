# Generate Valid Cross-Validation Scheme

Creates cross-validation folds that respect data dependency structure.
When spatial, temporal, or clustered dependencies are detected, random
CV is disabled and appropriate blocking strategies are enforced.

## Usage

``` r
borg_cv(
  data,
  diagnosis = NULL,
  v = 5,
  coords = NULL,
  time = NULL,
  groups = NULL,
  target = NULL,
  block_size = NULL,
  embargo = NULL,
  output = c("list", "rsample", "caret", "mlr3"),
  allow_random = FALSE,
  verbose = FALSE
)
```

## Arguments

- data:

  A data frame to create CV folds for.

- diagnosis:

  A
  [`BorgDiagnosis`](https://gillescolling.com/BORG/reference/BorgDiagnosis.md)
  object from
  [`borg_diagnose`](https://gillescolling.com/BORG/reference/borg_diagnose.md).
  If NULL, diagnosis is performed automatically.

- v:

  Integer. Number of folds. Default: 5.

- coords:

  Character vector of length 2 specifying coordinate column names.
  Required for spatial blocking if diagnosis is NULL.

- time:

  Character string specifying the time column name. Required for
  temporal blocking if diagnosis is NULL.

- groups:

  Character string specifying the grouping column name. Required for
  group CV if diagnosis is NULL.

- target:

  Character string specifying the response variable column name.

- block_size:

  Numeric. For spatial blocking, the minimum block size. If NULL,
  automatically determined from diagnosis. Should be larger than the
  autocorrelation range.

- embargo:

  Integer. For temporal blocking, minimum gap between train and test. If
  NULL, automatically determined from diagnosis.

- output:

  Character. Output format: "list" (default), "rsample", "caret",
  "mlr3".

- allow_random:

  Logical. If TRUE, allows random CV even when dependencies detected.
  Default: FALSE. Setting to TRUE requires explicit acknowledgment.

- verbose:

  Logical. If TRUE, print diagnostic messages. Default: FALSE.

## Value

Depending on `output`:

- "list":

  A list with elements: `folds` (list of train/test index vectors),
  `diagnosis` (the BorgDiagnosis used), `strategy` (CV strategy name),
  `params` (parameters used).

- "rsample":

  An `rsample` `rset` object compatible with tidymodels.

- "caret":

  A `trainControl` object for caret.

- "mlr3":

  An `mlr3` `Resampling` object.

## Details

### The Enforcement Principle

Unlike traditional CV helpers, `borg_cv` enforces valid evaluation:

- If spatial autocorrelation is detected, **random CV is disabled**

- If temporal autocorrelation is detected, **random CV is disabled**

- If clustered structure is detected, **random CV is disabled**

- To use random CV on dependent data, you must set `allow_random = TRUE`
  and provide justification (this is logged).

### Spatial Blocking

When spatial dependencies are detected, data are partitioned into
spatial blocks using k-means clustering on coordinates. Block size is
set to exceed the estimated autocorrelation range. This ensures train
and test sets are spatially separated.

### Temporal Blocking

When temporal dependencies are detected, data are split chronologically
with an embargo period between train and test sets. This prevents
information from future observations leaking into training.

### Group CV

When clustered structure is detected, entire groups (clusters) are held
out together. No group appears in both train and test within a fold.

## See also

[`borg_diagnose`](https://gillescolling.com/BORG/reference/borg_diagnose.md),
[`BorgDiagnosis`](https://gillescolling.com/BORG/reference/BorgDiagnosis.md)

## Examples

``` r
# Spatial data with autocorrelation
set.seed(42)
spatial_data <- data.frame(
  x = runif(200, 0, 100),
  y = runif(200, 0, 100),
  response = rnorm(200)
)

# Diagnose and create CV
cv <- borg_cv(spatial_data, coords = c("x", "y"), target = "response")
str(cv$folds)  # List of train/test indices

# Clustered data
clustered_data <- data.frame(
  site = rep(1:20, each = 10),
  value = rep(rnorm(20, sd = 2), each = 10) + rnorm(200, sd = 0.5)
)

cv <- borg_cv(clustered_data, groups = "site", target = "value")
cv$strategy  # "group_fold"

# Get rsample-compatible output for tidymodels
if (FALSE) { # \dontrun{
cv_rsample <- borg_cv(spatial_data, coords = c("x", "y"), output = "rsample")
} # }
```
