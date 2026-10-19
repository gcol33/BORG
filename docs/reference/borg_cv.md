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
  env = NULL,
  dist_mat = NULL,
  prediction_points = NULL,
  block_size = NULL,
  embargo = NULL,
  buffer = NULL,
  strategy = NULL,
  repeats = 1L,
  seed = NULL,
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

- env:

  Environmental covariates for environmental blocking. A `SpatRaster`,
  data frame, or matrix with one row per observation. Required when
  `strategy = "environmental_block"` or `"spatial_plus"`.

- dist_mat:

  A distance matrix or `dist` object for custom blocking. Must be an
  `n x n` matrix matching `nrow(data)`. Required when
  `strategy = "custom_block"`.

- prediction_points:

  Data frame, matrix, `SpatVector`, or `sf` object with coordinates of
  prediction locations. Required when `strategy = "knndm"` or `"nndm"`,
  which match cross-validation fold geometry to the prediction task.

- block_size:

  Numeric. For spatial blocking, the minimum block size. If NULL,
  automatically determined from diagnosis. Should be larger than the
  autocorrelation range.

- embargo:

  Integer. For temporal blocking, minimum gap between train and test. If
  NULL, automatically determined from diagnosis.

- buffer:

  Numeric. Spatial buffer distance (in coordinate units). Training
  points within this distance of any test-fold point are removed to
  reduce autocorrelation leakage. Default: NULL (no buffer).

- strategy:

  Character. Override the auto-detected CV strategy. Use
  `"temporal_expanding"` for expanding window (forward-chaining) or
  `"temporal_sliding"` for fixed-size sliding window. Default: NULL
  (auto-detect from diagnosis).

- repeats:

  Integer. Number of times to repeat CV fold generation with different
  random seeds. Default: 1 (no repetition).

- seed:

  Integer or NULL. Random seed for stochastic fold generators. When
  `NULL` (default), a fixed internal seed is used so single runs are
  reproducible. With `repeats > 1`, successive repeats derive distinct
  seeds from this value.

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

  A list with elements: `folds` (list of train/test index vectors), `n`
  (number of observations), `diagnosis` (the BorgDiagnosis used),
  `strategy` (CV strategy name), `params` (parameters used).

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

### Distance Matching (kNNDM and NNDM)

With `strategy = "knndm"` or `"nndm"` and `prediction_points`, folds are
shaped so the distribution of test-to-train nearest-neighbour distances
matches the prediction-to-sample nearest-neighbour distribution.
`"knndm"` (Linnenbrink et al. 2024) produces `v` folds and scales to
large samples; `"nndm"` (Mila et al. 2022) is the leave-one-out form,
growing an exclusion radius around each held-out point until the two
distance distributions align, subject to retaining at least `min_train`
(default 0.5) of the data per fold. NNDM builds an `n x n` distance
matrix and is best suited to samples up to a few thousand points; prefer
`"knndm"` beyond that.

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
# \donttest{
cv_rsample <- borg_cv(spatial_data, coords = c("x", "y"), output = "rsample")
# }
```
