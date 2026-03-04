# Framework Integration

BORG integrates with major R machine learning frameworks. This guide
shows how to validate workflows and use BORG-guarded CV functions in
each ecosystem.

## Base R

The simplest integration - manual index-based splitting:

``` r

# Create data
data <- iris
set.seed(42)
n <- nrow(data)
train_idx <- sample(n, 0.7 * n)
test_idx <- setdiff(1:n, train_idx)

# Validate the split
borg(data, train_idx = train_idx, test_idx = test_idx)
#> BorgRisk Assessment
#> ===================
#> 
#> Status: VALID (no hard violations)
#>   Hard violations:  0
#>   Soft inflations:  0
#>   Train indices:    105 rows
#>   Test indices:     45 rows
#>   Inspected at:     2026-03-04 12:49:45
#> 
#> No risks detected.
```

### Safe Preprocessing Pattern

``` r

# CORRECT: Fit preprocessing on training data only
train_data <- data[train_idx, ]
train_means <- colMeans(train_data[, 1:4])
train_sds <- apply(train_data[, 1:4], 2, sd)

# Apply train statistics to both sets
scaled_train <- scale(data[train_idx, 1:4], center = train_means, scale = train_sds)
scaled_test <- scale(data[test_idx, 1:4], center = train_means, scale = train_sds)
```

------------------------------------------------------------------------

## caret

BORG can validate `preProcess` and `trainControl` objects, and provides
a guarded wrapper.

### Validating preProcess Objects

``` r

library(caret)
#> Loading required package: ggplot2
#> Loading required package: lattice

data(mtcars)
train_idx <- 1:25
test_idx <- 26:32

# BAD: preProcess on full data (LEAKS!)
pp_bad <- preProcess(mtcars[, -1], method = c("center", "scale"))
borg_inspect(pp_bad, train_idx, test_idx, data = mtcars)
#> BorgRisk Assessment
#> ===================
#> 
#> Status: INVALID (hard violations detected)
#>   Hard violations:  2
#>   Soft inflations:  0
#>   Train indices:    25 rows
#>   Test indices:     7 rows
#>   Inspected at:     2026-03-04 12:49:45
#> 
#> --- HARD VIOLATIONS (must fix) ---
#> 
#> [1] preprocessing_leak
#>     preProcess centering parameters were computed on data beyond training set (mean difference: 16.1061)
#>     Source: preProcess
#>     Affected: 7 indices (first 5: 26, 27, 28, 29, 30)
#> 
#> [2] preprocessing_leak
#>     preProcess scaling parameters were computed on data beyond training set (sd difference: 9.7915)
#>     Source: preProcess
#>     Affected: 7 indices (first 5: 26, 27, 28, 29, 30)

# GOOD: preProcess on training data only
pp_good <- preProcess(mtcars[train_idx, -1], method = c("center", "scale"))
borg_inspect(pp_good, train_idx, test_idx, data = mtcars)
#> BorgRisk Assessment
#> ===================
#> 
#> Status: VALID (no hard violations)
#>   Hard violations:  0
#>   Soft inflations:  0
#>   Train indices:    25 rows
#>   Test indices:     7 rows
#>   Inspected at:     2026-03-04 12:49:45
#> 
#> No risks detected.
```

### BORG-Guarded trainControl

Use
[`borg_trainControl()`](https://gillescolling.com/BORG/reference/borg_trainControl.md)
to automatically block random resampling when dependencies are detected:

``` r

# Standard caret workflow with spatial data
spatial_data <- data.frame(
  lon = runif(200, 0, 100),
  lat = runif(200, 0, 100),
  response = rnorm(200)
)

# This will warn/error if random CV is inappropriate
ctrl <- borg_trainControl(
  data = spatial_data,
  coords = c("lon", "lat"),
  method = "cv",
  number = 5
)
# If spatial autocorrelation detected, blocks random CV
# Use auto_block = TRUE to automatically switch to spatial blocking
```

------------------------------------------------------------------------

## tidymodels (rsample + recipes)

### Validating Recipe Objects

``` r

library(recipes)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> 
#> Attaching package: 'recipes'
#> The following object is masked from 'package:stats':
#> 
#>     step
library(rsample)
#> 
#> Attaching package: 'rsample'
#> The following object is masked from 'package:caret':
#> 
#>     calibration

data(mtcars)
set.seed(123)
split <- initial_split(mtcars, prop = 0.8)
train_idx <- split$in_id
test_idx <- setdiff(seq_len(nrow(mtcars)), train_idx)

# BAD: Recipe prepped on full data
rec_bad <- recipe(mpg ~ ., data = mtcars) |>
  step_normalize(all_numeric_predictors()) |>
  prep()  # Uses full mtcars!

borg_inspect(rec_bad, train_idx, test_idx, data = mtcars)
#> BorgRisk Assessment
#> ===================
#> 
#> Status: INVALID (hard violations detected)
#>   Hard violations:  3
#>   Soft inflations:  0
#>   Train indices:    25 rows
#>   Test indices:     7 rows
#>   Inspected at:     2026-03-04 12:49:45
#> 
#> --- HARD VIOLATIONS (must fix) ---
#> 
#> [1] preprocessing_leak
#>     Recipe was prepped on 32 rows, but train set has 25 rows. Preprocessing may include test data.
#>     Source: recipe
#>     Affected: 7 indices (first 5: 2, 6, 12, 13, 16)
#> 
#> [2] preprocessing_leak
#>     step_normalize centering parameters were computed on data beyond training set (mean difference: 12.0659)
#>     Source: recipe$steps[[1]] (step_normalize)
#>     Affected: 7 indices (first 5: 2, 6, 12, 13, 16)
#> 
#> [3] preprocessing_leak
#>     step_normalize scaling parameters were computed on data beyond training set (sd difference: 5.72179)
#>     Source: recipe$steps[[1]] (step_normalize)
#>     Affected: 7 indices (first 5: 2, 6, 12, 13, 16)

# GOOD: Recipe prepped on training only
rec_good <- recipe(mpg ~ ., data = training(split)) |>
  step_normalize(all_numeric_predictors()) |>
  prep()

borg_inspect(rec_good, train_idx, test_idx, data = mtcars)
#> BorgRisk Assessment
#> ===================
#> 
#> Status: VALID (no hard violations)
#>   Hard violations:  0
#>   Soft inflations:  0
#>   Train indices:    25 rows
#>   Test indices:     7 rows
#>   Inspected at:     2026-03-04 12:49:45
#> 
#> No risks detected.
```

### BORG-Guarded rsample Functions

BORG provides drop-in replacements for rsample functions that respect
data dependencies:

``` r

# Standard rsample
folds <- vfold_cv(data, v = 5)  # Random folds

# BORG-guarded version
folds <- borg_vfold_cv(
  data = spatial_data,
  coords = c("lon", "lat"),
  v = 5,
  auto_block = TRUE  # Switches to spatial blocking if needed
)
```

``` r

# For grouped data
folds <- borg_group_vfold_cv(
  data = clinical_data,
  group = patient_id,
  v = 5
)
```

``` r

# For temporal data - enforces chronological ordering
split <- borg_initial_split(
  data = ts_data,
  time = "date",
  prop = 0.8
)
```

### Validating rsample Objects

``` r

# Validate existing rsample objects
ts_data <- data.frame(
  date = seq(as.Date("2020-01-01"), by = "day", length.out = 200),
  value = cumsum(rnorm(200))
)

rolling <- rolling_origin(
  data = ts_data,
  initial = 100,
  assess = 20,
  cumulative = FALSE
)

# Check for temporal leakage
borg_inspect(rolling, train_idx = NULL, test_idx = NULL)
#> BorgRisk Assessment
#> ===================
#> 
#> Status: VALID (no hard violations)
#>   Hard violations:  0
#>   Soft inflations:  0
#>   Train indices:    0 rows
#>   Test indices:     0 rows
#>   Inspected at:     2026-03-04 12:49:45
#> 
#> No risks detected.
```

------------------------------------------------------------------------

## mlr3

Validate mlr3 tasks and resamplings:

``` r

library(mlr3)

# Create task
task <- TaskClassif$new("iris", iris, target = "Species")

# Create resampling
resampling <- rsmp("cv", folds = 5)
resampling$instantiate(task)

# Validate first fold
train_idx <- resampling$train_set(1)
test_idx <- resampling$test_set(1)
borg_inspect(task, train_idx, test_idx)
```

------------------------------------------------------------------------

## Temporal Data Workflows

For time series and panel data, temporal ordering is critical.

### Basic Temporal Validation

``` r

set.seed(123)
n <- 365
ts_data <- data.frame(
  date = seq(as.Date("2020-01-01"), by = "day", length.out = n),
  value = cumsum(rnorm(n)),
  feature = rnorm(n)
)

# Chronological split
train_idx <- 1:252
test_idx <- 253:365

# Validate temporal ordering
result <- borg(ts_data, train_idx = train_idx, test_idx = test_idx, time = "date")
result
#> BorgRisk Assessment
#> ===================
#> 
#> Status: VALID (no hard violations)
#>   Hard violations:  0
#>   Soft inflations:  0
#>   Train indices:    252 rows
#>   Test indices:     113 rows
#>   Inspected at:     2026-03-04 12:49:45
#> 
#> No risks detected.
```

### Rolling Origin with rsample

``` r

rolling <- rolling_origin(
  data = ts_data,
  initial = 200,
  assess = 30,
  cumulative = FALSE
)

# Validate the resampling scheme
borg_inspect(rolling, train_idx = NULL, test_idx = NULL)
#> BorgRisk Assessment
#> ===================
#> 
#> Status: VALID (no hard violations)
#>   Hard violations:  0
#>   Soft inflations:  0
#>   Train indices:    0 rows
#>   Test indices:     0 rows
#>   Inspected at:     2026-03-04 12:49:45
#> 
#> No risks detected.
```

------------------------------------------------------------------------

## Spatial Data Workflows

For spatial data, nearby points are often correlated.

### Spatial Block Validation

``` r

set.seed(456)
n <- 200
spatial_data <- data.frame(
  lon = runif(n, -10, 10),
  lat = runif(n, -10, 10),
  response = rnorm(n),
  predictor = rnorm(n)
)

# Geographic split (west vs east)
train_idx <- which(spatial_data$lon < 0)
test_idx <- which(spatial_data$lon >= 0)

# Validate with spatial awareness
result <- borg(spatial_data,
               train_idx = train_idx,
               test_idx = test_idx,
               coords = c("lon", "lat"))
result
#> BorgRisk Assessment
#> ===================
#> 
#> Status: VALID (no hard violations)
#>   Hard violations:  0
#>   Soft inflations:  0
#>   Train indices:    91 rows
#>   Test indices:     109 rows
#>   Inspected at:     2026-03-04 12:49:45
#> 
#> No risks detected.
```

### Automatic Spatial CV Generation

``` r

# Let BORG generate spatially-blocked folds
result <- borg(spatial_data, coords = c("lon", "lat"), target = "response", v = 5)
result$diagnosis@recommended_cv
#> [1] "random"

# Access the folds
length(result$folds)
#> [1] 5
```

------------------------------------------------------------------------

## Grouped Data Workflows

For hierarchical data (patients, sites, species):

``` r

# Clinical trial data with repeated measures
clinical_data <- data.frame(
  patient_id = rep(1:50, each = 4),
  visit = rep(1:4, times = 50),
  outcome = rnorm(200)
)

# Let BORG create leave-group-out folds
result <- borg(clinical_data, groups = "patient_id", target = "outcome", v = 5)
result$diagnosis@recommended_cv
#> [1] "random"

# Verify no patient appears in both train and test
fold1 <- result$folds[[1]]
train_patients <- unique(clinical_data$patient_id[fold1$train])
test_patients <- unique(clinical_data$patient_id[fold1$test])
length(intersect(train_patients, test_patients))  # Should be 0
#> [1] 29
```

------------------------------------------------------------------------

## Complete Pipeline Validation

Validate an entire workflow at once:

``` r

# Build a workflow
data <- iris
set.seed(789)
n <- nrow(data)
train_idx <- sample(n, 0.7 * n)
test_idx <- setdiff(1:n, train_idx)

# Validate everything
result <- borg_validate(list(
  data = data,
  train_idx = train_idx,
  test_idx = test_idx
))

result
#> BorgRisk Assessment
#> ===================
#> 
#> Status: INVALID (hard violations detected)
#>   Hard violations:  1
#>   Soft inflations:  0
#>   Train indices:    105 rows
#>   Test indices:     45 rows
#>   Inspected at:     2026-03-04 12:49:45
#> 
#> --- HARD VIOLATIONS (must fix) ---
#> 
#> [1] duplicate_rows
#>     Test set contains 1 rows identical to training rows (memorization risk)
#>     Source: data.frame
#>     Affected: 102
```

### With Problematic Workflow

``` r

# Workflow with overlap (common mistake)
bad_workflow <- list(
  data = iris,
  train_idx = 1:100,
  test_idx = 51:150  # Overlaps!
)

result <- borg_validate(bad_workflow)
result
#> BorgRisk Assessment
#> ===================
#> 
#> Status: INVALID (hard violations detected)
#>   Hard violations:  2
#>   Soft inflations:  0
#>   Train indices:    100 rows
#>   Test indices:     100 rows
#>   Inspected at:     2026-03-04 12:49:45
#> 
#> --- HARD VIOLATIONS (must fix) ---
#> 
#> [1] index_overlap
#>     Train and test indices overlap (50 shared indices)
#>     Source: workflow$train_idx/test_idx
#>     Affected: 50 indices (first 5: 51, 52, 53, 54, 55)
#> 
#> [2] duplicate_rows
#>     Test set contains 50 rows identical to training rows (memorization risk)
#>     Source: data.frame
#>     Affected: 50 indices (first 5: 51, 52, 53, 54, 55)
```

------------------------------------------------------------------------

## Automatic Repair with borg_rewrite()

BORG can attempt to fix some issues automatically:

``` r

# Workflow with fixable issues
workflow <- list(
  data = iris,
  train_idx = 1:100,
  test_idx = 51:150  # Overlap
)

# Attempt fixes
fixed <- borg_rewrite(workflow)

if (length(fixed$unfixable) > 0) {
  cat("Could not fix:", paste(fixed$unfixable, collapse = ", "), "\n")
} else {
  cat("Workflow repaired successfully\n")
}
#> Could not fix: index_overlap, duplicate_rows
```

Note: Index overlap cannot be automatically fixed - it requires choosing
a new split strategy.

------------------------------------------------------------------------

## Summary: Framework Integration Patterns

| Framework | Validation Function | Guarded Alternative |
|----|----|----|
| Base R | [`borg()`](https://gillescolling.com/BORG/reference/borg.md), [`borg_inspect()`](https://gillescolling.com/BORG/reference/borg_inspect.md) | \- |
| caret | `borg_inspect(preProcess)` | [`borg_trainControl()`](https://gillescolling.com/BORG/reference/borg_trainControl.md) |
| rsample | `borg_inspect(vfold_cv)` | [`borg_vfold_cv()`](https://gillescolling.com/BORG/reference/borg_vfold_cv.md), [`borg_initial_split()`](https://gillescolling.com/BORG/reference/borg_initial_split.md) |
| recipes | `borg_inspect(recipe)` | \- |
| mlr3 | `borg_inspect(task)` | \- |

## See Also

- [`vignette("quickstart")`](https://gillescolling.com/BORG/articles/quickstart.md) -
  Basic usage and concepts

- [`vignette("risk-taxonomy")`](https://gillescolling.com/BORG/articles/risk-taxonomy.md) -
  Complete catalog of detectable risks
