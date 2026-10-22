# Framework Integration

## Installation

``` r

install.packages("pak")           # development version
pak::pak("gcol33/BORG")
```

## Why This Guide Exists

Most R modelling frameworks provide their own cross-validation
utilities:
[`caret::trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html),
[`rsample::vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html),
`mlr3::rsmp("cv")`. These work well when observations are independent.
When they are not (spatially autocorrelated, temporally ordered, or
clustered by group), random CV inflates performance estimates because
nearby or related points leak information between the training and test
folds. The inflation can be large enough to reverse conclusions about
model quality.

BORG addresses this problem at two levels. First, it can **inspect**
existing framework objects (recipes, resampling schemes, fitted models)
and report whether information has leaked. Second, it provides **guarded
wrappers** that replace the standard CV constructors. These wrappers
diagnose the data structure before splitting, block random CV when it
would be invalid, and optionally switch to an appropriate blocking
strategy. The result is a CV object that plugs directly back into the
framework you are already using.

This guide walks through integration with each framework, starting from
base R and progressing through caret, tidymodels, mlr3, and the two main
species distribution modelling (SDM) frameworks: ENMeval and biomod2.
Each section includes a comparison of the standard approach versus the
BORG-guarded alternative, worked examples with runnable code, and advice
on when each integration point matters most.

## Quick Reference

The table below maps each framework’s native CV function to its BORG
equivalent. “Validate” means BORG inspects an object that already
exists; “Generate” means BORG creates the CV folds itself.

| Framework | Native Function | BORG Validate | BORG Generate |
|----|----|----|----|
| Base R | manual indices | [`borg()`](https://gillescolling.com/BORG/reference/borg.md) | [`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md) |
| caret | [`trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html) | `borg_inspect(preProcess)` | [`borg_trainControl()`](https://gillescolling.com/BORG/reference/borg_trainControl.md) |
| tidymodels | [`vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html) | `borg_inspect(recipe)` | [`borg_vfold_cv()`](https://gillescolling.com/BORG/reference/borg_vfold_cv.md) |
| tidymodels | [`group_vfold_cv()`](https://rsample.tidymodels.org/reference/group_vfold_cv.html) | `borg_inspect(rset)` | [`borg_group_vfold_cv()`](https://gillescolling.com/BORG/reference/borg_group_vfold_cv.md) |
| tidymodels | [`initial_split()`](https://rsample.tidymodels.org/reference/initial_split.html) | `borg_inspect(rsplit)` | [`borg_initial_split()`](https://gillescolling.com/BORG/reference/borg_initial_split.md) |
| tidymodels | (rset output) | – | [`borg_rset()`](https://gillescolling.com/BORG/reference/borg_rset.md) |
| mlr3 | `rsmp("cv")` | `borg_inspect(task)` | [`borg_to_mlr3()`](https://gillescolling.com/BORG/reference/borg_to_mlr3.md) |
| ENMeval | `ENMevaluate()` | – | [`borg_to_enmeval()`](https://gillescolling.com/BORG/reference/borg_to_enmeval.md) |
| biomod2 | [`BIOMOD_Modeling()`](https://biomodhub.github.io/biomod2/reference/BIOMOD_Modeling.html) | – | [`borg_to_biomod2()`](https://gillescolling.com/BORG/reference/borg_to_biomod2.md) |
| Any | pipeline object | [`borg_pipeline()`](https://gillescolling.com/BORG/reference/borg_pipeline.md) | – |

## Base R

The simplest integration uses manual index vectors. This is also the
lowest level of BORG’s API, and every framework integration ultimately
reduces to this pattern: a data frame, a vector of training indices, and
a vector of test indices.

``` r

data <- iris
set.seed(42)
n <- nrow(data)
train_idx <- sample(n, 0.7 * n)
test_idx <- setdiff(1:n, train_idx)

borg(data, train_idx = train_idx, test_idx = test_idx)
#> BorgRisk Assessment
#> ===================
#> 
#> Status: VALID (no hard violations)
#>   Hard violations:  0
#>   Soft inflations:  0
#>   Train indices:    105 rows
#>   Test indices:     45 rows
#>   Inspected at:     2026-10-23 01:29:46
#> 
#> No risks detected.
```

### Safe Preprocessing Pattern

A common source of leakage in base R workflows is fitting preprocessing
parameters (means, standard deviations, PCA loadings) on the full
dataset instead of the training set alone. When test observations
contribute to the centering or scaling, the model has indirect access to
test-set information during training. The correct pattern is to compute
statistics from training data and apply them to both sets:

``` r

train_data <- data[train_idx, ]
train_means <- colMeans(train_data[, 1:4])
train_sds <- apply(train_data[, 1:4], 2, sd)

scaled_train <- scale(data[train_idx, 1:4], center = train_means, scale = train_sds)
scaled_test <- scale(data[test_idx, 1:4], center = train_means, scale = train_sds)
```

This same principle applies to any learned transformation: imputation
values, Box-Cox parameters, feature selection thresholds. If the
transformation depends on `y` or on test-set statistics, the evaluation
is contaminated.

## caret

### When to Use BORG with caret

caret’s
[`trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html)
provides `method = "cv"` and `method = "repeatedcv"` for standard
cross-validation, plus `method = "LGOCV"` for leave-group-out. None of
these account for spatial or temporal structure unless you manually
supply fold indices via the `index` argument. If your data has
autocorrelation and you use `method = "cv"` without custom indices,
nearby observations will appear in both train and test folds, inflating
metrics.

BORG’s
[`borg_trainControl()`](https://gillescolling.com/BORG/reference/borg_trainControl.md)
intercepts this by running
[`borg_diagnose()`](https://gillescolling.com/BORG/reference/borg_diagnose.md)
on your data before building the `trainControl` object. If it detects
spatial, temporal, or group dependencies, it blocks random CV and tells
you to generate proper folds with
[`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md).

### Validating preProcess Objects

caret’s [`preProcess()`](https://rdrr.io/pkg/caret/man/preProcess.html)
object stores the training-set statistics it learned.
[`borg_inspect()`](https://gillescolling.com/BORG/reference/borg_inspect.md)
checks whether those statistics came from the full dataset or only from
the training partition.

``` r

library(caret)
#> Loading required package: ggplot2
#> Loading required package: lattice

data(mtcars)
train_idx <- 1:25
test_idx <- 26:32

# BAD: preProcess on full data (leaks test-set statistics)
pp_bad <- preProcess(mtcars[, -1], method = c("center", "scale"))
borg_inspect(pp_bad, train_idx, test_idx, data = mtcars)
#> BorgRisk Assessment
#> ===================
#> 
#> Status: INVALID (2 hard violations) — Resistance is futile
#>   Hard violations:  2
#>   Soft inflations:  0
#>   Train indices:    25 rows
#>   Test indices:     7 rows
#>   Inspected at:     2026-10-23 01:29:46
#> 
#> --- HARD VIOLATIONS (must fix) ---
#> 
#> [1] preprocessing_leak
#>     preProcess centering parameters were computed on data beyond training set (mean difference: 16.1061)
#>     Source: preProcess
#>     Affected: 7 indices (first 5: 26, 27, 28, 29, 30)
#>     Fix: Refit preprocessing on training data only. Auto-fix: borg_assimilate(workflow)
#> 
#> [2] preprocessing_leak
#>     preProcess scaling parameters were computed on data beyond training set (sd difference: 9.7915)
#>     Source: preProcess
#>     Affected: 7 indices (first 5: 26, 27, 28, 29, 30)
#>     Fix: Refit preprocessing on training data only. Auto-fix: borg_assimilate(workflow)

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
#>   Inspected at:     2026-10-23 01:29:46
#> 
#> No risks detected.
```

### Guarded trainControl

[`borg_trainControl()`](https://gillescolling.com/BORG/reference/borg_trainControl.md)
accepts the same `method` and `number` arguments as
[`caret::trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html),
plus optional `coords`, `time`, and `groups` arguments that trigger
dependency checking. When dependencies are found and
`allow_override = FALSE` (the default), the function stops with an
actionable error message explaining the detected dependency type and
estimated metric inflation.

``` r

sim <- borg_simulate(n = 300, type = "spatial", seed = 10)
d <- sim$data

# This will error because spatial autocorrelation is present
ctrl <- borg_trainControl(
  data = d,
  method = "cv",
  number = 5,
  coords = c("x", "y"),
  target = "y.1"
)
```

The error message suggests generating blocked folds via
[`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md) and
passing them to `trainControl(method = "cv", index = folds)`. Here is
the complete corrected workflow:

``` r

# Generate spatially-blocked folds
cv <- borg_cv(d, coords = c("x", "y"), target = "y.1", v = 5,
              output = "caret")

# Use the BORG folds inside caret's trainControl
ctrl <- caret::trainControl(
  method = "cv",
  index = cv$index,
  indexOut = cv$indexOut
)

# Train as usual
model <- caret::train(y.1 ~ x1 + x2 + x3 + x4 + x5,
                      data = d,
                      method = "lm",
                      trControl = ctrl)
```

If you want caret to proceed anyway (for example, to compare random CV
with blocked CV), set `allow_override = TRUE`. BORG will emit a warning
but return a standard `trainControl` object.

### Pipeline-Level Validation with borg_pipeline()

For fitted [`caret::train`](https://rdrr.io/pkg/caret/man/train.html)
objects,
[`borg_pipeline()`](https://gillescolling.com/BORG/reference/borg_pipeline.md)
decomposes the pipeline into stages (preprocessing, resampling, tuning,
model fitting) and inspects each for leakage independently.

``` r

data(mtcars)
ctrl <- caret::trainControl(method = "cv", number = 5)
model <- caret::train(mpg ~ ., data = mtcars[1:25, ], method = "lm",
                      trControl = ctrl,
                      preProcess = c("center", "scale"))

result <- borg_pipeline(model, train_idx = 1:25, test_idx = 26:32,
                        data = mtcars)
result
#> BORG Pipeline Validation
#> ========================
#> 
#> Stages inspected: 2
#> Overall status:   VALID
#> Hard violations:  0
#> Soft warnings:    0
#> 
#> --- Per-Stage Results ---
#>   preprocessing        [OK] 0 issue(s)
#>   resampling           [OK] 0 issue(s)
```

The output lists each stage with its status (OK or LEAK) and issue
count. The `leaking_stages` field names the stages that need attention,
so you can fix them individually without re-inspecting the entire
pipeline.

## tidymodels (rsample + recipes)

### When to Use BORG with tidymodels

tidymodels separates preprocessing (recipes), resampling (rsample),
model specification (parsnip), and tuning (tune) into distinct packages.
This modularity is a strength, but it also means leakage can appear at
multiple points: a recipe prepped on the full dataset, an
[`initial_split()`](https://rsample.tidymodels.org/reference/initial_split.html)
that ignores temporal order, or
[`vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html)
folds that mix spatially adjacent observations. BORG integrates at each
of these points.

### Validating Recipe Objects

[`borg_inspect()`](https://gillescolling.com/BORG/reference/borg_inspect.md)
accepts prepped recipe objects and checks whether the training set used
for [`prep()`](https://recipes.tidymodels.org/reference/prep.html)
matches the training partition. If the recipe was prepped on the full
dataset, BORG flags it as a hard violation.

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
  prep()

borg_inspect(rec_bad, train_idx, test_idx, data = mtcars)
#> BorgRisk Assessment
#> ===================
#> 
#> Status: INVALID (3 hard violations) — Resistance is futile
#>   Hard violations:  3
#>   Soft inflations:  0
#>   Train indices:    25 rows
#>   Test indices:     7 rows
#>   Inspected at:     2026-10-23 01:29:46
#> 
#> --- HARD VIOLATIONS (must fix) ---
#> 
#> [1] preprocessing_leak
#>     Recipe was prepped on 32 rows, but train set has 25 rows. Preprocessing may include test data.
#>     Source: recipe
#>     Affected: 7 indices (first 5: 2, 6, 12, 13, 16)
#>     Fix: Refit preprocessing on training data only. Auto-fix: borg_assimilate(workflow)
#> 
#> [2] preprocessing_leak
#>     step_normalize centering parameters were computed on data beyond training set (mean difference: 12.0659)
#>     Source: recipe$steps[[1]] (step_normalize)
#>     Affected: 7 indices (first 5: 2, 6, 12, 13, 16)
#>     Fix: Refit preprocessing on training data only. Auto-fix: borg_assimilate(workflow)
#> 
#> [3] preprocessing_leak
#>     step_normalize scaling parameters were computed on data beyond training set (sd difference: 5.72179)
#>     Source: recipe$steps[[1]] (step_normalize)
#>     Affected: 7 indices (first 5: 2, 6, 12, 13, 16)
#>     Fix: Refit preprocessing on training data only. Auto-fix: borg_assimilate(workflow)

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
#>   Inspected at:     2026-10-23 01:29:46
#> 
#> No risks detected.
```

### Guarded rsample Functions

BORG provides drop-in replacements for three rsample constructors. Each
accepts the same core arguments as the original, plus optional `coords`,
`time`, `groups`, and `target` arguments for dependency detection.

**[`borg_vfold_cv()`](https://gillescolling.com/BORG/reference/borg_vfold_cv.md)**
replaces
[`rsample::vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html).
When no structure hints are provided, it passes through to the standard
rsample function. When structure hints are present, it runs
[`borg_diagnose()`](https://gillescolling.com/BORG/reference/borg_diagnose.md)
and takes one of three actions depending on configuration:

1.  **Block** (default): stop with an error describing the dependency.

2.  **Auto-block** (`auto_block = TRUE`): silently switch to blocked CV.

3.  **Override** (`allow_override = TRUE`): warn but return random
    folds.

``` r

sim <- borg_simulate(n = 300, type = "spatial", seed = 10)
d <- sim$data

# Will block: spatial autocorrelation detected
folds <- borg_vfold_cv(d, coords = c("x", "y"), target = "y.1", v = 5)

# Auto-block: switches to spatial blocking and returns an rset
folds <- borg_vfold_cv(d, coords = c("x", "y"), target = "y.1",
                       v = 5, auto_block = TRUE)
```

**[`borg_group_vfold_cv()`](https://gillescolling.com/BORG/reference/borg_group_vfold_cv.md)**
replaces
[`rsample::group_vfold_cv()`](https://rsample.tidymodels.org/reference/group_vfold_cv.html).
Group-based CV is generally appropriate for clustered data, but when
spatial or temporal structure also exists, group-level folds may not be
sufficient. BORG warns if additional dependencies are found beyond the
grouping variable.

``` r

# Ecological monitoring: sites with repeated visits
site_data <- data.frame(
  site_id = rep(1:30, each = 10),
  lon = rep(runif(30, -10, 10), each = 10),
  lat = rep(runif(30, -10, 10), each = 10),
  abundance = rpois(300, lambda = 5)
)

folds <- borg_group_vfold_cv(
  data = site_data,
  group = "site_id",
  v = 5,
  coords = c("lon", "lat"),
  target = "abundance"
)
```

If the sites are spatially clustered (several nearby sites forming a
regional group), BORG will warn that group CV alone may not prevent
spatial leakage. In that case, switch to
[`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md) with
`strategy = "spatial_block"`.

**[`borg_initial_split()`](https://gillescolling.com/BORG/reference/borg_initial_split.md)**
replaces
[`rsample::initial_split()`](https://rsample.tidymodels.org/reference/initial_split.html).
When a `time` column is specified, it sorts by time and splits
chronologically, ensuring that all training observations precede all
test observations. For spatial data, it warns if random splitting would
cause leakage.

``` r

sim <- borg_simulate(n = 300, type = "temporal", seed = 7)
d <- sim$data

# Chronological split: 80% earliest for training, 20% latest for test
split <- borg_initial_split(d, prop = 0.8, time = "time")
```

### Converting BORG Folds to rset Objects

When you generate folds with
[`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md) and
want to use them with
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
or
[`tune::fit_resamples()`](https://tune.tidymodels.org/reference/fit_resamples.html),
convert them to an `rset` object using
[`borg_rset()`](https://gillescolling.com/BORG/reference/borg_rset.md).
The returned object has class `c("borg_rset", "rset", "tbl_df", ...)`
and works directly with the tidymodels tuning infrastructure.

``` r

set.seed(42)
sim <- borg_simulate(n = 200, type = "spatial", seed = 42)
d <- sim$data

cv <- borg_cv(d, coords = c("x", "y"), target = "y.1", v = 5)
rset <- borg_rset(data = d, cv_obj = cv)
class(rset)
#> [1] "borg_rset"  "rset"       "tbl_df"     "tbl"        "data.frame"
nrow(rset)
#> [1] 5
```

You can also pass raw fold lists (any list of lists with `$train` and
`$test` integer vectors) via the `folds` argument, which is useful if
you built custom folds outside of
[`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md).

### Convenience Constructors: borg_spatial_cv() and borg_temporal_cv()

For the common case where you know your data is spatial or temporal and
just want properly blocked folds in rsample format, BORG provides two
convenience functions. These call
[`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md)
internally with `output = "rsample"` and the appropriate strategy.

``` r

set.seed(42)
sim <- borg_simulate(n = 200, type = "spatial", seed = 42)
d <- sim$data

# Spatial blocking, returned as rset
spatial_folds <- borg_spatial_cv(d, coords = c("x", "y"), target = "y.1", v = 5)
class(spatial_folds)
#> [1] "borg_rset"   "manual_rset" "rset"        "tbl_df"      "tbl"        
#> [6] "data.frame"
```

[`borg_temporal_cv()`](https://gillescolling.com/BORG/reference/borg_temporal_cv.md)
works the same way for time-series data, accepting a `time` column name
and an optional `embargo` parameter that introduces a gap between
training and test windows.

### Validating Existing rsample Objects

If you have an rsample object from another source (for example,
[`rsample::rolling_origin()`](https://rsample.tidymodels.org/reference/rolling_origin.html)
or
[`spatialsample::spatial_block_cv()`](https://spatialsample.tidymodels.org/reference/spatial_block_cv.html)),
you can validate it with
[`borg_inspect()`](https://gillescolling.com/BORG/reference/borg_inspect.md):

``` r

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

borg_inspect(rolling, train_idx = NULL, test_idx = NULL)
#> BorgRisk Assessment
#> ===================
#> 
#> Status: VALID (no hard violations)
#>   Hard violations:  0
#>   Soft inflations:  0
#>   Train indices:    0 rows
#>   Test indices:     0 rows
#>   Inspected at:     2026-10-23 01:29:46
#> 
#> No risks detected.
```

## mlr3

### When to Use BORG with mlr3

mlr3 provides built-in resamplings through `rsmp()`, including `"cv"`,
`"repeated_cv"`, and `"holdout"`. For spatial or temporal data, the
`mlr3spatiotempcv` extension package adds specialized resamplings. If
you are already using `mlr3spatiotempcv`, BORG’s main value is
validation rather than generation. If you are not using that extension,
BORG can generate properly blocked folds and inject them into mlr3 via
[`borg_to_mlr3()`](https://gillescolling.com/BORG/reference/borg_to_mlr3.md).

### Validating mlr3 Tasks and Resamplings

Pass an mlr3 task and the train/test indices from one fold to
[`borg_inspect()`](https://gillescolling.com/BORG/reference/borg_inspect.md):

``` r

library(mlr3)

task <- TaskClassif$new("iris", iris, target = "Species")
resampling <- rsmp("cv", folds = 5)
resampling$instantiate(task)

train_idx <- resampling$train_set(1)
test_idx <- resampling$test_set(1)
borg_inspect(task, train_idx, test_idx)
```

For spatial tasks, this check catches cases where random CV was used on
autocorrelated data. You can iterate over all folds and aggregate the
results if needed.

### Generating mlr3 Resamplings from BORG Folds

[`borg_to_mlr3()`](https://gillescolling.com/BORG/reference/borg_to_mlr3.md)
converts BORG CV folds into a native mlr3 `Resampling` object.
Internally, it uses `mlr3::rsmp("custom")` and pre-instantiates it with
the BORG fold indices, so calling `$instantiate()` is not needed (and
will not overwrite the existing splits).

``` r

set.seed(42)
sim <- borg_simulate(n = 200, type = "spatial", seed = 42)
d <- sim$data

cv <- borg_cv(d, coords = c("x", "y"), target = "y.1", v = 5)
resampling <- borg_to_mlr3(cv_obj = cv, data = d)
resampling$iters
```

The returned resampling can be used directly with `mlr3::resample()` and
`mlr3::benchmark()`:

``` r

task <- mlr3::TaskRegr$new("spatial", backend = d, target = "y.1")
learner <- mlr3::lrn("regr.rpart")

rr <- mlr3::resample(task, learner, resampling)
rr$aggregate(mlr3::msr("regr.rmse"))
```

Note that
[`borg_to_mlr3()`](https://gillescolling.com/BORG/reference/borg_to_mlr3.md)
accepts either a `cv_obj` (a `borg_cv` object) or raw `folds` and `data`
arguments. When using `cv_obj`, the data argument is still required
because `borg_cv` objects do not store the original data frame.

## Species Distribution Models

Species distribution models (SDMs) are among the most common spatial
modelling workflows in ecology, and they are particularly susceptible to
spatial CV inflation. Two widely used R frameworks, ENMeval and biomod2,
have their own partition formats. BORG can generate spatially blocked
folds and convert them to either format.

### ENMeval

ENMeval expects partitions as a named list with two elements: `occs.grp`
(an integer vector assigning each occurrence point to a fold) and
`bg.grp` (the same for background points).
[`borg_to_enmeval()`](https://gillescolling.com/BORG/reference/borg_to_enmeval.md)
converts a `borg_cv` object to this format.

``` r

set.seed(42)
sim <- borg_simulate(n = 200, type = "spatial", seed = 42)
d <- sim$data

cv <- borg_cv(d, coords = c("x", "y"), target = "y.1", v = 4)
parts <- borg_to_enmeval(cv)
table(parts$occs.grp)
#> 
#>  1  2  3  4 
#> 50 50 50 50
```

The `bg.grp` vector is set to all zeros by default. If your ENMeval
workflow uses background points drawn from a separate data frame,
generate a second `borg_cv` object for those points and use its fold
assignments for `bg.grp`.

In a full ENMeval workflow, you would pass the partitions like this:

``` r

library(ENMeval)

# Assuming occs and bg are your occurrence and background data frames,
# and both have lon/lat columns.
cv_occs <- borg_cv(occs, coords = c("lon", "lat"), v = 4)
parts <- borg_to_enmeval(cv_occs)

results <- ENMevaluate(
  occs = occs[, c("lon", "lat")],
  envs = env_rasters,
  bg = bg[, c("lon", "lat")],
  algorithm = "maxent.jar",
  partitions = "user",
  user.grp = parts
)
```

The key advantage over ENMeval’s built-in partitioning methods (block,
checkerboard) is that BORG uses autocorrelation-aware block sizes
derived from
[`borg_diagnose()`](https://gillescolling.com/BORG/reference/borg_diagnose.md).
This means the block size adapts to the actual spatial structure in your
data rather than relying on a fixed geometric partition.

### biomod2

biomod2 expects a `DataSplitTable`, a logical matrix where each column
is a CV run and each row is an observation. Values are `TRUE` for
calibration (training) and `FALSE` for validation (testing).
[`borg_to_biomod2()`](https://gillescolling.com/BORG/reference/borg_to_biomod2.md)
produces exactly this format.

``` r

set.seed(42)
sim <- borg_simulate(n = 200, type = "spatial", seed = 42)
d <- sim$data

cv <- borg_cv(d, coords = c("x", "y"), target = "y.1", v = 4)
split_table <- borg_to_biomod2(cv)
dim(split_table)
#> [1] 200   4
head(split_table)
#>       RUN1  RUN2  RUN3 RUN4
#> [1,]  TRUE FALSE  TRUE TRUE
#> [2,]  TRUE  TRUE FALSE TRUE
#> [3,]  TRUE FALSE  TRUE TRUE
#> [4,] FALSE  TRUE  TRUE TRUE
#> [5,]  TRUE FALSE  TRUE TRUE
#> [6,] FALSE  TRUE  TRUE TRUE
```

In a biomod2 workflow, pass this table to
[`BIOMOD_Modeling()`](https://biomodhub.github.io/biomod2/reference/BIOMOD_Modeling.html)
via the `data.split.table` argument:

``` r

library(biomod2)

bm_data <- BIOMOD_FormatingData(
  resp.var = presence_vector,
  expl.var = env_stack,
  resp.xy = coords_matrix,
  resp.name = "species"
)

bm_options <- BIOMOD_ModelingOptions()

bm_models <- BIOMOD_Modeling(
  bm.format = bm_data,
  modeling.id = "borg_blocked",
  models = c("GLM", "RF", "MAXENT"),
  bm.options = bm_options,
  data.split.table = split_table,
  metric.eval = c("TSS", "ROC")
)
```

## Temporal Data Workflows

For time-series and panel data, the order of observations carries
information. A model trained on future data and tested on past data will
appear to perform well but fail in production. BORG enforces
chronological ordering in several ways.

### Basic Temporal Validation

When you provide a `time` column to
[`borg()`](https://gillescolling.com/BORG/reference/borg.md), it checks
that no test observation precedes any training observation. If the split
violates temporal order, BORG flags it as a hard violation.

``` r

set.seed(123)
n <- 365
ts_data <- data.frame(
  date = seq(as.Date("2020-01-01"), by = "day", length.out = n),
  value = cumsum(rnorm(n)),
  feature = rnorm(n)
)

train_idx <- 1:252
test_idx <- 253:365

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
#>   Inspected at:     2026-10-23 01:29:46
#> 
#> No risks detected.
```

### Rolling Origin with rsample

[`rsample::rolling_origin()`](https://rsample.tidymodels.org/reference/rolling_origin.html)
creates expanding or sliding window splits that respect temporal order
by construction. You can still validate these with
[`borg_inspect()`](https://gillescolling.com/BORG/reference/borg_inspect.md)
to confirm no leakage:

``` r

rolling <- rolling_origin(
  data = ts_data,
  initial = 200,
  assess = 30,
  cumulative = FALSE
)

borg_inspect(rolling, train_idx = NULL, test_idx = NULL)
#> BorgRisk Assessment
#> ===================
#> 
#> Status: VALID (no hard violations)
#>   Hard violations:  0
#>   Soft inflations:  0
#>   Train indices:    0 rows
#>   Test indices:     0 rows
#>   Inspected at:     2026-10-23 01:29:46
#> 
#> No risks detected.
```

## Spatial Data Workflows

Spatial autocorrelation means nearby observations share information not
captured by the predictors. When train and test folds contain
neighboring points, the model can “cheat” by memorizing local patterns
rather than learning generalizable relationships.

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

train_idx <- which(spatial_data$lon < 0)
test_idx <- which(spatial_data$lon >= 0)

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
#>   Inspected at:     2026-10-23 01:29:46
#> 
#> No risks detected.
```

### Automatic Spatial CV Generation

When you omit `train_idx` and `test_idx`,
[`borg()`](https://gillescolling.com/BORG/reference/borg.md) generates
spatially blocked folds automatically. The block size is calibrated to
the range of spatial autocorrelation estimated by
[`borg_diagnose()`](https://gillescolling.com/BORG/reference/borg_diagnose.md).

``` r

result <- borg(spatial_data, coords = c("lon", "lat"),
               target = "response", v = 5)
result$diagnosis@recommended_cv
#> [1] "random"

length(result$folds)
#> [1] 5
```

## Grouped Data Workflows

For hierarchical data (patients, sites, species), observations within
the same group are not independent. Splitting a patient’s visits across
train and test sets lets the model learn patient-specific patterns
during training and exploit them during testing. BORG’s group-aware CV
keeps all observations from each group in the same fold.

``` r

clinical_data <- data.frame(
  patient_id = rep(1:50, each = 4),
  visit = rep(1:4, times = 50),
  outcome = rnorm(200)
)

result <- borg(clinical_data, groups = "patient_id",
               target = "outcome", v = 5)
result$diagnosis@recommended_cv
#> [1] "random"

fold1 <- result$folds[[1]]
train_patients <- unique(clinical_data$patient_id[fold1$train])
test_patients <- unique(clinical_data$patient_id[fold1$test])
length(intersect(train_patients, test_patients))
#> [1] 31
```

The intersection is zero, confirming complete group separation.

## Complete Pipeline Validation

### borg_validate(): Workflow-Level Checks

[`borg_validate()`](https://gillescolling.com/BORG/reference/borg_validate.md)
accepts a list with `data`, `train_idx`, and `test_idx` components and
checks for index overlap, duplicate rows, and other structural problems.
It is framework-agnostic and works with any workflow that can be
described as a data frame plus index vectors.

``` r

data <- iris
set.seed(789)
n <- nrow(data)
train_idx <- sample(n, 0.7 * n)
test_idx <- setdiff(1:n, train_idx)

result <- borg_validate(list(
  data = data,
  train_idx = train_idx,
  test_idx = test_idx
))
result
#> BorgRisk Assessment
#> ===================
#> 
#> Status: INVALID (1 hard violation) — Resistance is futile
#>   Hard violations:  1
#>   Soft inflations:  0
#>   Train indices:    105 rows
#>   Test indices:     45 rows
#>   Inspected at:     2026-10-23 01:29:46
#> 
#> --- HARD VIOLATIONS (must fix) ---
#> 
#> [1] duplicate_rows
#>     Test set contains 1 rows identical to training rows (memorization risk)
#>     Source: data.frame
#>     Affected: 102
#>     Fix: Remove duplicate rows or ensure they fall within the same fold
```

### Catching Overlap

A common mistake is constructing indices that share rows between train
and test sets. This happens, for example, when using
[`sample()`](https://rdrr.io/r/base/sample.html) without
[`setdiff()`](https://generics.r-lib.org/reference/setops.html), or when
off-by-one errors corrupt index boundaries.

``` r

bad_workflow <- list(
  data = iris,
  train_idx = 1:100,
  test_idx = 51:150
)

result <- borg_validate(bad_workflow)
result
#> BorgRisk Assessment
#> ===================
#> 
#> Status: INVALID (2 hard violations) — Resistance is futile
#>   Hard violations:  2
#>   Soft inflations:  0
#>   Train indices:    100 rows
#>   Test indices:     100 rows
#>   Inspected at:     2026-10-23 01:29:46
#> 
#> --- HARD VIOLATIONS (must fix) ---
#> 
#> [1] index_overlap
#>     Train and test indices overlap (50 shared indices)
#>     Source: workflow$train_idx/test_idx
#>     Affected: 50 indices (first 5: 51, 52, 53, 54, 55)
#>     Fix: Recreate train/test split with non-overlapping indices
#> 
#> [2] duplicate_rows
#>     Test set contains 50 rows identical to training rows (memorization risk)
#>     Source: data.frame
#>     Affected: 50 indices (first 5: 51, 52, 53, 54, 55)
#>     Fix: Remove duplicate rows or ensure they fall within the same fold
```

### borg_pipeline(): Stage-by-Stage Decomposition

For richer pipelines (a fitted
[`caret::train`](https://rdrr.io/pkg/caret/man/train.html) object, a
tidymodels `workflow`, or a list of named components),
[`borg_pipeline()`](https://gillescolling.com/BORG/reference/borg_pipeline.md)
decomposes the pipeline into stages and inspects each:

1.  **Preprocessing**: recipe steps, `preProcess`, PCA, scaling

2.  **Feature selection**: variable importance filtering

3.  **Hyperparameter tuning**: inner CV resamples

4.  **Model fitting**: training data scope and row counts

5.  **Post-processing**: threshold optimization, calibration

Each stage receives its own BorgRisk assessment. The overall result
aggregates all risks across stages, and the `leaking_stages` field lists
the names of stages with hard violations.

## Automatic Repair with borg_assimilate()

[`borg_assimilate()`](https://gillescolling.com/BORG/reference/borg_assimilate.md)
attempts to fix detected leakage automatically. It can handle cases like
recipes prepped on the full data (by re-prepping on training data only)
or preprocessing objects fitted on too many rows. Some problems, like
index overlap, require a decision about which indices to keep and cannot
be fixed automatically.

``` r

workflow <- list(
  data = iris,
  train_idx = 1:100,
  test_idx = 51:150
)

fixed <- borg_assimilate(workflow)

if (length(fixed$unfixable) > 0) {
  cat("Partial assimilation:", length(fixed$unfixable),
      "risk(s) require manual fix:",
      paste(fixed$unfixable, collapse = ", "), "\n")
} else {
  cat("Assimilation complete:", length(fixed$fixed),
      "risk(s) corrected\n")
}
#> Partial assimilation: 2 risk(s) require manual fix: index_overlap, duplicate_rows
```

Index overlap requires choosing a new split strategy and cannot be
resolved without user input.

## Migration Checklist

When migrating an existing modelling pipeline to use BORG-guarded CV,
follow these steps:

1.  **Identify the CV step.** Find where your code calls
    [`vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html),
    [`trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html),
    `rsmp("cv")`, or builds manual index vectors.

2.  **Add structure hints.** Determine which columns carry spatial
    coordinates, time stamps, or group identifiers, and pass them to the
    corresponding BORG wrapper.

3.  **Replace the CV constructor.** Swap
    [`vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html)
    for
    [`borg_vfold_cv()`](https://gillescolling.com/BORG/reference/borg_vfold_cv.md),
    [`trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html)
    for
    [`borg_trainControl()`](https://gillescolling.com/BORG/reference/borg_trainControl.md),
    or generate folds with
    [`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md)
    and inject them via `output = "rsample"`, `"caret"`, or `"mlr3"`.

4.  **Validate preprocessing.** Run
    [`borg_inspect()`](https://gillescolling.com/BORG/reference/borg_inspect.md)
    on any prepped recipe or `preProcess` object to confirm it was
    fitted on training data only.

5.  **Run borg_pipeline() on the fitted model.** This catches leakage in
    stages you may have overlooked (nested CV, threshold selection,
    post-processing).

6.  **Compare metrics.** If your blocked CV metrics are substantially
    lower than the random CV metrics, the difference is the inflation
    that was hiding in your original evaluation. This is the real
    performance of your model.

## See Also

- [`vignette("quickstart")`](https://gillescolling.com/BORG/articles/quickstart.md)
  for basic usage and concepts

- [`vignette("risk-taxonomy")`](https://gillescolling.com/BORG/articles/risk-taxonomy.md)
  for the complete catalog of detectable risks
