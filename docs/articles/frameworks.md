# Framework Integration

BORG integrates with major R machine learning frameworks. This guide
shows how to validate workflows in each ecosystem.

## Base R

Manual index-based splitting:

``` r

library(BORG)

# Create data and split
data <- iris
set.seed(42)
n <- nrow(data)
train_idx <- sample(n, 0.7 * n)
test_idx <- setdiff(1:n, train_idx)

# Guard the evaluation
ctx <- borg_guard(
  data = data,
  train_idx = train_idx,
  test_idx = test_idx,
  mode = "strict"
)

# Safe preprocessing (train only)
train_data <- data[train_idx, ]
train_means <- colMeans(train_data[, 1:4])
train_sds <- apply(train_data[, 1:4], 2, sd)

# Apply to both sets using TRAIN statistics
data[, 1:4] <- scale(data[, 1:4], center = train_means, scale = train_sds)
```

## caret

Validate `trainControl` and `preProcess` objects:

``` r

library(caret)
library(BORG)

data(mtcars)
train_idx <- 1:25
test_idx <- 26:32

# BAD: preProcess on full data
pp_bad <- preProcess(mtcars[, -1], method = c("center", "scale"))
result <- borg_inspect(pp_bad, train_idx, test_idx, data = mtcars)
# Detects preprocessing leak

# GOOD: preProcess on train only
pp_good <- preProcess(mtcars[train_idx, -1], method = c("center", "scale"))
result <- borg_inspect(pp_good, train_idx, test_idx, data = mtcars)
# No violations

# Inspect trainControl
ctrl <- trainControl(
  method = "cv",
  number = 5,
  index = createFolds(mtcars$mpg[train_idx], k = 5)
)
result <- borg_inspect(ctrl, train_idx, test_idx)
```

## tidymodels / recipes

Validate recipe objects:

``` r

library(recipes)
library(rsample)
library(BORG)

data(ames, package = "modeldata")

# Create split
split <- initial_split(ames, prop = 0.8)
train_idx <- split$in_id
test_idx <- setdiff(1:nrow(ames), train_idx)

# BAD: recipe prepped on full data
rec_bad <- recipe(Sale_Price ~ ., data = ames) %>%
  step_normalize(all_numeric_predictors()) %>%
  prep()  # Uses full ames data!

result <- borg_inspect(rec_bad, train_idx, test_idx, data = ames)
# Detects leak

# GOOD: recipe prepped on training only
rec_good <- recipe(Sale_Price ~ ., data = training(split)) %>%
  step_normalize(all_numeric_predictors()) %>%
  prep()

result <- borg_inspect(rec_good, train_idx, test_idx, data = ames)
# Clean
```

### rsample Objects

Inspect resampling schemes:

``` r

# Validate v-fold CV
folds <- vfold_cv(training(split), v = 5)
result <- borg_inspect(folds, train_idx, test_idx)

# Validate grouped CV
group_folds <- group_vfold_cv(data, group = patient_id, v = 5)
result <- borg_inspect(group_folds, train_idx, test_idx)

# Validate temporal splits
rolling <- sliding_window(ts_data, lookback = 100, assess_stop = 50)
result <- borg_inspect(rolling, train_idx, test_idx)
```

## mlr3

Validate tasks and resamplings:

``` r

library(mlr3)
library(BORG)

# Create task
task <- TaskClassif$new("iris", iris, target = "Species")

# Create resampling
resampling <- rsmp("cv", folds = 5)
resampling$instantiate(task)

# Inspect
train_idx <- resampling$train_set(1)
test_idx <- resampling$test_set(1)
result <- borg_inspect(task, train_idx, test_idx)
```

## Temporal Data

For time series or panel data, enable temporal validation:

``` r

# Expanding window backtest
ctx <- borg_guard(
  data = stock_data,
  train_idx = 1:252,  # First year
  test_idx = 253:504,  # Second year
  temporal_col = "date",
  mode = "strict"
)

# Rolling origin with rsample
rolling <- rolling_origin(
  data = ts_data,
  initial = 365,
  assess = 30,
  cumulative = FALSE
)
result <- borg_inspect(rolling, train_idx = NULL, test_idx = NULL)
```

## Spatial Data

For spatial cross-validation:

``` r

library(sf)

# Block CV with spatial awareness
ctx <- borg_guard(
  data = spatial_data,
  train_idx = train_idx,
  test_idx = test_idx,
  spatial_cols = c("longitude", "latitude"),
  mode = "warn"
)

# Inspects spatial separation between train/test
```

## Complete Workflow Validation

Validate an entire pipeline after the fact:

``` r

# Run your existing workflow
# ... preprocessing, training, prediction ...

# Then validate
result <- borg_validate(list(
  data = original_data,
  train_idx = train_idx,
  test_idx = test_idx,
  preprocess = list(recipe_obj, pca_obj),
  model = fitted_model,
  predictions = test_predictions,
  target_col = "outcome"
))

if (!result@is_valid) {
  print(result)
  stop("Evaluation invalid - see risk report above")
}
```

## Automatic Rewriting

Attempt to fix leaky pipelines:

``` r

# Attempt automatic fixes
fixed <- borg_rewrite(workflow)

if (length(fixed$unfixable) > 0) {
  warning("Could not fix: ", paste(fixed$unfixable, collapse = ", "))
}

# Use corrected workflow
new_workflow <- fixed$workflow
```

Note: Some violations cannot be automatically fixed (e.g., index overlap
requires a new split).
