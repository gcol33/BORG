# Quick Start

## What is BORG?

BORG inspects your preprocessing and model objects to detect data
leakage. It checks whether information from the test set leaked into
training, and blocks evaluation before you compute misleading metrics.

You pass fitted objects to
[`borg()`](https://gillescolling.com/BORG/reference/borg.md) and it
checks for leakage signatures.

## The Problem

Consider a common mistake:

``` r

# PROBLEMATIC WORKFLOW
data <- read.csv("my_data.csv")
data_scaled <- scale(data)  # Fitted on ALL rows

train_idx <- 1:800
test_idx <- 801:1000
train <- data_scaled[train_idx, ]
test <- data_scaled[test_idx, ]

model <- train_model(train)
performance <- evaluate(model, test)  # Inflated!
```

The [`scale()`](https://rdrr.io/r/base/scale.html) call used all 1000
rows to compute mean and SD. Test set statistics leaked into the
training data. The reported performance won’t match real-world results.

## How BORG Catches This

BORG inspects objects after you create them:

``` r

library(BORG)

# You did preprocessing
pp <- preProcess(data, method = c("center", "scale"))

# Now check it for leakage
borg(pp, train_idx = 1:800, test_idx = 801:1000, data = data)
# Error: BORG HARD VIOLATION: preprocessing fitted on 1000 rows, but training set has 800
```

BORG detected that `preProcess` was fitted on more rows than the
training set. The evaluation is blocked before you compute metrics.

## Typical Workflow

``` r

library(BORG)

# 1. Define your split
train_idx <- 1:800
test_idx <- 801:1000

# 2. Do preprocessing on TRAINING ONLY
pp <- preProcess(data[train_idx, ], method = c("center", "scale"))

# 3. Check the preprocessing object
borg(pp, train_idx, test_idx, data = data)
# No error - preprocessing is clean

# 4. Fit model on training data
model <- train(y ~ ., data = train_data, preProcess = pp)

# 5. Check the model
borg(model, train_idx, test_idx, data = data)
# No error - model is clean

# 6. Now safe to evaluate
predictions <- predict(model, test_data)
performance <- compute_metrics(predictions, test_data$y)
```

The key point: call
[`borg()`](https://gillescolling.com/BORG/reference/borg.md) on objects
*after* you create them, *before* you compute metrics.

## What BORG Can Check

Pass any of these to
[`borg()`](https://gillescolling.com/BORG/reference/borg.md):

``` r

# Data frame - checks split validity
borg(data, train_idx, test_idx)

# Preprocessing object - checks for leakage
borg(my_recipe, train_idx, test_idx, data = data)
borg(my_prcomp, train_idx, test_idx, data = data)

# Model - checks training data scope
borg(my_model, train_idx, test_idx, data = data)

# CV object - checks fold structure
borg(my_folds, train_idx, test_idx)
```

## Split Validation

For data frames, BORG checks split validity:

``` r

# Index overlap
borg(data, train_idx = 1:60, test_idx = 50:100)
# Error: BORG HARD VIOLATION: train_idx and test_idx overlap (11 shared indices)

# Group leakage (same patient in train and test)
borg(data, train_idx, test_idx, group_col = "patient_id")

# Temporal leakage (test data predates training)
borg(data, train_idx, test_idx, temporal_col = "date")
```

These checks run immediately - no object inspection needed.

## Risk Classification

**Hard Violations** - Evaluation is invalid. Blocked.

- Index overlap between train and test
- Preprocessing fitted on full data
- Target leakage (feature derived from outcome)
- Temporal look-ahead
- Group membership in both splits

**Soft Inflation** - Results biased but bounded. Warning.

- Spatial block size below autocorrelation range
- Post-hoc subgroup discovery
- Proxy leakage (high correlation with target)

## Other Functions

| Function | Purpose |
|----|----|
| [`borg()`](https://gillescolling.com/BORG/reference/borg.md) | Check any object for leakage |
| [`borg_inspect()`](https://gillescolling.com/BORG/reference/borg_inspect.md) | Detailed risk report |
| [`borg_validate()`](https://gillescolling.com/BORG/reference/borg_validate.md) | Check a complete workflow list |
| [`borg_rewrite()`](https://gillescolling.com/BORG/reference/borg_rewrite.md) | Attempt automatic repair |

## Next Steps

- [Risk
  Taxonomy](https://gillescolling.com/BORG/articles/risk-taxonomy.md):
  Full catalog of detectable risks
- [Framework
  Integration](https://gillescolling.com/BORG/articles/frameworks.md):
  Using BORG with caret, tidymodels, mlr3
