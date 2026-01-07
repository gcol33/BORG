# Quick Start

## What is BORG?

BORG (Bounded Outcome Risk Guard) automatically detects when model
evaluation is compromised by information leakage between training and
test data. It identifies violations that inflate performance estimates
and provides actionable remediation.

## The Problem

Consider a common machine learning workflow:

``` r

# Typical workflow (PROBLEMATIC)
data <- read.csv("my_data.csv")

# Preprocessing on FULL data - LEAKS TEST INFO
data_scaled <- scale(data)

# Split AFTER preprocessing
train_idx <- 1:800
test_idx <- 801:1000
train <- data_scaled[train_idx, ]
test <- data_scaled[test_idx, ]

# Model training and evaluation
model <- train_model(train)
performance <- evaluate(model, test)  # Inflated!
```

The problem: [`scale()`](https://rdrr.io/r/base/scale.html) computed
mean and standard deviation using ALL data, including the test set. This
means:

- Training data contains test set statistics
- Test performance is artificially inflated
- Results do not generalize to truly unseen data

## BORG Solution

### Proactive Guarding

Use
[`borg_guard()`](https://gillescolling.com/BORG/reference/borg_guard.md)
to create a protected evaluation context:

``` r

library(BORG)

# Create guarded context BEFORE any preprocessing
ctx <- borg_guard(
  data = my_data,
  train_idx = 1:800,

  test_idx = 801:1000,
  mode = "strict"
)
# Mode options:
#   "strict"  - Error on any violation
#   "warn"    - Warn but continue
#   "rewrite" - Auto-fix where possible
```

### Object Inspection

Inspect existing objects for leakage:

``` r

# Check a preprocessing object
pp <- preProcess(full_data, method = c("center", "scale"))
result <- borg_inspect(pp, train_idx = 1:800, test_idx = 801:1000, data = full_data)

if (!result@is_valid) {
  print(result)  # Detailed risk report
}
```

### Workflow Validation

Validate an entire evaluation pipeline:

``` r

result <- borg_validate(list(
  data = my_data,
  train_idx = train_idx,
  test_idx = test_idx,
  preprocess = my_recipe,
  model = fitted_model
))

# Check results
result@is_valid      # TRUE/FALSE
result@n_hard        # Count of hard violations
result@n_soft        # Count of soft warnings
as.data.frame(result)  # Tabular risk summary
```

## Risk Classification

BORG classifies risks into two categories:

### Hard Violations (Block)

These invalidate evaluation entirely:

- **Index overlap**: Same rows in train and test
- **Preprocessing leak**: Normalization/imputation fitted on full data
- **Target leakage**: Features derived from the outcome
- **Temporal look-ahead**: Using future information for past predictions

### Soft Inflation (Warn)

These bias results but may preserve model ranking:

- **Insufficient spatial blocking**: Block size smaller than
  autocorrelation range
- **Post-hoc subgroup analysis**: Discovering subgroups on test data
- **Proxy leakage**: Features suspiciously correlated with target

## Common Patterns

### Cross-Validation

``` r

# BAD: preprocessing before CV
data_scaled <- scale(data)
cv_results <- cross_validate(data_scaled)  # Leaky!

# GOOD: preprocessing inside CV folds
ctx <- borg_guard(data, train_idx, test_idx, mode = "strict")
# Preprocessing must happen per-fold
```

### Temporal Splits

``` r

# Enable temporal validation
ctx <- borg_guard(
  data = ts_data,
  train_idx = 1:800,
  test_idx = 801:1000,
  temporal_col = "date"  # Validates ordering
)
```

### Grouped Data

``` r

# Ensure group isolation
ctx <- borg_guard(
  data = patient_data,
  train_idx = train_idx,
  test_idx = test_idx,
  group_col = "patient_id"  # No patient in both sets
)
```

## Next Steps

- [Risk
  Taxonomy](https://gillescolling.com/BORG/articles/risk-taxonomy.md):
  Full catalog of detectable risks
- [Framework
  Integration](https://gillescolling.com/BORG/articles/frameworks.md):
  Using BORG with caret, tidymodels, mlr3
