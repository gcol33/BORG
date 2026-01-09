# BORG

[![R-CMD-check](https://github.com/gcol33/BORG/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gcol33/BORG/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Bounded Outcome Risk Guard for Model Evaluation**

BORG detects data leakage and invalid cross-validation setups before you compute performance metrics. It checks for information reuse between training and test data, and blocks evaluation when problems are found.

## Quick Start

```r
library(BORG)

# Validate a train/test split
data <- iris
train_idx <- 1:100
test_idx <- 101:150

borg(data, train_idx = train_idx, test_idx = test_idx)

# Detect overlapping indices
borg(data, train_idx = 1:100, test_idx = 51:150)
#> Error: index_overlap - Train and test indices overlap (50 shared indices)
```

## Statement of Need

A model shows 95% accuracy on test data, then drops to 60% in production. The usual cause: data leakage. Information from the test set contaminated training, and the reported metrics were wrong.

A [Princeton meta-analysis](https://reproducible.cs.princeton.edu/) found leakage errors in **648 published papers across 30 fields**. In civil war prediction research, correcting leakage revealed that "complex ML models do not perform substantively better than decades-old Logistic Regression." The reported gains were artifacts.

BORG catches these errors before metrics are computed.

## Features

### Core Validation

- **`borg()`**: Main entry point for all validation
  - Validates train/test splits against data
  - Detects preprocessing leakage (scaling, PCA fitted on full data)
  - Checks for target leakage (features derived from outcome)
  - Validates grouped data (same patient in train and test)
  - Validates temporal data (test predates training)
  - Validates spatial data (test points too close to training)

- **`borg_inspect()`**: Detailed inspection of specific objects
  - Works with `caret::preProcess`, `recipes::recipe`, `prcomp`
  - Checks `rsample` resampling objects
  - Validates fitted models (`lm`, `glm`, `ranger`, etc.)

- **`borg_diagnose()`**: Analyze data for dependency structure
  - Detects spatial autocorrelation (Moran's I)
  - Detects temporal autocorrelation (ACF/Ljung-Box)
  - Detects clustered structure (ICC)
  - Recommends appropriate CV strategy

### Risk Categories

| Category | Impact | Response |
|----------|--------|----------|
| **Hard Violation** | Results invalid | Blocks evaluation |
| **Soft Inflation** | Results biased | Warns, allows with caution |

**Hard Violations:**
- `index_overlap` - Same row in train and test
- `duplicate_rows` - Identical observations across sets
- `preprocessing_leak` - Scaler/PCA fitted on full data
- `target_leakage` - Feature with |r| > 0.99 with target
- `group_leakage` - Same group in train and test
- `temporal_leak` - Test data predates training

**Soft Inflation:**
- `proxy_leakage` - Feature with |r| 0.95-0.99 with target
- `spatial_proximity` - Test points close to training
- `spatial_overlap` - Test inside training convex hull

## Installation

```r
# Install from GitHub
# install.packages("pak")
pak::pak("gcol33/BORG")
```

## Usage Examples

### Basic Validation

```r
library(BORG)
data <- iris
train_idx <- 1:100
test_idx <- 101:150

# Returns BorgRisk object with validation results
result <- borg(data, train_idx = train_idx, test_idx = test_idx)
result
```

### Detecting Preprocessing Leakage

```r
# BAD: scale() fitted on all data before splitting
data_scaled <- scale(iris[, 1:4])
borg_inspect(data_scaled, train_idx = 1:100, test_idx = 101:150)
#> Hard violation: preprocessing_leak
```

### Target Leakage Detection

```r
# Feature highly correlated with outcome
leaky_data <- data.frame(
 x = rnorm(100),
 outcome = rnorm(100)
)
leaky_data$leaked <- leaky_data$outcome + rnorm(100, sd = 0.01)

borg_inspect(leaky_data, train_idx = 1:70, test_idx = 71:100, target = "outcome")
#> Hard violation: target_leakage_direct
```
### Grouped Data Validation

```r
# Clinical data with patient IDs
clinical <- data.frame(
  patient_id = rep(1:10, each = 10),
  measurement = rnorm(100)
)

# Random split ignoring patients
set.seed(123)
idx <- sample(100)
train_idx <- idx[1:70]
test_idx <- idx[71:100]

borg_inspect(clinical, train_idx, test_idx, groups = "patient_id")
#> Hard violation: group_leakage
```

### Spatial Data Validation

```r
spatial_data <- data.frame(
  lon = runif(200, -10, 10),
  lat = runif(200, -10, 10),
  response = rnorm(200)
)

# Let BORG diagnose and generate appropriate CV folds
result <- borg(spatial_data, coords = c("lon", "lat"), target = "response", v = 5)
result$diagnosis@recommended_cv
#> "spatial_block"
```

### Framework Integration

BORG works with common ML frameworks:

```r
# caret
library(caret)
pp <- preProcess(mtcars[, -1], method = c("center", "scale"))
borg_inspect(pp, train_idx = 1:25, test_idx = 26:32, data = mtcars)

# tidymodels
library(recipes)
rec <- recipe(mpg ~ ., data = mtcars) |>
  step_normalize(all_numeric_predictors()) |>
  prep()
borg_inspect(rec, train_idx = 1:25, test_idx = 26:32, data = mtcars)
```

## Interface Summary

| Function | Purpose |
|----------|---------|
| `borg()` | Main entry point - diagnose data or validate splits |
| `borg_inspect()` | Detailed inspection of objects |
| `borg_diagnose()` | Analyze data dependencies |
| `borg_validate()` | Validate complete workflow |
| `borg_rewrite()` | Attempt automatic repair |
| `plot()` | Visualize results |
| `summary()` | Generate methods text |
| `borg_certificate()` | Create validation certificate |
| `borg_export()` | Export certificate to YAML/JSON |

## Documentation

- [Quick Start](https://gcol33.github.io/BORG/articles/quickstart.html)
- [Risk Taxonomy](https://gcol33.github.io/BORG/articles/risk-taxonomy.html)
- [Framework Integration](https://gcol33.github.io/BORG/articles/frameworks.html)

## License

MIT (see the LICENSE.md file)
