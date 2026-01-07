# BORG

Bounded Outcome Risk Guard

BORG detects invalid model evaluation. It identifies when information
flows from test data into training, and blocks the evaluation before
misleading metrics are produced.

## Installation

``` r

# install.packages("remotes")
remotes::install_github("gcol33/BORG")
```

## The Problem

``` r

# This code produces inflated performance estimates
data <- read.csv("data.csv")
data_scaled <- scale(data[, -1])
train <- data_scaled[1:800, ]
test <- data_scaled[801:1000, ]
model <- lm(y ~ ., data = train)
rmse <- sqrt(mean((test$y - predict(model, test))^2))
```

The call to [`scale()`](https://rdrr.io/r/base/scale.html) computed mean
and standard deviation using all 1000 rows. The test set’s distribution
is now encoded in the training data. The reported RMSE does not estimate
performance on unseen data.

## The Solution

``` r

library(BORG)

# One function does it all
borg(data, train_idx = 1:800, test_idx = 801:1000)
# Error: BORG HARD VIOLATION: preprocessing fitted on full data
```

BORG fails closed. The evaluation does not proceed.

## Usage

``` r

# Validate a train/test split
borg(data, train_idx = 1:800, test_idx = 801:1000)

# Check a preprocessing object
borg(my_recipe, train_idx, test_idx, data = data)

# Check a fitted model
borg(my_model, train_idx, test_idx, data = data)

# Check a CV object
borg(my_folds, train_idx, test_idx)
```

## Functions

| Function | Purpose |
|----|----|
| [`borg()`](https://gillescolling.com/BORG/reference/borg.md) | **Main entry point** — auto-detects and validates |
| [`borg_guard()`](https://gillescolling.com/BORG/reference/borg_guard.md) | Create guarded evaluation context |
| [`borg_inspect()`](https://gillescolling.com/BORG/reference/borg_inspect.md) | Detailed object inspection |
| [`borg_validate()`](https://gillescolling.com/BORG/reference/borg_validate.md) | Validate complete workflow |
| [`borg_rewrite()`](https://gillescolling.com/BORG/reference/borg_rewrite.md) | Attempt automatic repair |

## Risk Classification

**Hard Violation** — Evaluation is invalid. Blocked. - Train-test index
overlap - Preprocessing fitted on full data - Target leakage (feature
derived from outcome) - Temporal look-ahead - Group membership in both
splits

**Soft Inflation** — Results biased but bounded. Constrained. - Spatial
block size below autocorrelation range - Embargo period below serial
dependence - Post-hoc subgroup discovery

## Design Principles

1.  BORG is a gate, not a linter
2.  Fail closed on ambiguity
3.  No metrics computed from invalid evaluations
4.  Risk defined at outcome validity, not technique correctness

## Documentation

- [Quick Start](https://gillescolling.com/BORG/articles/quickstart.html)
- [Risk
  Taxonomy](https://gillescolling.com/BORG/articles/risk-taxonomy.html)
- [Framework
  Integration](https://gillescolling.com/BORG/articles/frameworks.html)
- [Enforcement Surface
  Specification](https://gillescolling.com/BORG/ENFORCEMENT_SURFACE.html)

## License

MIT
