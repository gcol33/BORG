# BORG
Bounded Outcome Risk Guard

BORG detects invalid model evaluation. It identifies when information flows from test data into training, and blocks the evaluation before misleading metrics are produced.

## Why BORG?

You won't know your evaluation is broken. The code runs fine, metrics look good, you publish or deploy. Months later you find out your 94% accuracy is 70% on real data.

A [Princeton meta-analysis](https://reproducible.cs.princeton.edu/) found leakage errors in **648 published papers across 30 scientific fields**. In civil war prediction research, correcting leakage revealed that "complex ML models do not perform substantively better than decades-old Logistic Regression." The reported gains were artifacts.

BORG catches these errors before you compute metrics. It refuses to proceed when evaluation is invalid.

## Installation

```r
# install.packages("remotes")
remotes::install_github("gcol33/BORG")
```

## The Problem

```r
# This code produces inflated performance estimates
data <- read.csv("data.csv")
data_scaled <- scale(data[, -1])
train <- data_scaled[1:800, ]
test <- data_scaled[801:1000, ]
model <- lm(y ~ ., data = train)
rmse <- sqrt(mean((test$y - predict(model, test))^2))
```

The call to `scale()` computed mean and standard deviation using all 1000 rows. This leaks test set information into training. The reported RMSE won't match real-world performance.

## The Solution

```r
library(BORG)

# One function does it all
borg(data, train_idx = 1:800, test_idx = 801:1000)
# Error: BORG HARD VIOLATION: preprocessing fitted on full data
```

BORG fails closed. No metrics get computed.

## Usage

```r
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
|----------|---------|
| `borg()` | Validate any object against a train/test split |
| `borg_inspect()` | Detailed object inspection |
| `borg_validate()` | Validate complete workflow |
| `borg_rewrite()` | Attempt automatic repair |

## Risk Classification

**Hard Violation**: Evaluation is invalid. Blocked.

- Train-test index overlap
- Preprocessing fitted on full data
- Target leakage (feature derived from outcome)
- Temporal look-ahead
- Group membership in both splits

**Soft Inflation**: Results biased but bounded. Constrained.

- Spatial block size below autocorrelation range
- Embargo period below serial dependence
- Post-hoc subgroup discovery

## Design Principles

1. BORG is a gate, not a linter
2. Fail closed on ambiguity
3. No metrics computed from invalid evaluations
4. Risk defined at outcome validity, not technique correctness

## Documentation

- [Quick Start](https://gillescolling.com/BORG/articles/quickstart.html)
- [Risk Taxonomy](https://gillescolling.com/BORG/articles/risk-taxonomy.html)
- [Framework Integration](https://gillescolling.com/BORG/articles/frameworks.html)
- [Enforcement Surface Specification](https://gillescolling.com/BORG/ENFORCEMENT_SURFACE.html)

## License

MIT
