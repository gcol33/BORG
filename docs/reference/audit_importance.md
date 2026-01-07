# Audit Feature Importance Calculations

Detects when feature importance (SHAP, permutation importance, etc.) is
computed using test data, which can lead to biased feature selection and
data leakage.

## Usage

``` r
audit_importance(
  importance,
  data,
  train_idx,
  test_idx,
  method = "auto",
  model = NULL
)
```

## Arguments

- importance:

  A vector, matrix, or data frame of importance values.

- data:

  The data used to compute importance.

- train_idx:

  Integer vector of training indices.

- test_idx:

  Integer vector of test indices.

- method:

  Character indicating the importance method. One of "shap",
  "permutation", "gain", "impurity", or "auto" (default).

- model:

  Optional fitted model object for additional validation.

## Value

A BorgRisk object with audit results.

## Details

Feature importance computed on test data is a form of data leakage
because:

- SHAP values computed on test data reveal test set structure

- Permutation importance on test data uses test labels

- Feature selection based on test importance leads to overfit models

This function checks if the data used for importance calculation
includes test indices and flags potential violations.

## Examples

``` r
set.seed(42)
data <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
train_idx <- 1:70
test_idx <- 71:100

# Simulate importance values
importance <- c(x1 = 0.6, x2 = 0.4)

# Good: importance computed on training data
result <- audit_importance(importance, data[train_idx, ], train_idx, test_idx)

# Bad: importance computed on full data (includes test)
result_bad <- audit_importance(importance, data, train_idx, test_idx)
```
