# Audit Predictions for Data Leakage

Validates that predictions were generated correctly without data
leakage. Checks that predictions correspond to test data only and that
the prediction process did not use information from the test set.

## Usage

``` r
audit_predictions(
  predictions,
  train_idx,
  test_idx,
  actual = NULL,
  data = NULL,
  model = NULL
)
```

## Arguments

- predictions:

  Vector of predictions (numeric or factor).

- train_idx:

  Integer vector of training indices.

- test_idx:

  Integer vector of test indices.

- actual:

  Optional vector of actual values for comparison.

- data:

  Optional data frame containing the original data.

- model:

  Optional fitted model object for additional checks.

## Value

A BorgRisk object with audit results.

## Examples

``` r
# Create data and split
set.seed(42)
data <- data.frame(y = rnorm(100), x = rnorm(100))
train_idx <- 1:70
test_idx <- 71:100

# Fit model and predict
model <- lm(y ~ x, data = data[train_idx, ])
predictions <- predict(model, newdata = data[test_idx, ])

# Audit predictions
result <- audit_predictions(predictions, train_idx, test_idx)
```
