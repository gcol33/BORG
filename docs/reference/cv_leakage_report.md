# Generate CV Leakage Report

Generates a detailed report of cross-validation leakage issues.

## Usage

``` r
cv_leakage_report(cv_object, train_idx, test_idx)
```

## Arguments

- cv_object:

  A cross-validation object (trainControl, vfold_cv, etc.).

- train_idx:

  Integer vector of training indices.

- test_idx:

  Integer vector of test indices.

## Value

A list with detailed CV leakage information.

## Examples

``` r
# Using caret trainControl
if (requireNamespace("caret", quietly = TRUE)) {
  folds <- list(Fold1 = 1:10, Fold2 = 11:20, Fold3 = 21:25)
  ctrl <- caret::trainControl(method = "cv", index = folds)
  report <- cv_leakage_report(ctrl, train_idx = 1:25, test_idx = 26:32)
  print(report)
}
```
