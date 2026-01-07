# BORG TODO List

## High Priority - Test Coverage

- [x] Add tests for `borg_validate()` function
- [x] Add tests for `borg_rewrite()` function
- [x] Add tests for preprocessing object inspection (preProcess, recipe, prcomp)
- [x] Add integration tests with caret
- [x] Add integration tests with tidymodels/recipes
- [x] Add edge case tests for temporal validation
- [x] Add edge case tests for spatial validation
- [x] Add edge case tests for group validation

## Medium Priority - Detection Logic

- [x] Implement target leakage detection (beyond simple correlation)
  - Suspicious naming patterns (outcome_, future_, _target, etc.)
  - Perfect categorical separation
  - Temporal ordering checks
  - Group mean leakage detection
- [x] Implement feature engineering inspection
  - Global standardization detection
  - Rank/percentile feature checks
  - Binning leakage detection
  - Lag/lead feature checks for time series
- [x] Implement threshold selection detection
  - Threshold optimized on test data
  - Threshold selection using test predictions
  - Post-hoc threshold adjustment
- [x] Implement spatial autocorrelation checks
  - Spatial proximity between train/test
  - Spatial duplicate locations
  - Spatial block overlap
  - Buffer zone violations
- [x] Implement HPO validation checks
  - HPO using test data
  - Model selection on test data
  - Non-nested CV warnings
  - Excessive configuration warnings
  - Feature selection leakage in HPO
  - Early stopping on test data

## Low Priority - Documentation

- [x] Make `frameworks.Rmd` vignette code executable (remove `eval = FALSE`)
- [x] Add CHANGELOG.md (NEWS.md)
- [x] Add runnable demonstrations of framework integration
