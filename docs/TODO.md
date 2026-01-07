# BORG TODO List

## High Priority - Test Coverage

Add tests for
[`borg_validate()`](https://gillescolling.com/BORG/reference/borg_validate.md)
function

Add tests for
[`borg_rewrite()`](https://gillescolling.com/BORG/reference/borg_rewrite.md)
function

Add tests for preprocessing object inspection (preProcess, recipe,
prcomp)

Add integration tests with caret

Add integration tests with tidymodels/recipes

Add edge case tests for temporal validation

Add edge case tests for spatial validation

Add edge case tests for group validation

## Medium Priority - Detection Logic

Implement target leakage detection (beyond simple correlation)

- Suspicious naming patterns (outcome\_, future\_, \_target, etc.)
- Perfect categorical separation
- Temporal ordering checks
- Group mean leakage detection

Implement feature engineering inspection

- Global standardization detection
- Rank/percentile feature checks
- Binning leakage detection
- Lag/lead feature checks for time series

Implement threshold selection detection

- Threshold optimized on test data
- Threshold selection using test predictions
- Post-hoc threshold adjustment

Implement spatial autocorrelation checks

- Spatial proximity between train/test
- Spatial duplicate locations
- Spatial block overlap
- Buffer zone violations

Implement HPO validation checks

- HPO using test data
- Model selection on test data
- Non-nested CV warnings
- Excessive configuration warnings
- Feature selection leakage in HPO
- Early stopping on test data

## Low Priority - Documentation

Make `frameworks.Rmd` vignette code executable (remove `eval = FALSE`)

Add CHANGELOG.md (NEWS.md)

Add runnable demonstrations of framework integration
