# Changelog

## BORG 0.1.0

Initial release.

### Core functionality

- [`borg_guard()`](https://gillescolling.com/BORG/reference/borg_guard.md):
  Creates a validation context for train/test splits with support for
  temporal, spatial, and grouped structures
- [`borg_validate()`](https://gillescolling.com/BORG/reference/borg_validate.md):
  Comprehensive workflow validation including:
  - Index overlap detection
  - Duplicate row detection
  - Target leakage detection (suspicious naming, perfect separation)
  - Feature engineering leakage (global standardization, rank features)
  - Threshold selection leakage
  - Spatial autocorrelation checks
  - HPO validation checks (test data usage, non-nested CV)
- [`borg_inspect()`](https://gillescolling.com/BORG/reference/borg_inspect.md):
  Inspects preprocessing objects for data leakage:
  - caret `preProcess` objects

  - caret `trainControl` objects

  - tidymodels `recipe` objects

  - `prcomp` PCA objects

  - rsample resampling objects
- [`borg_rewrite()`](https://gillescolling.com/BORG/reference/borg_rewrite.md):
  Attempts automatic fixes for detected violations
- `BorgRisk` S4 class for structured risk assessment reports

### Performance

- C++ backends via Rcpp for fast hash-based index overlap and duplicate
  detection

### Framework integration

- caret: `preProcess`, `trainControl`, `train` objects
- tidymodels: `recipe`, `rsplit`, `vfold_cv`, `rset` objects
- mlr3: task and resampling validation
- Base R: manual index-based splits
