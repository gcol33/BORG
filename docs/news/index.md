# Changelog

## BORG 0.2.0 (development)

### New features

#### Visualization functions

- [`plot_split()`](https://gillescolling.com/BORG/reference/plot_split.md):
  Visualize train/test split distribution with temporal or group
  structure
- [`plot_risk()`](https://gillescolling.com/BORG/reference/plot_risk.md):
  Display risk assessment results as horizontal bar chart
- [`plot_temporal()`](https://gillescolling.com/BORG/reference/plot_temporal.md):
  Timeline visualization with gap analysis and look-ahead detection
- [`plot_spatial()`](https://gillescolling.com/BORG/reference/plot_spatial.md):
  Spatial split visualization with convex hulls
- [`plot_groups()`](https://gillescolling.com/BORG/reference/plot_groups.md):
  Group-based split visualization with leakage highlighting

#### Model inspection

- Extended
  [`borg_inspect()`](https://gillescolling.com/BORG/reference/borg_inspect.md)
  to support fitted model objects:
  - `lm` and `glm` models (checks data used in fitting)
  - `ranger` random forest models
  - `xgboost` models
  - `lightgbm` models
  - `parsnip` model fits
  - `workflow` objects (tidymodels)

#### Audit functions

- [`audit_predictions()`](https://gillescolling.com/BORG/reference/audit_predictions.md):
  Validate prediction vectors against expected indices
- [`cv_leakage_report()`](https://gillescolling.com/BORG/reference/cv_leakage_report.md):
  Generate detailed cross-validation leakage reports
- [`audit_importance()`](https://gillescolling.com/BORG/reference/audit_importance.md):
  Detect feature importance computed on test data (SHAP, permutation)

#### Tidymodels integration

- Added `tune_results` inspection for tidymodels tuning objects
- Detects when hyperparameter tuning uses test data in resamples

#### Configuration

- Added
  [`borg_auto_check()`](https://gillescolling.com/BORG/reference/borg_auto_check.md)
  to enable/disable automatic validation
- Added
  [`borg_options()`](https://gillescolling.com/BORG/reference/borg_options.md)
  to query current configuration
- New options: `borg.auto_check`, `borg.strict`, `borg.verbose`

### Bug fixes

- Fixed rsample vfold_cv inspection for attribute structure changes

## BORG 0.1.0

Initial release.

### Core functionality

- `borg_guard()`: Creates a validation context for train/test splits
  with support for temporal, spatial, and grouped structures
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
