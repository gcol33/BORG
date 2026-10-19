# Changelog

## BORG 0.3.2 (development)

### Correctness fixes

- `borg_cv(strategy = "group_fold")` now assigns groups to folds by
  their label rather than by position. Numeric group identifiers whose
  values were not `1:n_groups` (e.g. site codes) previously produced
  empty test folds and full-data training sets.
- Stochastic fold generators (`spatial_block`, `environmental_block`,
  `leave_location_out`, `llto`, `knndm`, `custom_block`, random) now
  accept a `seed` argument threaded from `borg_cv(seed = )` and
  [`borg_repeated_cv()`](https://gillescolling.com/BORG/reference/borg_repeated_cv.md)
  instead of a hardcoded internal seed. Repeated CV now produces
  genuinely distinct fold sets across repeats for every strategy.
- [`borg_block_size()`](https://gillescolling.com/BORG/reference/borg_block_size.md)
  now varies the folds with the candidate block size (the block size
  drives the number of spatial clusters), so the reported optimum
  reflects the data rather than the search grid.
- Temporal block CV no longer backfills small early folds with future
  observations. Training is strictly past-only; the first block (with no
  prior data) is dropped, so `v` test blocks yield up to `v - 1` usable
  folds.
- `borg_conformal(method = "split")` refits the model on the complement
  of the calibration set so the nonconformity scores are genuinely
  out-of-sample, and uses the conformal order statistic (`type = 1`) for
  the quantile. Split intervals now reach their nominal coverage.
- [`borg_di()`](https://gillescolling.com/BORG/reference/borg_di.md) /
  [`borg_aoa()`](https://gillescolling.com/BORG/reference/borg_aoa.md)
  follow Meyer & Pebesma (2021): the dissimilarity index is normalized
  by the mean pairwise training distance, and the applicability
  threshold is the outlier-adjusted maximum (Q75 + 1.5 \* IQR) of the
  cross-validated training DI.
- Spatial buffering and distance-based diagnostics use Haversine
  distance when a geographic CRS is available.
  [`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md) now
  threads the CRS to the buffer, and
  [`borg_block_size()`](https://gillescolling.com/BORG/reference/borg_block_size.md)
  /
  [`borg_check_residuals()`](https://gillescolling.com/BORG/reference/borg_check_residuals.md)
  accept a `crs` argument.
- [`borg_willmott()`](https://gillescolling.com/BORG/reference/borg_willmott.md)
  computes the refined index `d1` with the correct denominator;
  [`borg_local_moran()`](https://gillescolling.com/BORG/reference/borg_local_moran.md)
  uses conditional permutation for its pseudo p-values.
- The C++ duplicate-row check compares rows at full double precision
  instead of truncating to six decimals, and the compiled leakage checks
  validate their index arguments before dereferencing.

### Internal

- Added parameter-recovery and coverage tests for the statistical
  estimators (`borg_conformal`, `borg_aoa`, `borg_willmott`,
  `borg_calibration`, `borg_bootstrap`, `borg_debias`).
- Consolidated the two metric dispatchers into a single implementation
  and the train-scope leak check into a shared helper; removed dead
  code.
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  for `borg_result` now offer the same set of plot types (`split`,
  `risk`, `spatial`, `temporal`, `groups`).

## BORG 0.3.1

CRAN release: 2026-03-29

### Nearest-neighbour distance matching (NNDM)

- New `borg_cv(strategy = "nndm", prediction_points = ...)` implements
  the leave-one-out NNDM scheme of Mila et al. (2022): each held-out
  point grows an exclusion radius until the test-to-train
  nearest-neighbour distance distribution matches the
  prediction-to-sample distribution, retaining at least half the data
  per fold. Complements the existing `"knndm"` k-fold form.
- Prediction-coordinate handling for `"knndm"` and `"nndm"` now shares
  one internal helper.

### Clustered spatial sampling

- New
  [`borg_clustered_sample()`](https://gillescolling.com/BORG/reference/borg_clustered_sample.md)
  draws parent-offspring clustered point samples over a bounding box,
  coordinate frame, `sf` polygon, or `SpatVector`. Useful for
  simulations and for reproducing the spatial dependence that random CV
  understates.

### Normalized dissimilarity index

- `borg_aoa(normalize = TRUE)` adds a `di_norm` column rescaling the
  dissimilarity index by the AOA threshold, so `di_norm = 1` marks the
  applicability boundary and values are comparable across models.

## BORG 0.2.7

### Suggested fixes in risk reports

- `show(BorgRisk)` now prints actionable suggested fixes for each
  detected risk
- `as.data.frame(BorgRisk)` includes a `suggested_fix` column
- Risks fixable by
  [`borg_assimilate()`](https://gillescolling.com/BORG/reference/borg_assimilate.md)
  are labeled with the auto-fix call
- Manual-intervention risks provide specific guidance (e.g., “use
  spatial block CV”)

### Improved ggplot2 autoplot methods

- `autoplot(BorgRisk)`: Lollipop chart with fix annotations, point size
  encodes affected indices
- `autoplot(borg_result, type = "temporal")`: New temporal split
  visualization with look-ahead violation detection
- `autoplot(borg_result, type = "groups")`: New group assignment bar
  chart with overlap highlighting
- `autoplot(BorgDiagnosis)`: Richer panel with detection threshold gauge
  bars

### Better terra/sf spatial integration

- Spatial autoplot now uses
  [`tidyterra::geom_spatvector()`](https://dieghernan.github.io/tidyterra/reference/ggspatvector.html)
  for native SpatVector rendering when available
- `sf` objects preserve geometry directly (no coordinate extraction +
  reconstruction round-trip)
- Faceted spatial CV plots work natively with both sf and SpatVector
  inputs
- Added `tidyterra` to Suggests

------------------------------------------------------------------------

## BORG 0.2.3

### Native R Integration

This release replaces custom functions with standard R S3 methods for a
more idiomatic interface.

#### S3 methods for plotting

- `plot(BorgRisk)`: Visualize risk assessment results
- `plot(borg_result)`: Visualize CV fold splits
- `plot(borg_comparison)`: Compare random vs blocked CV results

#### S3 methods for summaries

- `summary(BorgDiagnosis)`: Generate methods section text for
  publications
- `summary(BorgRisk)`: Summarize detected risks
- `summary(borg_result)`: Generate methods text from borg() output
- `summary(borg_comparison)`: Detailed comparison summary

#### Dedicated export functions

- [`borg_certificate()`](https://gillescolling.com/BORG/reference/borg_certificate.md):
  Create structured validation certificate
- [`borg_export()`](https://gillescolling.com/BORG/reference/borg_export.md):
  Write certificate to YAML/JSON file

#### Removed

- `borg_plot()` - use
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) instead
- [`borg_report()`](https://gillescolling.com/BORG/reference/borg_report.md) -
  use [`summary()`](https://rdrr.io/r/base/summary.html),
  [`borg_certificate()`](https://gillescolling.com/BORG/reference/borg_certificate.md),
  or
  [`borg_export()`](https://gillescolling.com/BORG/reference/borg_export.md)
- Legacy plot functions (`plot_split`, `plot_risk`, etc.)
- Legacy report functions (`borg_methods_text`)

------------------------------------------------------------------------

## BORG 0.2.2

### New features

#### Framework wrappers

BORG-guarded versions of common CV functions that block random CV when
dependencies detected:

- [`borg_vfold_cv()`](https://gillescolling.com/BORG/reference/borg_vfold_cv.md):
  Wraps
  [`rsample::vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html)
  with dependency checking
  - Blocks random CV when spatial/temporal/clustered dependencies
    detected
  - `auto_block = TRUE` automatically switches to appropriate blocked CV
  - `allow_override = TRUE` allows proceeding with warning
- [`borg_group_vfold_cv()`](https://gillescolling.com/BORG/reference/borg_group_vfold_cv.md):
  Wraps
  [`rsample::group_vfold_cv()`](https://rsample.tidymodels.org/reference/group_vfold_cv.html)
  with additional checks
  - Warns about dependencies not handled by grouping alone
- [`borg_initial_split()`](https://gillescolling.com/BORG/reference/borg_initial_split.md):
  Wraps
  [`rsample::initial_split()`](https://rsample.tidymodels.org/reference/initial_split.html)
  - Enforces chronological splits when `time` specified
  - Warns about spatial dependencies
- [`borg_trainControl()`](https://gillescolling.com/BORG/reference/borg_trainControl.md):
  Wraps
  [`caret::trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html)
  - Blocks random resampling methods with detected dependencies

#### Hook system (experimental)

- [`borg_register_hooks()`](https://gillescolling.com/BORG/reference/borg_register_hooks.md):
  Register validation hooks on framework functions
- [`borg_unregister_hooks()`](https://gillescolling.com/BORG/reference/borg_unregister_hooks.md):
  Remove registered hooks

------------------------------------------------------------------------

## BORG 0.2.1

### New features

#### Empirical CV comparison

- [`borg_compare_cv()`](https://gillescolling.com/BORG/reference/borg_compare_cv.md):
  Run random vs blocked CV on the same data to empirically demonstrate
  metric inflation
  - Supports spatial, temporal, and clustered data
  - Custom model and predict functions
  - Multiple metrics: RMSE, MAE, R², AUC, accuracy
  - Paired t-test for statistical significance
  - Plot methods: boxplot, density, paired comparison

#### Publication-ready reporting

- `borg_methods_text()`: Generate copy-paste methods section text for
  manuscripts
  - Includes dependency diagnosis details
  - Cites quantitative metrics (Moran’s I, ICC, etc.)
  - Optional empirical comparison results
  - BORG package citation
- [`borg_certificate()`](https://gillescolling.com/BORG/reference/borg_certificate.md):
  Create structured validation certificates
  - Machine-readable format
  - Includes data characteristics, diagnosis, and inflation estimates
  - Timestamps and version tracking
- [`borg_export()`](https://gillescolling.com/BORG/reference/borg_export.md):
  Export certificates to YAML or JSON format

#### API improvements

- Unified [`borg()`](https://gillescolling.com/BORG/reference/borg.md)
  entry point with two modes:
  - Diagnosis mode: `borg(data, coords=, time=, groups=)` returns
    diagnosis + CV folds

  - Validation mode: `borg(data, train_idx=, test_idx=)` validates
    existing splits
- Renamed parameters for consistency: `temporal_col` → `time`,
  `group_col` → `groups`, `spatial_cols` → `coords`

------------------------------------------------------------------------

## BORG 0.2.0

### Major changes: From Validator to Enforcer

BORG now detects data dependency structure and enforces appropriate
cross-validation strategies. Random CV is blocked when dependencies are
detected.

#### Dependency diagnosis

- [`borg_diagnose()`](https://gillescolling.com/BORG/reference/borg_diagnose.md):
  Automatically detects data dependency structure
  - **Spatial autocorrelation**: Moran’s I test, variogram-based range
    estimation
  - **Temporal autocorrelation**: ACF/Ljung-Box test, decorrelation lag
    detection
  - **Clustered structure**: ICC computation, design effect estimation
  - Returns severity assessment and recommended CV strategy
  - Estimates metric inflation from using random CV
- `BorgDiagnosis` S4 class: Structured diagnosis results with slots for:
  - `dependency_type`: “none”, “spatial”, “temporal”, “clustered”,
    “mixed”
  - `severity`: “none”, “moderate”, “severe”
  - `recommended_cv`: appropriate CV strategy
  - `inflation_estimate`: estimated AUC/RMSE bias from random CV

#### Enforced CV generation

- [`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md):
  Generates valid CV schemes based on diagnosis
  - **Spatial blocking**: k-means clustering with block size \>
    autocorrelation range

  - **Temporal blocking**: chronological splits with embargo periods

  - **Group CV**: leave-group-out with balanced fold assignment

  - **Mixed strategies**: spatial-temporal, spatial-group,
    temporal-group

  - Random CV disabled when dependencies detected (requires explicit
    `allow_random = TRUE`)

  - Output formats: list, rsample, caret, mlr3

### New features

#### Visualization functions

- `plot_split()`: Visualize train/test split distribution with temporal
  or group structure
- `plot_risk()`: Display risk assessment results as horizontal bar chart
- `plot_temporal()`: Timeline visualization with gap analysis and
  look-ahead detection
- `plot_spatial()`: Spatial split visualization with convex hulls
- `plot_groups()`: Group-based split visualization with leakage
  highlighting

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
- [`borg_assimilate()`](https://gillescolling.com/BORG/reference/borg_assimilate.md):
  Assimilates leaky pipelines into compliance (auto-fix)
- `BorgRisk` S4 class for structured risk assessment reports

### Performance

- C++ backends via Rcpp for fast hash-based index overlap and duplicate
  detection

### Framework integration

- caret: `preProcess`, `trainControl`, `train` objects
- tidymodels: `recipe`, `rsplit`, `vfold_cv`, `rset` objects
- mlr3: task and resampling validation
- Base R: manual index-based splits
