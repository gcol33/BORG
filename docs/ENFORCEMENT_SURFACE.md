# BORG Enforcement Surface Specification

**Version:** 1.0.0 **Status:** Frozen **Last Modified:** 2025-01-07

> This document is the authoritative contract for BORG’s enforcement
> behavior. Changes require a rationale entry in the changelog below.

------------------------------------------------------------------------

## Changelog

| Version | Date | Change | Rationale |
|----|----|----|----|
| 1.0.0 | 2025-01-07 | Initial specification | Defines complete enforcement surface for v0.1.0 release |

------------------------------------------------------------------------

## Governing Principles

1.  **BORG is a gate, not a linter.** Invalid evaluations do not
    proceed.
2.  **Fail closed.** Ambiguous cases block rather than warn.
3.  **No metrics leave the system unless valid.** Performance estimates
    from compromised evaluations are not computed.
4.  **Actions are verbs.** Block, Rewrite, Constrain. Not “Warn” or
    “Inform”.
5.  **Risk is defined at the outcome layer.** Validity of conclusions,
    not correctness of technique.

------------------------------------------------------------------------

## Risk Classification Definitions

**Hard Violation**: The evaluation is fundamentally invalid. Results
cannot be trusted under any interpretation. BORG must refuse to compute
or must block the operation.

**Soft Inflation**: Results are biased but bounded. Performance
estimates are misleading but may retain ordinal validity (model ranking
may be preserved). BORG may allow with enforced constraints or automatic
rewriting.

------------------------------------------------------------------------

## 1. Cross-Validation Schemes

### 1.1 K-Fold Cross-Validation

**Observable Signals in R:** - `caret::trainControl` objects with
`index`/`indexOut` slots - `rsample::vfold_cv` objects storing fold
assignments - Custom fold vectors as integer indices - Preprocessing
objects (`recipes::recipe`, `caret::preProcess`) fitted before splitting

**Detection Method:** - Check if preprocessing parameters (mean, sd, PCA
loadings) were computed on full data - Inspect `recipe$template` row
count vs training fold sizes - Compare `preProcess$mean` against
fold-specific means

**Risk Classification:** Hard Violation (if preprocessing leaks), Soft
Inflation (if only minor statistics leak)

**Enforcement Action:** - **Block**: If `preProcess` or `recipe` fitted
on full data before fold creation - **Rewrite**: Inject preprocessing
into each fold’s training pipeline automatically

------------------------------------------------------------------------

### 1.2 Stratified K-Fold

**Observable Signals in R:** - `caret::createFolds(y, k, list = TRUE)`
with class imbalance - `rsample::vfold_cv(strata = ...)` specification -
Class distribution in each fold vs natural distribution

**Detection Method:** - Compare class proportions per fold to full
dataset - Check if rare classes are artificially balanced across folds -
Detect if stratification variable has temporal/spatial structure

**Risk Classification:** Soft Inflation

**Enforcement Action:** - **Allow with constraints**: Warn if class
proportions are artificially balanced - Flag if stratification variable
correlates with time/space indices

------------------------------------------------------------------------

### 1.3 Leave-One-Out Cross-Validation (LOOCV)

**Observable Signals in R:** - `caret::trainControl(method = "LOOCV")` -
`rsample::loo_cv()` objects - Fold count equals row count

**Detection Method:** - Check `length(folds) == nrow(data)` - Detect
global preprocessing before LOOCV

**Risk Classification:** Hard Violation (preprocessing leak is
amplified), Soft Inflation (variance estimates unreliable)

**Enforcement Action:** - **Block**: If any global preprocessing
detected - **Allow with constraints**: Require acknowledgment that
variance estimates are correlated

------------------------------------------------------------------------

### 1.4 Repeated Cross-Validation

**Observable Signals in R:** -
`caret::trainControl(method = "repeatedcv", repeats = n)` - Multiple
`rsample::vfold_cv` objects with different seeds - Aggregated metrics
across repetitions

**Detection Method:** - Check for consistent bias direction across
repetitions - Detect if preprocessing was fitted once and reused across
all repetitions

**Risk Classification:** Soft Inflation (averaging hides but doesn’t fix
bias)

**Enforcement Action:** - **Allow with constraints**: Verify each
repetition independently enforces anti-leakage - Warn that stability ≠
validity

------------------------------------------------------------------------

### 1.5 Nested Cross-Validation

**Observable Signals in R:** - `caret::trainControl` with
`search = "grid"` or `"random"` inside outer loop -
`mlr3::ResamplingNested` objects - Inner/outer fold structure in tuning
results

**Detection Method:** - Trace data flow: outer test indices must not
appear in inner loop computations - Check early stopping criteria
against outer fold data - Inspect threshold selection data source

**Risk Classification:** Hard Violation (if outer fold influences inner
loop)

**Enforcement Action:** - **Block**: If outer test data indices appear
in inner tuning decisions - **Rewrite**: Enforce strict index separation
between inner and outer loops

------------------------------------------------------------------------

### 1.6 Group K-Fold / Leave-One-Group-Out

**Observable Signals in R:** - `caret::groupKFold(group, k)` with group
vector - `rsample::group_vfold_cv(group = ...)` specification - Group
column in data frame

**Detection Method:** - Check for unique groups in train vs test (no
overlap) - Detect if group variable fully captures dependency
structure - Test for residual within-group correlation in features

**Risk Classification:** Hard Violation (if groups overlap), Soft
Inflation (if grouping insufficient)

**Enforcement Action:** - **Block**: If any group ID appears in both
train and test - **Allow with constraints**: Require grouping variable
to be validated against correlation structure

------------------------------------------------------------------------

## 2. Train-Test Splits

### 2.1 Random Hold-Out Split

**Observable Signals in R:** - `caret::createDataPartition()` output -
`rsample::initial_split()` objects - Row indices in train/test -
`.Random.seed` state at split time

**Detection Method:** - Check if split was created before or after
exploratory analysis - Detect if test indices were accessed before final
evaluation - Track call history for test set access patterns

**Risk Classification:** Soft Inflation (if split timing uncertain)

**Enforcement Action:** - **Allow with constraints**: Require split
creation timestamp before any data exploration - Log all test set
accesses

------------------------------------------------------------------------

### 2.2 Multiple Random Splits with Selection

**Observable Signals in R:** - Multiple split objects with different
seeds - Results object containing metrics from multiple splits -
Reported metric differs from full distribution of split metrics

**Detection Method:** - Compare reported metric to mean/median across
all splits - Detect if only best/worst splits are reported - Check for
selective reporting patterns

**Risk Classification:** Soft Inflation (selective reporting inflates
estimates)

**Enforcement Action:** - **Allow with constraints**: Require reporting
of all splits or pre-registered single split - Block selective metric
reporting

------------------------------------------------------------------------

### 2.3 Data Snooping Through Iterative Refinement

**Observable Signals in R:** -
[`predict()`](https://rdrr.io/r/stats/predict.html) call count on test
set - Model version history with test evaluations interleaved -
Incrementing model modifications after each test evaluation

**Detection Method:** - Count test set evaluation events - Track model
object modifications between test evaluations - Detect if
hyperparameters change after test feedback

**Risk Classification:** Hard Violation (test set becomes validation
set)

**Enforcement Action:** - **Block**: After first test evaluation, refuse
further evaluations on same data - Enforce single-use test set policy

------------------------------------------------------------------------

## 3. Temporal Evaluation Schemes

### 3.1 Walk-Forward / Expanding Window Backtest

**Observable Signals in R:** - `rsample::sliding_window()` /
`rolling_origin()` objects - Time index column in data - Feature columns
with future-derived values

**Detection Method:** - Check feature timestamps against prediction
timestamps - Detect if features contain information from t+k for
predictions at t - Validate that model at time t uses only data ≤ t

**Risk Classification:** Hard Violation (look-ahead bias)

**Enforcement Action:** - **Block**: If any feature timestamp exceeds
prediction timestamp - **Rewrite**: Automatically filter features to
respect temporal ordering

------------------------------------------------------------------------

### 3.2 Rolling Window Backtest

**Observable Signals in R:** - Fixed window size parameter - Window size
selection methodology - Regime change indicators in time series

**Detection Method:** - Check if window size was optimized using future
data - Detect regime changes that window spans inappropriately - Verify
stationarity assumption within windows

**Risk Classification:** Soft Inflation (if window size data-snooped)

**Enforcement Action:** - **Allow with constraints**: Require window
size to be pre-specified or selected on past data only

------------------------------------------------------------------------

### 3.3 Purged and Embargoed Cross-Validation

**Observable Signals in R:** - Purge gap parameter (rows removed near
boundary) - Embargo period parameter - Autocorrelation structure of
target/features

**Detection Method:** - Compare embargo period to measured
autocorrelation decay - Check if purge removes all serially dependent
observations - Validate embargo against feature autocorrelation, not
just target

**Risk Classification:** Soft Inflation (if embargo insufficient)

**Enforcement Action:** - **Allow with constraints**: Require embargo ≥
autocorrelation decay length - Warn if measured autocorrelation exceeds
embargo

------------------------------------------------------------------------

### 3.4 Point-in-Time Feature Construction

**Observable Signals in R:** - Data vintage timestamps (if available) -
Revised vs preliminary data indicators - Feature values that changed
after creation date

**Detection Method:** - Check for data revision columns - Compare
feature values to vintage-specific values - Detect anachronistic feature
values

**Risk Classification:** Hard Violation (look-ahead through revisions)

**Enforcement Action:** - **Block**: If revised data used without
vintage timestamps - Require point-in-time database or vintage column

------------------------------------------------------------------------

## 4. Spatial and Hierarchical Evaluation

### 4.1 Spatial Cross-Validation (Block CV)

**Observable Signals in R:** - `sf` spatial objects with coordinates -
`sp::SpatialPoints` coordinate data - `blockCV` or `spatialsample` fold
objects - Spatial block size parameter

**Detection Method:** - Compute spatial autocorrelation range
(variogram) - Compare block size to autocorrelation range - Detect if
train/test blocks are spatially adjacent

**Risk Classification:** Soft Inflation (if blocks too small)

**Enforcement Action:** - **Allow with constraints**: Require block size
≥ autocorrelation range - Warn if spatial leakage detected between
adjacent blocks

------------------------------------------------------------------------

### 4.2 Leave-One-Location-Out (Spatial Transfer)

**Observable Signals in R:** - Location/site column in data -
Coordinates per location - Environmental covariates per location

**Detection Method:** - Check spatial separation between train and test
locations - Detect if test locations are within convex hull of training
locations - Compare environmental envelopes of train vs test

**Risk Classification:** Soft Inflation (interpolation vs extrapolation
confusion)

**Enforcement Action:** - **Allow with constraints**: Flag if test
locations are interpolative, not extrapolative - Require explicit
statement of transfer claim scope

------------------------------------------------------------------------

### 4.3 Clustered Standard Errors Without Clustered CV

**Observable Signals in R:** - `sandwich::vcovCL()` or `clubSandwich`
usage post-hoc - Cluster variable present but not used in CV splitting -
`lme4::lmer()` random effects without grouped CV

**Detection Method:** - Check if cluster variable exists but CV ignored
it - Detect cluster-aware standard errors applied to non-clustered CV
results

**Risk Classification:** Soft Inflation (point estimates biased, SE
correction insufficient)

**Enforcement Action:** - **Block**: Standard errors from non-clustered
CV with clustered correction - **Rewrite**: Force cluster-aware CV
splitting

------------------------------------------------------------------------

## 5. Simulation-Based Evaluation

### 5.1 In-Silico Validation with Known Ground Truth

**Observable Signals in R:** - Simulation function parameters - Model
architecture designed with knowledge of simulation - Simulation-to-model
alignment

**Detection Method:** - Check if simulation parameters were fixed before
model development - Detect if model architecture exploits known
simulation structure - Compare model assumptions to simulation
assumptions

**Risk Classification:** Soft Inflation (alignment may not generalize)

**Enforcement Action:** - **Allow with constraints**: Require simulation
parameters to be pre-specified - Warn if model assumptions match
simulation assumptions exactly

------------------------------------------------------------------------

### 5.2 Synthetic Data Augmentation Evaluated on Real Test Set

**Observable Signals in R:** - Synthetic data generator (GAN, SMOTE,
etc.) - Generator training data includes test set - Augmented training
data contains synthetic samples

**Detection Method:** - Trace generator training data indices - Check if
generator saw test set before augmentation - Validate generator training
used only train split

**Risk Classification:** Hard Violation (test set information in
augmented training)

**Enforcement Action:** - **Block**: If generator trained on data
including test indices - **Rewrite**: Retrain generator on train-only
data

------------------------------------------------------------------------

### 5.3 Bootstrap Performance Estimates

**Observable Signals in R:** -
[`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html) results -
`.632` or `.632+` estimator specification - Bootstrap sample indices

**Detection Method:** - Check bootstrap estimator assumptions against
data dimensionality - Detect high-dimensional settings where .632
correction fails - Validate that out-of-bag samples are genuinely
independent

**Risk Classification:** Soft Inflation (bias correction may be
incorrect)

**Enforcement Action:** - **Allow with constraints**: Warn in
high-dimensional settings - Require explicit acknowledgment of estimator
assumptions

------------------------------------------------------------------------

## 6. Target and Feature Leakage Patterns

### 6.1 Target Leakage (Direct)

**Observable Signals in R:** - Feature names suggesting outcome
derivation (“diagnosis_date”, “outcome_indicator”) - Perfect or
near-perfect correlation between feature and target - Feature creation
timestamp after target determination

**Detection Method:** - Scan feature names for target-derived patterns -
Check feature-target correlations \> 0.95 - Validate causal ordering if
metadata available

**Risk Classification:** Hard Violation (trivial prediction, no
generalization)

**Enforcement Action:** - **Block**: If feature correlates \> 0.99 with
target - Flag features with suspicious names for manual review

------------------------------------------------------------------------

### 6.2 Target Leakage (Indirect/Proxy)

**Observable Signals in R:** - Features correlated with target only
through confounders - Feature importance concentrated on likely proxy
variables - Domain-inconsistent feature-target relationships

**Detection Method:** - Check if high-importance features have plausible
causal paths - Detect features that are proxies for group membership -
Flag features with implausibly strong predictive power

**Risk Classification:** Soft Inflation (may not generalize under
distribution shift)

**Enforcement Action:** - **Allow with constraints**: Require domain
validation of feature-target relationships - Warn on suspiciously
powerful features

------------------------------------------------------------------------

### 6.3 Train-Test Leakage Through Identifiers

**Observable Signals in R:** - ID columns in data (row names, explicit
ID column) - Duplicate rows between train and test - Near-duplicate
detection (high similarity between train/test rows)

**Detection Method:** - Check for duplicate row hashes between
train/test - Detect shared ID values across splits - Compute
nearest-neighbor distances between train and test

**Risk Classification:** Hard Violation (memorization, not learning)

**Enforcement Action:** - **Block**: If any ID appears in both train and
test - **Block**: If duplicate rows exist across splits - Warn on
near-duplicates (cosine similarity \> 0.99)

------------------------------------------------------------------------

### 6.4 Leakage Through Global Feature Engineering

**Observable Signals in R:** - Target encoding computed on full data -
Frequency statistics from full data - Embeddings (word2vec, PCA) fitted
on full data

**Detection Method:** - Trace feature engineering objects to their
training data - Check if encoding/embedding objects used train+test -
Validate feature statistics against train-only statistics

**Risk Classification:** Hard Violation (test statistics in training
features)

**Enforcement Action:** - **Block**: If feature engineering objects
fitted on full data - **Rewrite**: Recompute features within each fold
using train-only data

------------------------------------------------------------------------

## 7. Metric and Threshold Selection

### 7.1 Threshold Optimization on Test Data

**Observable Signals in R:** - `pROC::coords()` optimal threshold from
test ROC - Threshold selection code accessing test labels - Multiple
thresholds evaluated on test set

**Detection Method:** - Check if threshold selection used test labels -
Trace threshold value origin to train vs test data - Detect threshold
optimization loops over test data

**Risk Classification:** Hard Violation (threshold is a model parameter)

**Enforcement Action:** - **Block**: If threshold selected using test
labels - **Rewrite**: Move threshold selection to validation set

------------------------------------------------------------------------

### 7.2 Metric Selection After Results Are Known

**Observable Signals in R:** - Multiple metrics computed, subset
reported - Metric choice differs from pre-registration (if any) -
Primary metric declaration after evaluation

**Detection Method:** - Check if primary metric was declared before
evaluation - Detect selective metric reporting - Compare reported
metrics to computed metrics

**Risk Classification:** Soft Inflation (favorable metric selection)

**Enforcement Action:** - **Allow with constraints**: Require
pre-specified primary metric - Report all computed metrics if primary
not pre-specified

------------------------------------------------------------------------

### 7.3 Subgroup Analysis Post Hoc

**Observable Signals in R:** - Subgroup definitions created after test
evaluation - Subgroup-specific metrics showing heterogeneous
performance - Subgroups discovered through test set analysis

**Detection Method:** - Check if subgroup definitions predate test
evaluation - Detect subgroups with anomalously good/bad performance -
Trace subgroup discovery to train vs test data

**Risk Classification:** Soft Inflation (cherry-picking favorable
subgroups)

**Enforcement Action:** - **Allow with constraints**: Require subgroup
pre-specification or discovery on train data only - Flag post-hoc
subgroup analyses

------------------------------------------------------------------------

## 8. External Validation and Transfer Evaluation

### 8.1 External Validation on Overlapping Populations

**Observable Signals in R:** - Shared time period, geography, or
institution between datasets - Similar covariate distributions between
train and external - Overlapping collection protocols

**Detection Method:** - Compare covariate distributions (train vs
external) - Check metadata for overlap in time/space/institution -
Compute domain shift metrics (MMD, KL divergence)

**Risk Classification:** Soft Inflation (narrow generalization scope)

**Enforcement Action:** - **Allow with constraints**: Require
documentation of overlap dimensions - Warn if external data too similar
to training data

------------------------------------------------------------------------

### 8.2 Transfer Learning Evaluation with Shared Pretraining Data

**Observable Signals in R:** - Foundation model with unknown pretraining
data - Test data potentially in pretraining corpus - Zero-shot or
few-shot evaluation on common benchmarks

**Detection Method:** - Check for known contamination (benchmark data in
pretraining) - Detect memorization signals (verbatim reproduction) -
Validate test data was verifiably excluded from pretraining

**Risk Classification:** Hard Violation (memorization, not transfer)

**Enforcement Action:** - **Block**: If test data known to be in
pretraining - Require contamination audit or held-out test sets

------------------------------------------------------------------------

### 8.3 Domain Adaptation with Target Domain Labels

**Observable Signals in R:** - Labeled target domain data used for
adaptation - Same target data used for adaptation and evaluation - No
held-out target domain test set

**Detection Method:** - Check if adaptation data overlaps with
evaluation data - Trace label usage in adaptation procedure - Validate
separate adaptation vs evaluation splits

**Risk Classification:** Hard Violation (circular evaluation)

**Enforcement Action:** - **Block**: If adaptation and evaluation use
same labeled data - **Rewrite**: Split target domain into adaptation and
evaluation sets

------------------------------------------------------------------------

## 9. Ensemble and Model Selection Leakage

### 9.1 Stacking/Blending with Validation Set Reuse

**Observable Signals in R:** - Stacking predictions from base models -
Meta-learner trained on validation predictions - Same validation set
used for base model selection and meta-learner training

**Detection Method:** - Trace validation data through base model
selection and meta-learner - Check if base model hyperparameters were
selected using same data as meta-learner - Detect double-dipping on
validation predictions

**Risk Classification:** Hard Violation (validation set used twice)

**Enforcement Action:** - **Block**: If validation set used for both
base selection and meta-training - **Rewrite**: Create four-way split:
base train, base validation, meta train, final test

------------------------------------------------------------------------

### 9.2 Hyperparameter Optimization Across Full Dataset

**Observable Signals in R:** - `caret::train()` with CV including test
data - Grid search results using full dataset - Bayesian optimization
with test data in CV

**Detection Method:** - Check if HPO CV indices overlap with final test
indices - Trace hyperparameter selection to data used - Validate HPO was
nested within train split

**Risk Classification:** Hard Violation (test data influenced
hyperparameters)

**Enforcement Action:** - **Block**: If HPO used test data in any form -
**Rewrite**: Enforce nested CV with strict test set isolation

------------------------------------------------------------------------

### 9.3 Early Stopping on Test Performance

**Observable Signals in R:** - Training history with test metrics at
each epoch - `keras::EarlyStopping` callback on test data - Model
selection based on test metric trajectory

**Detection Method:** - Check early stopping callback data source -
Detect if test metrics influenced training termination - Validate early
stopping used validation (not test) data

**Risk Classification:** Hard Violation (test set used for model
selection)

**Enforcement Action:** - **Block**: If early stopping monitored test
metrics - **Rewrite**: Redirect early stopping to separate validation
set

------------------------------------------------------------------------

## 10. Preprocessing and Pipeline Leakage

### 10.1 Imputation Fitted on Full Data

**Observable Signals in R:** - `mice::mice()` fitted before splitting -
`recipes::step_impute_*()` trained on full data -
`caret::preProcess(method = "medianImpute")` on full data

**Detection Method:** - Check imputation model training data - Compare
imputation parameters to train-only statistics - Trace `$mean`,
`$median` in imputation objects

**Risk Classification:** Hard Violation (test statistics in imputed
training values)

**Enforcement Action:** - **Block**: If imputation fitted on
train+test - **Rewrite**: Refit imputation within each fold on
train-only data

------------------------------------------------------------------------

### 10.2 Normalization/Scaling with Test Statistics

**Observable Signals in R:** -
[`scale()`](https://rdrr.io/r/base/scale.html) applied to full data
before splitting - `caret::preProcess(method = c("center", "scale"))` on
full data - `recipes::step_normalize()` trained on full data

**Detection Method:** - Compare scaling parameters to train-only
mean/sd - Check `preProcess$mean`, `preProcess$std` against fold
statistics - Trace normalization parameters to training data

**Risk Classification:** Hard Violation (test scale information in
training)

**Enforcement Action:** - **Block**: If normalization fitted on
train+test - **Rewrite**: Compute normalization on train, apply frozen
to test

------------------------------------------------------------------------

### 10.3 Feature Selection Using Test Labels

**Observable Signals in R:** - `caret::rfe()` using full data -
Correlation-based selection on full data - `Boruta` or similar run
before splitting

**Detection Method:** - Check if feature selection used test labels -
Compare selected features to train-only selection - Trace selection
criteria computation to data used

**Risk Classification:** Hard Violation (test labels drove feature
choice)

**Enforcement Action:** - **Block**: If feature selection used test
labels - **Rewrite**: Perform feature selection within each fold on
train-only

------------------------------------------------------------------------

### 10.4 Dimensionality Reduction (PCA, UMAP) on Full Data

**Observable Signals in R:** -
[`prcomp()`](https://rdrr.io/r/stats/prcomp.html) or
[`stats::princomp()`](https://rdrr.io/r/stats/princomp.html) on full
data - `umap::umap()` fitted before splitting - `recipes::step_pca()`
trained on full data

**Detection Method:** - Compare PCA loadings to train-only PCA - Check
if UMAP embedding used test data - Trace dimensionality reduction object
training data

**Risk Classification:** Hard Violation (test structure in reduced
space)

**Enforcement Action:** - **Block**: If dimensionality reduction fitted
on train+test - **Rewrite**: Fit reduction on train, transform test with
frozen parameters

------------------------------------------------------------------------

## Summary: Enforcement Action Matrix

| Scheme Category | Primary Detection | Default Action |
|----|----|----|
| Cross-validation preprocessing | `preProcess`/`recipe` timing | Block |
| Group overlap in CV | Index intersection | Block |
| Temporal look-ahead | Feature vs target timestamps | Block |
| Spatial autocorrelation | Block size vs variogram range | Constrain |
| Target leakage (direct) | Correlation \> 0.99 | Block |
| Target leakage (proxy) | Domain implausibility | Warn |
| ID/duplicate leakage | Index intersection, hashing | Block |
| Global feature engineering | Encoding object training data | Block |
| Threshold on test | Label access in threshold code | Block |
| Metric cherry-picking | Pre-specification check | Constrain |
| Stacking double-dip | Data flow tracing | Block |
| HPO on test data | Index tracing through tuning | Block |
| Early stopping on test | Callback data source | Block |
| Preprocessing leak | Parameter origin tracing | Block |

------------------------------------------------------------------------

## Implementation Priority

**Phase 1 - Hard Violations (Must Block):** 1. Train-test index overlap
2. Preprocessing fitted on full data 3. Target leakage (direct,
correlation \> 0.99) 4. Temporal look-ahead in features 5. HPO using
test data

**Phase 2 - Structural Violations (Block or Rewrite):** 1. Group overlap
in grouped CV 2. Global feature engineering 3. Threshold optimization on
test 4. Early stopping on test 5. Stacking with validation reuse

**Phase 3 - Soft Inflation (Constrain and Warn):** 1. Spatial block size
vs autocorrelation 2. Embargo vs temporal autocorrelation 3. External
validation overlap 4. Subgroup analysis timing 5. Bootstrap estimator
assumptions

------------------------------------------------------------------------

## R Object Inspection API (Required)

BORG must be able to inspect:

``` r

# Preprocessing objects
borg_inspect(preProcess_obj)     # Extract training data indices
borg_inspect(recipe_obj)         # Extract template row count, prep data

# CV objects
borg_inspect(trainControl_obj)   # Extract index, indexOut
borg_inspect(rsample_split)      # Extract analysis/assessment indices

# Model objects
borg_inspect(caret_train)        # Extract resampling results, data used
borg_inspect(mlr3_learner)       # Extract task data, resampling

# Feature engineering
borg_inspect(target_encoder)     # Extract encoding training data
borg_inspect(pca_object)         # Extract training data indices

# Temporal
borg_inspect(ts_features)        # Extract timestamps per feature
borg_inspect(ts_target)          # Extract timestamps per target value
```

Each inspector returns a standardized structure enabling BORG to trace
data flow and detect violations.
