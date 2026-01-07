# BORG Feature Implementation Plan

## Phase 1: Core Inspection Enhancements

### 1.1 Model Object Inspection
Inspect fitted models to verify they were trained on correct data.

**Supported models:**
- [ ] `lm`, `glm` (base R)
- [ ] `ranger` (random forest)
- [ ] `xgboost` / `lightgbm`
- [ ] `caret::train` result objects
- [ ] `parsnip` fitted models

**Detection logic:**
- Compare model's training row count vs expected train_idx length
- Check if model stores training data hash (ranger does)
- Verify no test indices in any stored CV results

### 1.2 Workflow/Pipeline Inspection
Holistic inspection of complete ML pipelines.

**Supported pipelines:**
- [ ] tidymodels `workflow` objects
- [ ] caret `train` with preprocessing
- [ ] Custom pipeline lists

**Detection logic:**
- Trace data flow through each step
- Verify preprocessing → model → prediction chain
- Check for test data leaking into any stage

### 1.3 Prediction Audit
Verify predictions were generated correctly.

**Checks:**
- [ ] Predictions made only on test data
- [ ] No train data in prediction set
- [ ] Prediction count matches test set size
- [ ] No data leakage in prediction features

---

## Phase 2: Reporting & Visualization

### 2.1 CV Leakage Report
Detailed breakdown of cross-validation issues.

**Features:**
- [ ] Per-fold leakage summary
- [ ] Affected indices per fold
- [ ] Severity classification
- [ ] Suggested fixes

### 2.2 Visualization Functions
Visual representation of validation results.

**Plots:**
- [ ] `plot_split()` - train/test index distribution
- [ ] `plot_temporal()` - timeline showing train/test with gaps
- [ ] `plot_spatial()` - map of train/test locations
- [ ] `plot_groups()` - group membership visualization
- [ ] `plot_risk()` - summary of all detected risks

---

## Phase 3: Advanced Detection

### 3.1 Feature Importance Audit
Detect if feature importance was computed incorrectly.

**Checks:**
- [ ] Permutation importance on test data
- [ ] SHAP values computed on full data
- [ ] Variable importance from refitted models

### 3.2 Improved Auto-Fix (`borg_rewrite`)
Smarter automatic corrections.

**Improvements:**
- [ ] Fix overlapping indices by reassigning
- [ ] Refit preprocessing on train only
- [ ] Regenerate CV folds correctly
- [ ] Provide before/after comparison

---

## Phase 4: Integrations

### 4.1 Framework Hooks
Automatic validation during model training.

**Integrations:**
- [ ] `options(borg.auto_check = TRUE)` global hook
- [ ] tidymodels tune integration
- [ ] caret trainControl callback

### 4.2 MLOps Integration
Log validation results to tracking systems.

**Platforms:**
- [ ] mlflow logging
- [ ] vetiver model cards
- [ ] Custom webhook support

### 4.3 Shiny Dashboard
Interactive exploration interface.

**Features:**
- [ ] Upload workflow for inspection
- [ ] Interactive risk explorer
- [ ] Export reports

---

## Implementation Priority

| Priority | Feature | Effort | Value |
|----------|---------|--------|-------|
| 1 | Model object inspection | Medium | High |
| 2 | Visualization (plot_split, plot_risk) | Medium | High |
| 3 | Prediction audit | Low | High |
| 4 | Workflow inspection | High | High |
| 5 | CV leakage report | Low | Medium |
| 6 | Feature importance audit | Medium | Medium |
| 7 | Improved borg_rewrite | High | Medium |
| 8 | Framework hooks | Medium | Low |
| 9 | MLOps integration | Medium | Low |
| 10 | Shiny dashboard | High | Low |

---

## Current Progress

- [x] Phase 1.1: Model object inspection (in progress)
- [ ] Phase 1.2: Workflow inspection
- [ ] Phase 1.3: Prediction audit
- [ ] Phase 2.1: CV leakage report
- [ ] Phase 2.2: Visualization
- [ ] Phase 3.1: Feature importance audit
- [ ] Phase 3.2: Improved borg_rewrite
- [ ] Phase 4.1: Framework hooks
- [ ] Phase 4.2: MLOps integration
- [ ] Phase 4.3: Shiny dashboard
