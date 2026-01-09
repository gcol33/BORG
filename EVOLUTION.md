# BORG Evolution Plan: From Validator to Enforcer

## Current Status (v0.2.0)

**Overall: 75/100** — Production-ready core, Phases 1-2 complete.

| Dimension | Score | Notes |
|-----------|-------|-------|
| Core Vision (Enforcer) | 85% | Diagnosis + enforced CV generation works; random CV is blocked |
| Validation Depth | 90% | Comprehensive leakage detection across many object types |
| Framework Integration | 75% | Good output formats, but no hook interception yet |
| Reporting | 30% | Plots exist, but no certificates/machine-readable output |
| API Cleanliness | 90% | Unified `borg()` entry point is clean and intuitive |
| Test Coverage | 85% | 317 passing tests, good coverage of core paths |
| Documentation | 80% | Good roxygen docs, 3 vignettes |

### Package Stats (v0.2.0)
- **R code**: 6,144 lines across 11 files
- **Test code**: 3,851 lines
- **Exported functions**: 19
- **R CMD check**: 0 errors, 0 warnings, 0 notes

---

## The Vision

BORG is a **CV enforcer** — it makes invalid evaluation impossible.

> "Random CV is disabled because your data are dependent."

This document outlines the path from where we are to where we need to be.

---

## Phase 1: Autocorrelation Detection Engine ✅ COMPLETE

**Goal**: BORG automatically detects data dependency structure.

**Status**: Fully implemented in `borg_diagnose()` (903 lines).

### 1.1 Spatial Autocorrelation Detection ⭐⭐⭐⭐

```r
borg_diagnose(data, coords = c("lon", "lat"))
```

✅ Implemented:
- Moran's I test on residuals
- Basic variogram-based range estimation
- Nearest-neighbor distance distribution

⚠️ Could improve:
- More sophisticated variogram fitting (exponential, spherical models)
- Anisotropic autocorrelation detection

### 1.2 Temporal Autocorrelation Detection ⭐⭐⭐⭐

```r
borg_diagnose(data, time = "date")
```

✅ Implemented:
- ACF/PACF analysis
- Ljung-Box test
- Decorrelation lag detection
- Returns: `temporal_lag`, `acf_lag1`, `ljung_box_p`

### 1.3 Clustered Structure Detection ⭐⭐⭐⭐

```r
borg_diagnose(data, groups = "site_id")
```

✅ Implemented:
- Intraclass correlation (ICC)
- Design effect estimation
- Returns: `n_clusters`, `icc`, `design_effect`, `avg_cluster_size`

### 1.4 Unified Diagnosis ⭐⭐⭐

```r
diagnosis <- borg_diagnose(data,
                           coords = c("x", "y"),
                           time = "date",
                           groups = "site")
```

✅ Returns a `BorgDiagnosis` S4 object with:
- `@dependency_type`: "spatial", "temporal", "clustered", "mixed", "none"
- `@severity`: "none", "moderate", "severe"
- `@recommended_cv`: "spatial_block", "temporal_block", "group_fold", etc.
- `@spatial`, `@temporal`, `@clustered`: detailed diagnostics
- `@inflation_estimate`: theoretical bias from random CV

⚠️ Could improve:
- Smarter mixed dependency resolution (currently picks dominant)
- Interaction effects between dependency types

---

## Phase 2: CV Strategy Generator ✅ COMPLETE

**Goal**: BORG generates valid CV schemes, not just validates them.

**Status**: Fully implemented in `borg_cv()` (634 lines).

### 2.1 `borg_cv()` — The Enforced Default ⭐⭐⭐⭐⭐

```r
cv <- borg_cv(data, diagnosis)
```

✅ Implemented:
- Spatial blocking (k-means clustering, block size > autocorrelation range)
- Temporal blocking (chronological splits with embargo periods)
- Group-out CV (leave-group-out with balanced fold assignment)
- **Random CV blocked** when dependencies detected (requires `allow_random = TRUE`)

### 2.2 Integration with Existing Frameworks ⭐⭐⭐⭐

```r
# rsample
folds <- borg_cv(data, diagnosis, output = "rsample")  # ✅

# caret
ctrl <- borg_cv(data, diagnosis, output = "caret")     # ✅

# mlr3
resampling <- borg_cv(data, diagnosis, output = "mlr3") # ✅
```

✅ All three output formats implemented and tested.

### 2.3 The "Disabled Random" UX ⭐⭐⭐⭐⭐

✅ Implemented — random CV throws error when dependencies detected:

```r
borg_cv(data, diagnosis)
# Error: BORG: Random CV blocked. Spatial dependency detected (severity: severe).
#        Recommended strategy: spatial_block
#        To override: borg_cv(..., allow_random = TRUE)
```

### 2.4 Unified Entry Point ⭐⭐⭐⭐⭐

```r
# Diagnosis mode: auto-detect and generate valid CV
result <- borg(data, coords = c("x", "y"), target = "response")
result$diagnosis  # BorgDiagnosis
result$folds      # Valid CV folds

# Validation mode: check existing split
borg(data, train_idx = 1:70, test_idx = 71:100)
```

---

## Phase 3: Inflation Estimation ⚠️ PARTIAL

**Goal**: Quantify how wrong random CV would be.

### 3.1 Theoretical Inflation Bounds ⭐⭐⭐

✅ Implemented in `borg_diagnose()`:

```r
diagnosis@inflation_estimate
# $auc_inflation: 0.12  (random CV AUC inflated by ~12%)
# $rmse_deflation: 0.23 (random CV RMSE deflated by ~23%)
# $confidence: "high"
```

Based on:
- Effective sample size ratio
- Autocorrelation strength
- Design effect for clustered data

⚠️ Could improve:
- More sophisticated theoretical models
- Confidence intervals on estimates

### 3.2 Empirical Inflation Check ❌ NOT IMPLEMENTED

```r
borg_compare_cv(model, data, diagnosis)
```

**This is the highest-impact missing feature.**

Would run both random and blocked CV, reporting:
- Metric difference (AUC, RMSE, etc.)
- Variance ratio
- Leakage severity score

This is the "smoking gun" for reviewers — empirical proof that random CV inflates metrics.

**Priority**: HIGH — critical for adoption.

---

## Phase 4: Reviewer-Ready Reports ❌ NOT STARTED

**Goal**: Generate artifacts reviewers can require.

**Status**: Not implemented. Visualization exists (`plot_*` functions) but no formal reports.

### 4.1 Validation Certificate ❌

```r
cert <- borg_certificate(workflow, data, diagnosis)
export(cert, "validation_certificate.pdf")
```

Would contain:
- Dependency diagnosis summary
- CV strategy used and why
- Comparison with random CV (inflation estimate)
- Checksums for reproducibility

### 4.2 Structured Report Format ❌

Machine-readable YAML/JSON for automated review:

```yaml
borg_validation:
  version: "0.2.0"
  diagnosis:
    spatial_autocorrelation: true
    range_km: 12.3
    temporal_autocorrelation: false
  cv_strategy:
    type: "spatial_block"
    block_size_km: 15.0
    n_folds: 5
  validation:
    passed: true
    inflation_avoided: "~35% AUC"
  timestamp: "2025-01-09T14:32:00Z"
  data_hash: "sha256:abc123..."
```

### 4.3 Inline Report for Manuscripts ❌

```r
borg_methods_text(cert)
```

Would generate copy-paste methods section:

> "Model evaluation used spatial block cross-validation (n=5 folds, block size=15km)
> following BORG v0.2.0 validation. Spatial autocorrelation was detected with an
> estimated range of 12.3km (Moran's I = 0.43, p < 0.001). Random cross-validation
> was not used as it would inflate AUC estimates by approximately 35%."

### 4.4 What IS Implemented ⭐⭐⭐

✅ Visualization functions (555 lines in `borg_plot.R`):
- `plot_split()`: Train/test distribution
- `plot_risk()`: Risk assessment bar chart
- `plot_temporal()`: Timeline with gap analysis
- `plot_spatial()`: Spatial split with convex hulls
- `plot_groups()`: Group-based visualization

---

## Phase 5: Framework Integration (The lme4 Move) ⚠️ PARTIAL

**Goal**: Make BORG the path of least resistance.

### 5.1 Auto-Check Hooks ⭐⭐⭐

✅ Implemented:
```r
borg_auto_check(enable = TRUE)
borg_options()  # Query current config
```

❌ NOT implemented — function interception:
- `rsample::vfold_cv()` — blocks random CV on diagnosed data
- `caret::trainControl()` — injects BORG validation
- `mlr3::rsmp("cv")` — requires explicit override

The `borg_auto_check()` function exists but only sets options; it doesn't actually intercept framework functions.

### 5.2 Package-Level Integration ❌

Work with maintainers to add BORG as suggested dependency:
- tidymodels ecosystem
- mlr3 ecosystem
- caret (maintenance mode, but still used)

### 5.3 The Override Escape Hatch ✅

Implemented via `allow_random = TRUE`:

```r
borg_cv(data, diagnosis, allow_random = TRUE)  # Explicit override
```

### 5.4 Object Inspection ⭐⭐⭐⭐

✅ Comprehensive inspection support (1,558 lines in `borg_inspect.R`):

| Object Type | Status | Rating |
|-------------|--------|--------|
| `prcomp` PCA | ✅ | ⭐⭐⭐⭐ |
| caret `preProcess` | ✅ | ⭐⭐⭐⭐ |
| caret `trainControl` | ✅ | ⭐⭐⭐⭐ |
| tidymodels `recipe` | ✅ | ⭐⭐⭐⭐ |
| rsample objects | ✅ | ⭐⭐⭐⭐ |
| `lm`/`glm` models | ✅ | ⭐⭐⭐⭐ |
| `ranger` models | ✅ | ⭐⭐⭐ |
| `xgboost` models | ✅ | ⭐⭐⭐ |
| `lightgbm` models | ✅ | ⭐⭐⭐ |
| `parsnip`/`workflow` | ✅ | ⭐⭐⭐ |
| mlr3 tasks | ✅ | ⭐⭐⭐ |

---

## Phase 6: Ecosystem Effects ❌ NOT STARTED

### 6.1 Reviewer Checklist Integration ❌

Provide reviewers with:
- Checklist items for common journals
- Template reviewer comments
- Links to BORG validation requirements

### 6.2 Teaching Materials ⚠️ PARTIAL

✅ Existing vignettes:
- `quickstart.Rmd`: Basic usage guide
- `frameworks.Rmd`: Framework integration examples
- `risk-taxonomy.Rmd`: Risk type documentation

❌ Missing:
- "Why Random CV Fails on Ecological Data"
- "From Random to Blocked: A Migration Guide"
- Shiny app: Interactive inflation demonstration

### 6.3 Journal Partnerships ❌

Target journals:
- Methods in Ecology and Evolution
- Ecological Modelling
- Journal of Biogeography
- Ecography

Goal: "BORG validation recommended" in author guidelines.

---

## Implementation Priority (Updated)

| Phase | Status | Priority | Next Action |
|-------|--------|----------|-------------|
| 1.1 Spatial detection | ✅ DONE | - | Improve variogram fitting |
| 1.2 Temporal detection | ✅ DONE | - | - |
| 1.3 Cluster detection | ✅ DONE | - | - |
| 2.1 borg_cv() | ✅ DONE | - | - |
| 2.2 Framework output | ✅ DONE | - | - |
| 3.1 Inflation bounds | ✅ DONE | - | Add confidence intervals |
| **3.2 borg_compare_cv()** | ❌ TODO | **HIGH** | **Empirical smoking gun** |
| **4.1 borg_certificate()** | ❌ TODO | **HIGH** | **Reviewer artifact** |
| 4.2 YAML/JSON export | ❌ TODO | MEDIUM | Machine-readable output |
| **4.3 borg_methods_text()** | ❌ TODO | **HIGH** | **Copy-paste for papers** |
| 5.1 Function interception | ❌ TODO | LOW | Complex, may not be worth it |
| 6.2 Teaching vignettes | ❌ TODO | MEDIUM | "Why Random CV Fails" |

### Recommended Next Steps

1. **`borg_compare_cv()`** — Run random vs blocked CV, show metric inflation empirically
2. **`borg_methods_text()`** — Generate methods section text for manuscripts
3. **`borg_certificate()`** — PDF/HTML validation certificate

These three features would dramatically increase adoption by making BORG useful for publication workflows.

---

## Success Metrics

1. **Adoption**: Downloads, GitHub stars, citations
2. **Enforcement**: % of users who switch from random to blocked CV
3. **Reviewer uptake**: Mentions in peer review
4. **Journal adoption**: Guidelines that reference BORG
5. **Inflation caught**: Documented cases of prevented metric inflation

---

## The End State

When BORG succeeds:

1. `vfold_cv()` on spatial data feels as wrong as `lm()` on repeated measures
2. Reviewers write "Please provide BORG validation certificate"
3. Random CV on dependent data becomes a rejection signal
4. Blocked CV is the default, not the alternative

> "People don't choose better practice. They accept defaults."

BORG becomes that default.
