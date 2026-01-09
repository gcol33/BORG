# BORG Evolution Plan: From Validator to Enforcer

## Current Status (v0.2.1)

**Overall: 85/100** — Production-ready with reporting features.

| Dimension | Score | Notes |
|-----------|-------|-------|
| Core Vision (Enforcer) | 85% | Diagnosis + enforced CV generation works; random CV is blocked |
| Validation Depth | 90% | Comprehensive leakage detection across many object types |
| Framework Integration | 75% | Good output formats, but no hook interception yet |
| Reporting | 75% | Certificates, methods text, YAML/JSON export |
| API Cleanliness | 90% | Unified `borg()` entry point is clean and intuitive |
| Test Coverage | 90% | 368+ passing tests |
| Documentation | 80% | Good roxygen docs, 3 vignettes |

### Package Stats (v0.2.1)
- **R code**: ~7,000 lines across 13 files
- **Test code**: ~4,500 lines
- **Exported functions**: 24
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

### 3.2 Empirical Inflation Check ✅ IMPLEMENTED

```r
comparison <- borg_compare_cv(
  data = spatial_data,
  formula = response ~ x + y,
  coords = c("x", "y"),
  repeats = 10
)
print(comparison)
plot(comparison)
```

✅ Implemented features:
- Runs both random and blocked CV on same data
- Reports metric difference with statistical test (paired t-test)
- Supports multiple metrics: RMSE, MAE, R², AUC, accuracy
- Custom model functions supported
- Plot methods: boxplot, density, paired comparison

This is the "smoking gun" for reviewers — empirical proof that random CV inflates metrics.

---

## Phase 4: Reviewer-Ready Reports ✅ IMPLEMENTED

**Goal**: Generate artifacts reviewers can require.

**Status**: Fully implemented with methods text, certificates, and export.

### 4.1 Validation Certificate ✅

```r
cert <- borg_certificate(diagnosis, data, comparison = comparison)
print(cert)
borg_export(cert, "validation_certificate.yaml")
```

✅ Contains:
- BORG version and timestamp
- Data characteristics (n, features, hash)
- Dependency diagnosis summary
- CV strategy and parameters
- Theoretical and empirical inflation estimates

### 4.2 Structured Report Format ✅

Machine-readable YAML/JSON export via `borg_export()`:

```r
borg_export(cert, "validation.yaml")
borg_export(cert, "validation.json")
```

### 4.3 Inline Report for Manuscripts ✅

```r
cat(borg_methods_text(diagnosis, comparison = comparison))
```

Generates copy-paste methods section:

> "Spatial autocorrelation was detected in the data (Moran's I = 0.43, p < 0.001)
> with an estimated autocorrelation range of 12.3 units. Model performance was
> evaluated using spatial block cross-validation (k = 5 folds). Empirical comparison
> showed that random cross-validation significantly inflated RMSE estimates by 23.5%
> (paired t-test, p < 0.001, n = 10 repeats). Cross-validation strategy was determined
> using the BORG package (version 0.2.1) for R."

### 4.4 Visualization ⭐⭐⭐

✅ Visualization functions (555 lines in `borg_plot.R`):
- `plot_split()`: Train/test distribution
- `plot_risk()`: Risk assessment bar chart
- `plot_temporal()`: Timeline with gap analysis
- `plot_spatial()`: Spatial split with convex hulls
- `plot_groups()`: Group-based visualization
- `plot.borg_comparison()`: CV comparison plots (boxplot, density, paired)

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

## Implementation Priority (Updated v0.2.1)

| Phase | Status | Priority | Next Action |
|-------|--------|----------|-------------|
| 1.1 Spatial detection | ✅ DONE | - | Improve variogram fitting |
| 1.2 Temporal detection | ✅ DONE | - | - |
| 1.3 Cluster detection | ✅ DONE | - | - |
| 2.1 borg_cv() | ✅ DONE | - | - |
| 2.2 Framework output | ✅ DONE | - | - |
| 3.1 Inflation bounds | ✅ DONE | - | Add confidence intervals |
| 3.2 borg_compare_cv() | ✅ DONE | - | - |
| 4.1 borg_certificate() | ✅ DONE | - | - |
| 4.2 YAML/JSON export | ✅ DONE | - | - |
| 4.3 borg_methods_text() | ✅ DONE | - | - |
| 5.1 Function interception | ❌ TODO | LOW | Complex, may not be worth it |
| 6.2 Teaching vignettes | ❌ TODO | MEDIUM | "Why Random CV Fails" |

### Recommended Next Steps

1. **Vignette: "Why Random CV Fails"** — Educational content with worked examples
2. **Shiny app** — Interactive inflation demonstration
3. **Journal outreach** — Get BORG mentioned in author guidelines

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
