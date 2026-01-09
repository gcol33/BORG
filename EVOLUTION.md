# BORG Evolution Plan: From Validator to Enforcer

## The Vision Gap

BORG is currently a **validation guard** — it checks splits you give it.

The vision is a **CV enforcer** — it makes invalid evaluation impossible.

> "Random CV is disabled because your data are dependent."

This document outlines the path from where we are to where we need to be.

---

## Phase 1: Autocorrelation Detection Engine

**Goal**: BORG automatically detects data dependency structure.

### 1.1 Spatial Autocorrelation Detection

```r
borg_diagnose(data, coords = c("lon", "lat"))
```

- Compute empirical variogram
- Estimate spatial autocorrelation range
- Return: `spatial_range`, `effective_sample_size`, `inflation_factor`

**Detection triggers**:
- Moran's I test on residuals
- Variogram range estimation
- Nearest-neighbor distance distribution

### 1.2 Temporal Autocorrelation Detection

```r
borg_diagnose(data, time = "date")
```

- ACF/PACF analysis
- Ljung-Box test
- Return: `temporal_lag`, `decay_rate`, `embargo_minimum`

### 1.3 Clustered Structure Detection

```r
borg_diagnose(data, groups = "site_id")
```

- Intraclass correlation (ICC)
- Effective sample size per cluster
- Return: `n_clusters`, `icc`, `design_effect`

### 1.4 Unified Diagnosis

```r
diagnosis <- borg_diagnose(data,
                           coords = c("x", "y"),
                           time = "date",
                           groups = "site")
```
Returns a `BorgDiagnosis` object with:
- `@dependency_type`: "spatial", "temporal", "clustered", "mixed"
- `@severity`: "none", "moderate", "severe"
- `@recommended_cv`: "spatial_block", "temporal_block", "group_fold", etc.
- `@parameters`: list of estimated parameters (range, lag, ICC)

---

## Phase 2: CV Strategy Generator

**Goal**: BORG generates valid CV schemes, not just validates them.

### 2.1 `borg_cv()` — The Enforced Default

```r
cv <- borg_cv(data, diagnosis)
```

Based on diagnosis, generates appropriate CV structure:
- Spatial blocking with block size > autocorrelation range
- Temporal blocking with gap > decorrelation lag
- Group-out CV for clustered data
- Hybrid schemes for mixed dependencies

**Key principle**: No random CV option when dependency detected.

### 2.2 Integration with Existing Frameworks

```r
# rsample
folds <- borg_cv(data, diagnosis, output = "rsample")

# caret
ctrl <- borg_cv(data, diagnosis, output = "caret")

# mlr3
resampling <- borg_cv(data, diagnosis, output = "mlr3")
```

### 2.3 The "Disabled Random" UX

When user tries to use random CV on dependent data:

```r
rsample::vfold_cv(data)
# With borg_auto_check() enabled:
# Error: BORG BLOCKED: Spatial autocorrelation detected (range = 12.3 km).
#        Random CV would inflate metrics by ~40%.
#        Use borg_cv(data) to generate valid folds.
```

---

## Phase 3: Inflation Estimation

**Goal**: Quantify how wrong random CV would be.

### 3.1 Theoretical Inflation Bounds

Based on:
- Effective sample size ratio
- Autocorrelation strength
- Train/test proximity

Output:
```r
diagnosis@inflation_estimate
# $auc_inflation: 0.12  (random CV AUC inflated by ~12%)
# $rmse_deflation: 0.23 (random CV RMSE deflated by ~23%)
# $confidence: "high"
```

### 3.2 Empirical Inflation Check

```r
borg_compare_cv(model, data, diagnosis)
```

Runs both random and blocked CV, reports:
- Metric difference
- Variance ratio
- Leakage severity score

This is the "smoking gun" for reviewers.

---

## Phase 4: Reviewer-Ready Reports

**Goal**: Generate artifacts reviewers can require.

### 4.1 Validation Certificate

```r
cert <- borg_certificate(workflow, data, diagnosis)
export(cert, "validation_certificate.pdf")
```

Contains:
- Dependency diagnosis summary
- CV strategy used and why
- Comparison with random CV (inflation estimate)
- Checksums for reproducibility

### 4.2 Structured Report Format

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

### 4.3 Inline Report for Manuscripts

```r
borg_methods_text(cert)
```

Generates copy-paste methods section:

> "Model evaluation used spatial block cross-validation (n=5 folds, block size=15km)
> following BORG v0.2.0 validation. Spatial autocorrelation was detected with an
> estimated range of 12.3km (Moran's I = 0.43, p < 0.001). Random cross-validation
> was not used as it would inflate AUC estimates by approximately 35%."

---

## Phase 5: Framework Integration (The lme4 Move)

**Goal**: Make BORG the path of least resistance.

### 5.1 Auto-Check Hooks

```r
borg_auto_check(enable = TRUE)
```

When enabled, intercepts:
- `rsample::vfold_cv()` — blocks random CV on diagnosed data
- `caret::trainControl()` — injects BORG validation
- `mlr3::rsmp("cv")` — requires explicit override

### 5.2 Package-Level Integration

Work with maintainers to add BORG as suggested dependency:
- tidymodels ecosystem
- mlr3 ecosystem
- caret (maintenance mode, but still used)

### 5.3 The Override Escape Hatch

For legitimate random CV use cases:

```r
rsample::vfold_cv(data, borg_override = "independent_confirmed")
```

Requires explicit declaration. Gets logged.

---

## Phase 6: Ecosystem Effects

### 6.1 Reviewer Checklist Integration

Provide reviewers with:
- Checklist items for common journals
- Template reviewer comments
- Links to BORG validation requirements

### 6.2 Teaching Materials

- Vignette: "Why Random CV Fails on Ecological Data"
- Vignette: "From Random to Blocked: A Migration Guide"
- Shiny app: Interactive inflation demonstration

### 6.3 Journal Partnerships

Target journals:
- Methods in Ecology and Evolution
- Ecological Modelling
- Journal of Biogeography
- Ecography

Goal: "BORG validation recommended" in author guidelines.

---

## Implementation Priority

| Phase | Priority | Complexity | Impact |
|-------|----------|------------|--------|
| 1.1 Spatial detection | HIGH | Medium | High |
| 1.2 Temporal detection | HIGH | Low | High |
| 1.3 Cluster detection | MEDIUM | Low | Medium |
| 2.1 borg_cv() | HIGH | High | Critical |
| 2.2 Framework output | MEDIUM | Medium | High |
| 3.1 Inflation bounds | HIGH | Medium | Critical |
| 4.1 Certificates | MEDIUM | Low | High |
| 5.1 Auto-check hooks | LOW | High | Critical |

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
