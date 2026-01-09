# Risk Taxonomy

This document catalogs all evaluation risks that BORG detects, organized
by mechanism.

## Hard Violations

These fundamentally invalidate evaluation. BORG blocks or errors on
detection.

### 1. Index Overlap

**What**: Same row indices appear in both training and test sets.

**Detection**: Set intersection of `train_idx` and `test_idx`.

**Example**:

``` r

train_idx <- 1:60
test_idx <- 50:100  # Rows 50-60 in both!

borg(data, train_idx, test_idx)
# Error: BORG HARD VIOLATION: train_idx and test_idx overlap (11 shared indices)
```

### 2. Duplicate Rows

**What**: Test set contains rows identical to training rows.

**Detection**: Row hashing and comparison (C++ backend for numeric
data).

**Why it matters**: Model may have memorized these exact patterns.

``` r

result <- borg(data, train_idx, test_idx)
# risks[[1]]$type == "duplicate_rows"
```

### 3. Preprocessing Leak

**What**: Normalization, imputation, or PCA fitted on full data before
splitting.

**Detection**: Recompute statistics on train-only data and compare to
stored parameters. If they differ, preprocessing used test data.

**Supported objects**:

- `caret::preProcess` - checks `$mean` and `$std`
- `recipes::recipe` - checks step parameters after `prep()`
- `prcomp` - checks `$center`, `$scale`, and rotation matrix

``` r

# Check a preprocessing object
borg(my_recipe, train_idx, test_idx, data = data)
```

### 4. Target Leakage (Direct)

**What**: Feature has \|correlation\| \> 0.99 with target.

**Detection**: Compute correlation of each numeric feature with target
column on training data.

**Why it matters**: Feature is likely derived from the outcome (e.g.,
“days_since_diagnosis” to predict “has_disease”).

``` r

borg(data, train_idx, test_idx, target_col = "outcome")
# Error: Feature 'days_since_event' has correlation 0.998 with target.
```

### 5. Temporal Ordering

**What**: Test observations predate training observations.

**Detection**: Compare max training timestamp to min test timestamp.

``` r

borg(ts_data, train_idx, test_idx, temporal_col = "date")
# Error: BORG HARD VIOLATION: Temporal ordering violated. 50 test observations
#   predate training data.
```

### 6. Group Overlap

**What**: Same group (patient, site, etc.) appears in both train and
test.

**Detection**: Set intersection of group values.

``` r

borg(patient_data, train_idx, test_idx, group_col = "patient_id")
# Error: BORG HARD VIOLATION: Groups appear in both train and test (3 overlapping)
```

### 7. CV Fold Contamination

**What**: Cross-validation folds contain test indices.

**Detection**: Check if any fold’s training indices intersect with
held-out test set.

**Supported objects**:

- `caret::trainControl` - checks `$index` and `$indexOut`
- `rsample::vfold_cv` and other `rset` objects
- `rsample::rsplit` objects

### 8. Model Scope

**What**: Model was trained on more rows than the training set.

**Detection**: Compare `nrow(trainingData)` or `length(fitted.values)`
to `length(train_idx)`.

**Supported objects**: `lm`, `glm`, `ranger`, `caret::train`, parsnip
models, workflows.

## Soft Warnings

These bias results but model ranking may be preserved. BORG warns but
continues.

### 1. Target Leakage (Proxy)

**What**: Feature has correlation 0.95-0.99 with target.

**Detection**: Same as direct leakage, different threshold.

**Why warning not error**: May be legitimate strong predictor, requires
domain review.

``` r

result <- borg(data, train_idx, test_idx, target_col = "y")
# Warning: Feature 'x2' has correlation 0.96 with target. May be a proxy.
```

### 2. Spatial Proximity

**What**: Test points are very close to training points.

**Detection**: Compute minimum distance from each test point to nearest
training point. Flag if \< 1% of spatial spread.

**Impact**: Spatial autocorrelation may inflate apparent performance.

``` r

borg(geo_data, train_idx, test_idx, spatial_cols = c("lon", "lat"))
```

### 3. Spatial Overlap

**What**: Test region falls inside training region.

**Detection**: Compute convex hull of training points, count how many
test points fall inside.

**Threshold**: Warning if \> 50% of test points inside training hull.

## Risk Type Reference

| Risk Type               | Severity | Detection Method      |
|-------------------------|----------|-----------------------|
| `index_overlap`         | hard     | Index intersection    |
| `duplicate_rows`        | hard     | Row hashing           |
| `preprocessing_leak`    | hard     | Parameter comparison  |
| `target_leakage_direct` | hard     | Correlation \> 0.99   |
| `group_overlap`         | hard     | Group intersection    |
| `cv_leak`               | hard     | Fold index check      |
| `model_scope`           | hard     | Row count comparison  |
| `target_leakage_proxy`  | soft     | Correlation 0.95-0.99 |
| `spatial_proximity`     | soft     | Distance check        |
| `spatial_overlap`       | soft     | Convex hull test      |

## Accessing Risk Details

``` r

result <- borg(data, train_idx, test_idx, target_col = "y")

# Summary
result@is_valid
result@n_hard
result@n_soft

# Individual risks
for (risk in result@risks) {
  cat(risk$type, ":", risk$severity, "\n")
  cat("  ", risk$description, "\n")
  if (!is.null(risk$affected_indices)) {
    cat("  Affected:", length(risk$affected_indices), "indices\n")
  }
}

# Tabular format
df <- as.data.frame(result)
```
