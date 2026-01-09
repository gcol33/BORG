# BorgDiagnosis S4 Class

Holds the result of
[`borg_diagnose`](https://gillescolling.com/BORG/reference/borg_diagnose.md):
a structured assessment of data dependency patterns that affect
cross-validation validity.

## Usage

``` r
# S4 method for class 'BorgDiagnosis'
show(object)
```

## Arguments

- object:

  A `BorgDiagnosis` object to be printed.

## Slots

- `dependency_type`:

  Character. Primary dependency type detected: "none", "spatial",
  "temporal", "clustered", or "mixed".

- `severity`:

  Character. Overall severity: "none", "moderate", "severe".

- `recommended_cv`:

  Character. Recommended CV strategy: "random", "spatial_block",
  "temporal_block", "group_fold", "spatial_temporal".

- `spatial`:

  List. Spatial autocorrelation diagnostics with elements: detected
  (logical), morans_i (numeric), morans_p (numeric), range_estimate
  (numeric), effective_n (numeric), coords_used (character).

- `temporal`:

  List. Temporal autocorrelation diagnostics with elements: detected
  (logical), acf_lag1 (numeric), ljung_box_p (numeric),
  decorrelation_lag (integer), embargo_minimum (integer), time_col
  (character).

- `clustered`:

  List. Clustered structure diagnostics with elements: detected
  (logical), icc (numeric), n_clusters (integer), cluster_sizes
  (numeric), design_effect (numeric), group_col (character).

- `inflation_estimate`:

  List. Estimated metric inflation from random CV with elements:
  auc_inflation (numeric, proportion), rmse_deflation (numeric),
  confidence (character: "low"/"medium"/"high"), basis (character).

- `n_obs`:

  Integer. Number of observations in the dataset.

- `timestamp`:

  POSIXct. When the diagnosis was performed.

- `call`:

  Language object. The original call that triggered diagnosis.

## See also

[`borg_diagnose`](https://gillescolling.com/BORG/reference/borg_diagnose.md),
[`borg_cv`](https://gillescolling.com/BORG/reference/borg_cv.md)
