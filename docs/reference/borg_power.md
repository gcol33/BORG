# Estimate Statistical Power After Blocking

Computes how much statistical power is lost when switching from random
to blocked cross-validation. Reports effective sample size, minimum
detectable effect size, and whether the dataset is large enough.

## Usage

``` r
borg_power(
  data,
  diagnosis = NULL,
  coords = NULL,
  time = NULL,
  groups = NULL,
  target = NULL,
  alpha = 0.05,
  power = 0.8,
  effect_size = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  A data frame.

- diagnosis:

  A
  [`BorgDiagnosis`](https://gillescolling.com/BORG/reference/BorgDiagnosis.md)
  object. If NULL, computed automatically from the other arguments.

- coords:

  Character vector of length 2 for spatial coordinates.

- time:

  Character string for the time column.

- groups:

  Character string for the grouping column.

- target:

  Character string for the response variable.

- alpha:

  Significance level. Default: 0.05.

- power:

  Target power. Default: 0.80.

- effect_size:

  Numeric. Expected effect size (Cohen's d for continuous, OR for
  binary). If NULL, reports minimum detectable effect size instead.

- verbose:

  Logical. Print progress messages. Default: FALSE.

## Value

An object of class `"borg_power"` containing:

- n_actual:

  Total number of observations

- n_effective:

  Effective sample size after accounting for dependencies

- design_effect:

  Variance inflation factor from dependencies

- power_random:

  Statistical power under random CV

- power_blocked:

  Statistical power under blocked CV

- power_loss:

  Absolute power loss (power_random - power_blocked)

- min_detectable_effect:

  Minimum detectable effect at target power

- min_detectable_effect_random:

  Same, under random CV (for comparison)

- sufficient:

  Logical. Is the dataset large enough at target power?

- recommendation:

  Character. Human-readable recommendation.

- diagnosis:

  The BorgDiagnosis used

## Details

When data have spatial, temporal, or clustered dependencies, blocked CV
reduces the effective sample size. This function quantifies that
reduction using the design effect (DEFF):

\$\$n\_{eff} = n / DEFF\$\$

The design effect is computed from:

- **Spatial**: Moran's I and the ratio of autocorrelation range to study
  extent (Griffith, 2005)

- **Temporal**: ACF lag-1 autocorrelation (\\DEFF \approx (1 + \rho) /
  (1 - \rho)\\)

- **Clustered**: ICC and mean cluster size (\\DEFF = 1 + (m - 1) \times
  ICC\\)

For mixed dependencies, design effects are combined multiplicatively.

## See also

[`borg_diagnose`](https://gillescolling.com/BORG/reference/borg_diagnose.md),
[`borg_cv`](https://gillescolling.com/BORG/reference/borg_cv.md)

## Examples

``` r
# Clustered data
clustered_data <- data.frame(
  site = rep(1:20, each = 10),
  value = rep(rnorm(20, sd = 2), each = 10) + rnorm(200, sd = 0.5)
)

pw <- borg_power(clustered_data, groups = "site", target = "value")
print(pw)
```
