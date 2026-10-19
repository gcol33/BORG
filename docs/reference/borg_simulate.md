# Generate Synthetic Data with Known Leakage

Creates datasets with controlled spatial autocorrelation, temporal
dependence, or target leakage. The true inflation magnitude is known,
enabling benchmarking of leakage detection methods and quantifying how
much performance metrics inflate under different CV strategies.

## Usage

``` r
borg_simulate(
  n = 500L,
  type = c("spatial", "temporal", "target_leak", "preprocessing_leak", "combined",
    "independent"),
  n_predictors = 5L,
  signal_strength = 0.3,
  autocorrelation = 0.5,
  leak_strength = 0.9,
  grid_size = NULL,
  seed = 42L
)
```

## Arguments

- n:

  Integer. Number of observations. Default: 500.

- type:

  Character. Type of dependency to simulate. One of `"spatial"`,
  `"temporal"`, `"target_leak"`, `"preprocessing_leak"`, `"combined"`,
  or `"independent"` (control). Default: `"spatial"`.

- n_predictors:

  Integer. Number of predictor variables. Default: 5.

- signal_strength:

  Numeric in `[0, 1]`. Fraction of variance explained by predictors.
  Default: 0.3.

- autocorrelation:

  Numeric in `[0, 1]`. Strength of spatial or temporal autocorrelation.
  0 = none, 1 = very strong. Default: 0.5.

- leak_strength:

  Numeric in `[0, 1]`. For target leakage scenarios, how strongly the
  leaked feature correlates with the response. Default: 0.9.

- grid_size:

  Integer. For spatial data, side length of the spatial grid (total
  points = `n`, placed on a grid of approximately this resolution).
  Default: NULL (auto).

- seed:

  Integer. Random seed for reproducibility. Default: 42.

## Value

A list with class `"borg_simulation"` containing:

- data:

  The generated data frame with predictors, response, and (if
  spatial/temporal) coordinate/time columns.

- true_r2:

  The true R-squared if the model could perfectly recover the signal
  (upper bound for honest evaluation).

- type:

  The dependency type used.

- params:

  List of all generation parameters.

- leaked_vars:

  Character vector of intentionally leaked variable names (for
  target_leak/preprocessing_leak types).

- coords:

  Character vector of coordinate column names (if spatial).

- time_col:

  Name of time column (if temporal).

## Details

### Simulation types

- `"spatial"`:

  Predictors and response have Gaussian spatial autocorrelation on a 2D
  grid. Random CV inflates R2 relative to spatial block CV.

- `"temporal"`:

  Predictors and response are AR(1) time series. Random CV inflates
  metrics relative to temporal CV.

- `"target_leak"`:

  One predictor is a noisy copy of the response (post-hoc information).

- `"preprocessing_leak"`:

  Predictors are normalized on the full dataset, not within each fold.
  The data itself is independent but leakage occurs through shared
  statistics.

- `"combined"`:

  Spatial autocorrelation plus one target-leaked predictor.

- `"independent"`:

  No dependencies. Control scenario where random and blocked CV should
  yield similar results.

## See also

[`borg_cv`](https://gillescolling.com/BORG/reference/borg_cv.md),
[`borg_compare_cv`](https://gillescolling.com/BORG/reference/borg_compare_cv.md)

## Examples

``` r
# Spatial leakage benchmark
sim <- borg_simulate(n = 300, type = "spatial", autocorrelation = 0.7)
str(sim$data)
sim$true_r2

# Compare random vs spatial CV
# \donttest{
cv_spatial <- borg_cv(sim$data, coords = sim$coords, target = "y")
# }
```
