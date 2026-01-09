# Diagnose Data Dependency Structure

Automatically detects spatial autocorrelation, temporal autocorrelation,
and clustered structure in data. Returns a diagnosis object that
specifies appropriate cross-validation strategies.

## Usage

``` r
borg_diagnose(
  data,
  coords = NULL,
  time = NULL,
  groups = NULL,
  target = NULL,
  alpha = 0.05,
  verbose = FALSE
)
```

## Arguments

- data:

  A data frame to diagnose.

- coords:

  Character vector of length 2 specifying coordinate column names (e.g.,
  `c("lon", "lat")` or `c("x", "y")`). If NULL, spatial autocorrelation
  is not tested.

- time:

  Character string specifying the time column name. Can be Date,
  POSIXct, or numeric. If NULL, temporal autocorrelation is not tested.

- groups:

  Character string specifying the grouping column name (e.g., "site_id",
  "patient_id"). If NULL, clustered structure is not tested.

- target:

  Character string specifying the response variable column name. Used
  for more accurate autocorrelation diagnostics on residuals. Optional.

- alpha:

  Numeric. Significance level for autocorrelation tests. Default: 0.05.

- verbose:

  Logical. If TRUE, print diagnostic progress. Default: FALSE.

## Value

A
[`BorgDiagnosis`](https://gillescolling.com/BORG/reference/BorgDiagnosis.md)
object containing:

- Detected dependency type(s)

- Severity assessment

- Recommended CV strategy

- Detailed diagnostics for each dependency type

- Estimated metric inflation from using random CV

## Details

### Spatial Autocorrelation

Detected using Moran's I test on the target variable (or first numeric
column). The autocorrelation range is estimated from the empirical
variogram. Effective sample size is computed as \\n\_{eff} = n / DEFF\\
where DEFF is the design effect.

### Temporal Autocorrelation

Detected using the Ljung-Box test on the target variable. The
decorrelation lag is the first lag where ACF drops below the
significance threshold. Minimum embargo period is set to the
decorrelation lag.

### Clustered Structure

Detected by computing the intraclass correlation coefficient (ICC). An
ICC \> 0.05 indicates meaningful clustering. The design effect (DEFF)
quantifies variance inflation: \\DEFF = 1 + (m-1) \times ICC\\ where m
is the average cluster size.

## Examples

``` r
# Spatial data example
set.seed(42)
spatial_data <- data.frame(
  x = runif(100, 0, 100),
  y = runif(100, 0, 100),
  response = rnorm(100)
)
# Add spatial autocorrelation (nearby points are similar)
for (i in 2:100) {
  nearest <- which.min((spatial_data$x[1:(i-1)] - spatial_data$x[i])^2 +
                       (spatial_data$y[1:(i-1)] - spatial_data$y[i])^2)
  spatial_data$response[i] <- 0.7 * spatial_data$response[nearest] +
                              0.3 * rnorm(1)
}

diagnosis <- borg_diagnose(spatial_data, coords = c("x", "y"),
                           target = "response")
print(diagnosis)

# Clustered data example
clustered_data <- data.frame(
  site = rep(1:10, each = 20),
  value = rep(rnorm(10, sd = 2), each = 20) + rnorm(200, sd = 0.5)
)

diagnosis <- borg_diagnose(clustered_data, groups = "site", target = "value")
print(diagnosis)
```
