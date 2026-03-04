# Create Validation Certificate

Generate a structured validation certificate documenting the BORG
analysis for reproducibility and audit trails.

## Usage

``` r
borg_certificate(diagnosis, data, comparison = NULL, cv = NULL)
```

## Arguments

- diagnosis:

  A `BorgDiagnosis` object.

- data:

  The data frame that was analyzed.

- comparison:

  Optional. A `borg_comparison` object with empirical inflation
  estimates.

- cv:

  Optional. A `borg_cv` object with the CV folds used.

## Value

A `borg_certificate` object containing:

- `meta`: Package version, R version, timestamp

- `data`: Data characteristics and hash

- `diagnosis`: Dependency type, severity, recommended CV

- `cv_strategy`: CV type and fold count

- `inflation`: Theoretical and empirical estimates

## See also

[`borg_export`](https://gillescolling.com/BORG/reference/borg_export.md)
for writing certificates to file.

## Examples

``` r
set.seed(42)
data <- data.frame(
  x = runif(100, 0, 100),
  y = runif(100, 0, 100),
  response = rnorm(100)
)
diagnosis <- borg_diagnose(data, coords = c("x", "y"), target = "response",
                           verbose = FALSE)
cert <- borg_certificate(diagnosis, data)
print(cert)
```
