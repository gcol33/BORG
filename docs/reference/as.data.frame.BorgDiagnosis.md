# Coerce BorgDiagnosis to Data Frame

Converts a `BorgDiagnosis` object into a one-row data frame of
diagnostic results for programmatic access.

## Usage

``` r
# S3 method for class 'BorgDiagnosis'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A `BorgDiagnosis` object.

- row.names:

  Optional row names for the output data frame.

- optional:

  Logical. Passed to
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html).

- ...:

  Additional arguments passed to
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html).

## Value

A one-row data frame with columns: `dependency_type`, `severity`,
`recommended_cv`, `n_obs`, `spatial_detected`, `morans_i`,
`temporal_detected`, `acf_lag1`, `clustered_detected`, `icc`.

## See also

[`BorgDiagnosis`](https://gillescolling.com/BORG/reference/BorgDiagnosis.md)
