# Coerce BorgRisk to Data Frame

Converts a `BorgRisk` object into a data frame of detected risks.

## Usage

``` r
# S3 method for class 'BorgRisk'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A `BorgRisk` object.

- row.names:

  Optional row names for the output data frame.

- optional:

  Logical. Passed to
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html).

- ...:

  Additional arguments passed to
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html).

## Value

A data frame where each row corresponds to a detected risk. Columns are:
`type`, `severity`, `description`, `source_object`, `n_affected`.

## See also

[`BorgRisk`](https://gillescolling.com/BORG/reference/BorgRisk.md)
