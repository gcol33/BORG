# Temporal Block Cross-Validation (rsample-compatible)

Creates temporal block CV folds that plug directly into
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
and
[`tune::fit_resamples()`](https://tune.tidymodels.org/reference/fit_resamples.html).

## Usage

``` r
borg_temporal_cv(data, time, target = NULL, v = 5, embargo = NULL, ...)
```

## Arguments

- data:

  A data frame.

- time:

  Character. Time column name.

- target:

  Character. Target variable.

- v:

  Integer. Number of folds. Default: 5.

- embargo:

  Numeric. Time gap between train and test sets.

- ...:

  Additional arguments passed to
  [`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md).

## Value

A `borg_rset` object (subclass of `rset`) compatible with tidymodels.

## Examples

``` r
# \donttest{
if (requireNamespace("rsample", quietly = TRUE)) {
  d <- data.frame(time = 1:100, x = rnorm(100), y = cumsum(rnorm(100)))
  folds <- borg_temporal_cv(d, time = "time", target = "y")
}
# }
```
