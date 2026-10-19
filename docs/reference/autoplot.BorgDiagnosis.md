# Autoplot Method for BorgDiagnosis Objects

Creates a summary panel of detected dependency diagnostics.

## Usage

``` r
# S3 method for class 'BorgDiagnosis'
autoplot(object, type = c("summary", "variogram"), ...)
```

## Arguments

- object:

  A
  [`BorgDiagnosis`](https://gillescolling.com/BORG/reference/BorgDiagnosis.md)
  object.

- type:

  Character. Plot type: `"summary"` (default) for dependency gauge
  panel, or `"variogram"` for empirical semivariogram showing spatial
  autocorrelation structure and suggested block size.

- ...:

  Additional arguments (currently unused).

## Value

A `ggplot` object.

## Details

Requires the ggplot2 package.

## Examples

``` r
# \donttest{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  set.seed(42)
  d <- data.frame(
    site = rep(1:20, each = 10),
    value = rep(rnorm(20, sd = 2), each = 10) + rnorm(200, sd = 0.5)
  )
  diag <- borg_diagnose(d, groups = "site", target = "value")
  ggplot2::autoplot(diag)
}
# }
```
