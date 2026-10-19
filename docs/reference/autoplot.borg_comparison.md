# Autoplot Method for borg_comparison Objects

Visualizes random vs blocked CV comparison results.

## Usage

``` r
# S3 method for class 'borg_comparison'
autoplot(object, type = c("boxplot", "density", "paired"), ...)
```

## Arguments

- object:

  A `borg_comparison` object from
  [`borg_compare_cv()`](https://gillescolling.com/BORG/reference/borg_compare_cv.md).

- type:

  Character. Plot type: `"boxplot"` (default), `"density"`, or
  `"paired"`.

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
    x = rnorm(200),
    y = rep(rnorm(20, sd = 2), each = 10) + rnorm(200, sd = 0.5)
  )
  comp <- borg_compare_cv(d, formula = y ~ x, groups = "site", repeats = 3)
  ggplot2::autoplot(comp)
}
# }
```
