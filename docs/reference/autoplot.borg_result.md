# Autoplot Method for borg_result Objects

Creates ggplot2 visualizations of BORG diagnosis + CV results.

## Usage

``` r
# S3 method for class 'borg_result'
autoplot(
  object,
  type = borg_result_plot_types(),
  fold = 1,
  data = NULL,
  coords = NULL,
  raster = NULL,
  time = NULL,
  groups = NULL,
  ...
)
```

## Arguments

- object:

  A `borg_result` object from
  [`borg()`](https://gillescolling.com/BORG/reference/borg.md).

- type:

  Character. Plot type: `"split"` (default), `"risk"`, `"spatial"`,
  `"temporal"`, or `"groups"`.

- fold:

  Integer. Which fold to plot. Default: 1.

- data:

  Optional data frame (or sf/SpatVector) for spatial plots. Required for
  `type = "spatial"` to obtain coordinates.

- coords:

  Character vector of coordinate column names. Required for
  `type = "spatial"` when `data` is a plain data.frame.

- raster:

  Optional
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).
  When provided with `type = "spatial"`, the first layer is rendered as
  a background map behind the fold points. Requires the tidyterra
  package.

- time:

  Character name of the time column in `data`, or a vector of time
  values (length = `nrow(data)`). Required for `type = "temporal"`.

- groups:

  Character name of the grouping column in `data`, or a vector of group
  labels. Required for `type = "groups"`.

- ...:

  Additional arguments (currently unused).

## Value

A `ggplot` object.

## Details

Requires the ggplot2 package. For spatial plots, the sf package is
recommended for proper map projections.

## Examples

``` r
# \donttest{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  set.seed(42)
  d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
  result <- borg(d, coords = c("x", "y"), target = "z")
  ggplot2::autoplot(result)
  ggplot2::autoplot(result, type = "spatial", data = d, coords = c("x", "y"))
}
# }
```
