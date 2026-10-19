# Autoplot Method for borg_cv Objects

Visualizes cross-validation fold structure.

## Usage

``` r
# S3 method for class 'borg_cv'
autoplot(
  object,
  type = c("folds", "spatial", "sizes"),
  data = NULL,
  coords = NULL,
  raster = NULL,
  ...
)
```

## Arguments

- object:

  A `borg_cv` object from
  [`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md).

- type:

  Character. Plot type: `"folds"` (default), `"spatial"`, or `"sizes"`.

- data:

  Optional data frame (or sf/SpatVector) for spatial plots. Required for
  `type = "spatial"`.

- coords:

  Character vector of coordinate column names. Required for
  `type = "spatial"` when `data` is a plain data.frame.

- raster:

  Optional
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).
  When provided with `type = "spatial"`, the first layer is rendered as
  a background map behind the fold points. Requires the tidyterra
  package.

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
  d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
  cv <- borg_cv(d, coords = c("x", "y"), target = "z")
  ggplot2::autoplot(cv, type = "sizes")
  ggplot2::autoplot(cv, type = "spatial", data = d, coords = c("x", "y"))
}
# }
```
