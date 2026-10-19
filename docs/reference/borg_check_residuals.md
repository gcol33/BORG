# Check Residual Spatial Autocorrelation

After fitting a model, checks whether residuals still exhibit spatial
autocorrelation. If they do, the model has not fully captured the
spatial process and predictions may be biased.

## Usage

``` r
borg_check_residuals(
  model,
  data = NULL,
  coords = NULL,
  alpha = 0.05,
  crs = NULL
)
```

## Arguments

- model:

  A fitted model with a
  [`residuals()`](https://rdrr.io/r/stats/residuals.html) method, or a
  numeric vector of residuals.

- data:

  Data frame with coordinate columns (needed if `model` is a numeric
  vector).

- coords:

  Character vector of length 2. Coordinate column names.

- alpha:

  Numeric. Significance level for Moran's I test. Default: 0.05.

- crs:

  Optional coordinate reference system (an `sf` CRS, EPSG code, or PROJ
  string). When geographic (lon/lat), distances use the Haversine
  formula; otherwise planar Euclidean.

## Value

A list with class `"borg_residual_check"` containing:

- morans_i:

  Moran's I of residuals

- p_value:

  P-value from Moran's I test

- significant:

  Logical. Whether residual autocorrelation is significant

- variogram:

  Residual variogram data frame (if computed)

- assessment:

  Character. "clean", "mild", or "strong"

Has an `autoplot()` method showing the residual variogram.

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(100, 0, 100), y = runif(100, 0, 100))
d$z <- sin(d$x / 10) + rnorm(100, sd = 0.5)
model <- lm(z ~ x + y, data = d)
check <- borg_check_residuals(model, d, coords = c("x", "y"))
check
```
