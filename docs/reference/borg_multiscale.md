# Multi-Scale Performance Assessment

Evaluates model accuracy at increasing spatial aggregation scales.
Predictions and observations are averaged within grid cells of
increasing size, and performance is computed at each scale. Reveals
scale-dependent error patterns.

## Usage

``` r
borg_multiscale(
  data,
  predictions,
  target,
  coords,
  scales = NULL,
  metric = c("rmse", "mae", "rsq")
)
```

## Arguments

- data:

  Data frame with coordinates and target variable.

- predictions:

  Numeric vector of predicted values (same length as `nrow(data)`).

- target:

  Character. Target variable name.

- coords:

  Character vector of length 2. Coordinate columns.

- scales:

  Numeric vector. Grid cell sizes to evaluate. If `NULL`, auto-generated
  from 5 percent to 50 percent of extent in 8 steps.

- metric:

  Character. `"rmse"` (default), `"mae"`, or `"rsq"`.

## Value

A data frame with class `"borg_multiscale"` containing:

- scale:

  Grid cell size

- metric_value:

  Performance at this scale

- n_cells:

  Number of occupied grid cells

Has an `autoplot()` method.

## References

Riemann, R., Wilson, B. T., Lister, A., & Parks, S. (2010). An effective
assessment protocol for continuous geospatial datasets of forest
characteristics using USFS Forest Inventory and Analysis (FIA) data.
*Remote Sensing of Environment*, 114(10), 2337-2352.
[doi:10.1016/j.rse.2010.05.010](https://doi.org/10.1016/j.rse.2010.05.010)

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(200, 0, 100), y = runif(200, 0, 100),
                 z = rnorm(200))
preds <- d$z + rnorm(200, sd = 0.5)
ms <- borg_multiscale(d, preds, target = "z", coords = c("x", "y"))
ms
```
