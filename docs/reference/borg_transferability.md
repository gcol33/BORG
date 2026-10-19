# Assess Geographic Transferability

Evaluates how well a model transfers across geographic regions by
splitting data into spatial zones, training on each zone, and testing on
all others. Quantifies performance decay with geographic distance.

## Usage

``` r
borg_transferability(
  data,
  formula,
  coords,
  n_regions = 4L,
  metric = c("rmse", "mae", "rsq"),
  fit_fun = stats::lm
)
```

## Arguments

- data:

  Data frame with coordinates and predictors.

- formula:

  Model formula.

- coords:

  Character vector of length 2. Coordinate columns.

- n_regions:

  Integer. Number of geographic regions to create. Default: 4.

- metric:

  Character. Default: `"rmse"`.

- fit_fun:

  Function. Default: `lm`.

## Value

A list with class `"borg_transferability"` containing:

- matrix:

  Performance matrix (n_regions x n_regions): entry (i, j) = metric when
  training on region i, testing on region j

- distance_matrix:

  Geographic distance between region centroids

- decay:

  Data frame: distance, metric_value for plotting decay

- mean_transfer:

  Mean cross-region performance

- mean_within:

  Mean within-region performance

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(200, 0, 100), y = runif(200, 0, 100),
                 a = rnorm(200))
d$z <- d$a * 2 + sin(d$x / 20) + rnorm(200, sd = 0.5)
tf <- borg_transferability(d, z ~ a, coords = c("x", "y"), n_regions = 4)
tf
```
