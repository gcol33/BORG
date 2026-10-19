# Detect Model Extrapolation

Checks whether prediction locations fall outside the environmental
envelope of training data. Uses per-variable range checks and
multivariate Mahalanobis distance.

## Usage

``` r
borg_extrapolation(train, new, predictors = NULL, coords = NULL)
```

## Arguments

- train:

  Data frame of training data.

- new:

  Data frame of prediction data (same predictor columns).

- predictors:

  Character vector. Predictor column names. If `NULL`, uses all shared
  numeric columns.

- coords:

  Character vector of length 2. Coordinate columns in `new` for spatial
  mapping. Optional.

## Value

A data frame with class `"borg_extrapolation"` containing:

- mahal_dist:

  Mahalanobis distance to training centroid

- n_vars_outside:

  Number of variables outside training range

- vars_outside:

  Comma-separated names of out-of-range variables

- extrapolating:

  Logical. TRUE if any variable is out of range

- x, y:

  Coordinates (if provided)

## Examples

``` r
set.seed(42)
train <- data.frame(a = rnorm(80), b = rnorm(80))
new <- data.frame(a = c(rnorm(10), rnorm(10, mean = 5)),
                   b = c(rnorm(10), rnorm(10, mean = 5)))
ext <- borg_extrapolation(train, new)
table(ext$extrapolating)
```
