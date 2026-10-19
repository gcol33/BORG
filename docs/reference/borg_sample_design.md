# Suggest Sampling Locations to Improve AOA

Identifies locations in the prediction domain where new training data
would most reduce the dissimilarity index, thereby expanding the area of
applicability.

## Usage

``` r
borg_sample_design(
  train,
  prediction,
  predictors = NULL,
  coords = NULL,
  n = 10L,
  weights = NULL
)
```

## Arguments

- train:

  Data frame of existing training data.

- prediction:

  Data frame of prediction locations (with coordinates).

- predictors:

  Character vector. Predictor columns for DI computation.

- coords:

  Character vector of length 2. Coordinate columns in `prediction`.

- n:

  Integer. Number of suggested sampling locations. Default: 10.

- weights:

  Numeric vector. Variable importance weights for DI.

## Value

A data frame with class `"borg_sample_design"` containing the top-n
prediction locations ranked by DI (highest first), with columns: x, y,
di, rank.

## Examples

``` r
set.seed(42)
train <- data.frame(x = runif(50, 0, 50), y = runif(50, 0, 50),
                     a = rnorm(50))
pred <- data.frame(x = runif(200, 0, 100), y = runif(200, 0, 100),
                    a = rnorm(200))
design <- borg_sample_design(train, pred, predictors = "a",
                               coords = c("x", "y"), n = 5)
design
```
