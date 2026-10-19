# Dissimilarity Index

Computes the dissimilarity index of Meyer & Pebesma (2021): the weighted
Euclidean distance in feature space from each point to its nearest
training observation, divided by the mean of all pairwise distances
among training points. Points with high DI are dissimilar to the
training data and predictions may be unreliable.

## Usage

``` r
borg_di(train, new = NULL, train_idx = NULL, predictors = NULL, weights = NULL)
```

## Arguments

- train:

  Data frame of training predictors (or full data with `train_idx`).

- new:

  Data frame of new/prediction locations with the same predictor
  columns. If `NULL`, computes DI for training data (leave-one-out).

- train_idx:

  Integer vector. If provided, `train` is the full dataset and
  `train_idx` selects training rows.

- predictors:

  Character vector. Predictor column names. If `NULL`, uses all shared
  numeric columns.

- weights:

  Numeric vector. Variable importance weights (length = number of
  predictors). If `NULL`, all variables weighted equally. Typically from
  [`borg_importance()`](https://gillescolling.com/BORG/reference/borg_importance.md).

## Value

A numeric vector of DI values (one per row of `new`, or per training row
if `new` is NULL). Has class `"borg_di"` and attribute `"threshold"`
(the outlier-adjusted maximum of the training DI under the boxplot rule
Q75 + 1.5 \* IQR, used as the default AOA cutoff).

## References

Meyer, H., & Pebesma, E. (2021). Predicting into unknown space?
Estimating the area of applicability of spatial prediction models.
*Methods in Ecology and Evolution*, 12(9), 1620-1633.
[doi:10.1111/2041-210X.13650](https://doi.org/10.1111/2041-210X.13650)

## Examples

``` r
set.seed(42)
train <- data.frame(a = rnorm(80), b = rnorm(80))
new <- data.frame(a = rnorm(20, mean = 3), b = rnorm(20))
di <- borg_di(train, new)
summary(di)
```
