# Check Geographic Representativeness of CV Folds

Evaluates whether each fold's test set covers a representative portion
of the study area. Flags folds that are geographically isolated or
biased toward one region.

## Usage

``` r
borg_check_coverage(folds, data, coords, threshold = 0.2)
```

## Arguments

- folds:

  A `borg_cv` object or list of fold lists.

- data:

  Data frame with coordinate columns.

- coords:

  Character vector of length 2. Coordinate column names.

- threshold:

  Numeric. Minimum proportion of geographic extent that each fold should
  cover (0-1). Default: 0.2 (each fold covers at least 20 percent of the
  x and y extent).

## Value

A data frame with class `"borg_geo_strat"` containing per-fold coverage
metrics:

- fold:

  Fold index

- x_coverage:

  Proportion of x-extent covered by test set

- y_coverage:

  Proportion of y-extent covered by test set

- area_ratio:

  Convex hull area ratio (test / total)

- centroid_x, centroid_y:

  Test set centroid

- balanced:

  Whether fold meets the threshold

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(200, 0, 100), y = runif(200, 0, 100), z = rnorm(200))
cv <- borg_cv(d, coords = c("x", "y"), target = "z")
strat <- borg_check_coverage(cv, d, coords = c("x", "y"))
strat
```
