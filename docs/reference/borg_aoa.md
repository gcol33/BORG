# Area of Applicability

Determines where a spatial prediction model can be trusted, following
Meyer & Pebesma (2021). Combines the dissimilarity index (DI) with a
threshold derived from cross-validated DI values to produce a binary
applicability mask.

## Usage

``` r
borg_aoa(
  train,
  new,
  predictors = NULL,
  coords = NULL,
  weights = NULL,
  folds = NULL,
  threshold = NULL,
  normalize = FALSE
)
```

## Arguments

- train:

  Data frame of training data.

- new:

  Data frame of prediction locations (same predictor columns).

- predictors:

  Character vector. Predictor column names.

- coords:

  Character vector of length 2. Coordinate columns in `new` for spatial
  mapping. Optional.

- weights:

  Numeric vector. Variable importance weights.

- folds:

  Optional `borg_cv` object or fold list. If provided, the threshold is
  derived from cross-validated DI (more robust).

- threshold:

  Numeric. Manual DI threshold override. If `NULL`, computed as the
  outlier-adjusted maximum (Q75 + 1.5 \* IQR rule) of the LOO training
  DI, or of the cross-validated DI when `folds` is given.

- normalize:

  Logical. If `TRUE`, add a `di_norm` column holding the dissimilarity
  index rescaled by the AOA threshold, so that `di_norm = 1` marks the
  applicability boundary and values above 1 lie outside the AOA. Makes
  DI comparable across models and thresholds. Default: `FALSE`.

## Value

A data frame with class `"borg_aoa"` containing:

- di:

  Dissimilarity index for each prediction point

- aoa:

  Logical. TRUE if inside AOA (DI \<= threshold)

- di_norm:

  DI divided by the threshold (only when `normalize = TRUE`); the AOA
  boundary sits at 1

- x, y:

  Coordinates (if `coords` provided)

Has `autoplot()` method showing the AOA map.

## References

Meyer, H., & Pebesma, E. (2021). Predicting into unknown space?
Estimating the area of applicability of spatial prediction models.
*Methods in Ecology and Evolution*, 12(9), 1620-1633.
[doi:10.1111/2041-210X.13650](https://doi.org/10.1111/2041-210X.13650)

## Examples

``` r
set.seed(42)
train <- data.frame(x = runif(80, 0, 50), y = runif(80, 0, 50),
                     a = rnorm(80), b = rnorm(80))
pred <- data.frame(x = runif(200, 0, 100), y = runif(200, 0, 100),
                    a = rnorm(200), b = rnorm(200))
aoa <- borg_aoa(train, pred, predictors = c("a", "b"), coords = c("x", "y"))
table(aoa$aoa)
```
