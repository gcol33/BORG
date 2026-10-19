# Predict onto a SpatRaster with AOA Masking

Generates spatial predictions from a fitted model onto a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
computes the dissimilarity index and area of applicability, and returns
a multi-layer raster with prediction, DI, and AOA mask layers.

## Usage

``` r
borg_predict_raster(
  model,
  raster,
  train_data,
  predictors = NULL,
  weights = NULL,
  threshold = NULL,
  type = "response"
)
```

## Arguments

- model:

  A fitted model with a
  [`predict()`](https://rdrr.io/r/stats/predict.html) method.

- raster:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with predictor layers matching the model's training variables.

- train_data:

  Data frame of training data used to fit the model.

- predictors:

  Character vector. Predictor column names. If `NULL`, uses raster layer
  names.

- weights:

  Numeric vector. Variable importance weights for DI.

- threshold:

  Numeric. Manual AOA threshold. If `NULL`, computed from training data.

- type:

  Character. Prediction type passed to
  [`predict()`](https://rdrr.io/r/stats/predict.html). Default:
  `"response"`.

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with three layers:

- prediction:

  Model predictions

- di:

  Dissimilarity index

- aoa:

  Area of applicability (1 = inside, 0 = outside)

## Details

Requires the terra package.

## Examples

``` r
# \donttest{
if (requireNamespace("terra", quietly = TRUE)) {
  set.seed(42)
  r <- terra::rast(nrows = 20, ncols = 20, xmin = 0, xmax = 100,
                   ymin = 0, ymax = 100, nlyrs = 2)
  terra::values(r) <- cbind(rnorm(400), rnorm(400))
  names(r) <- c("bio1", "bio2")
  train <- data.frame(bio1 = rnorm(50), bio2 = rnorm(50))
  train$y <- train$bio1 * 2 + rnorm(50, sd = 0.5)
  model <- lm(y ~ bio1 + bio2, data = train)
  result <- borg_predict_raster(model, r, train, predictors = c("bio1", "bio2"))
  names(result)
}
# }
```
