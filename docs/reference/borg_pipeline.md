# Validate an Entire Modeling Pipeline

Walks a tidymodels `workflow()` or
[`caret::train()`](https://rdrr.io/pkg/caret/man/train.html) object and
validates every step — preprocessing, feature selection, tuning, and
model fitting — for information leakage.

## Usage

``` r
borg_pipeline(pipeline, train_idx, test_idx, data = NULL, ...)
```

## Arguments

- pipeline:

  A modeling pipeline object. Supported types:

  - A tidymodels `workflow` object (fitted or unfitted)

  - A [`caret::train`](https://rdrr.io/pkg/caret/man/train.html) object

  - A list with named components (recipe, model, tune_results, etc.)

- train_idx:

  Integer vector of training row indices.

- test_idx:

  Integer vector of test row indices.

- data:

  Optional data frame. Required for parameter-level checks.

- ...:

  Additional arguments passed to inspectors.

## Value

An object of class `"borg_pipeline"` containing:

- stages:

  Named list of per-stage BorgRisk results

- overall:

  Aggregated BorgRisk for the full pipeline

- n_stages:

  Number of stages inspected

- leaking_stages:

  Character vector of stage names with hard violations

## Details

`borg_pipeline()` decomposes a pipeline into stages and inspects each:

1.  **Preprocessing**: Recipe steps, preProcess, PCA, scaling

2.  **Feature selection**: Variable importance, filtering

3.  **Hyperparameter tuning**: Inner CV resamples

4.  **Model fitting**: Training data scope, row counts

5.  **Post-processing**: Threshold optimization, calibration

Each stage gets its own BorgRisk assessment. The overall result
aggregates all risks across stages.

## See also

[`borg_validate`](https://gillescolling.com/BORG/reference/borg_validate.md),
[`borg_inspect`](https://gillescolling.com/BORG/reference/borg_inspect.md)

## Examples

``` r
# \donttest{
if (requireNamespace("caret", quietly = TRUE)) {
  ctrl <- caret::trainControl(method = "cv", number = 5)
  model <- caret::train(mpg ~ ., data = mtcars[1:25, ], method = "lm",
                        trControl = ctrl, preProcess = c("center", "scale"))
  result <- borg_pipeline(model, train_idx = 1:25, test_idx = 26:32,
                          data = mtcars)
  print(result)
}
# }
```
