# Compare Random vs Blocked Cross-Validation

Runs both random and blocked cross-validation on the same data and
model, providing empirical evidence of metric inflation from ignoring
data dependencies.

## Usage

``` r
borg_compare_cv(
  data,
  formula,
  model_fn = NULL,
  predict_fn = NULL,
  metric = NULL,
  diagnosis = NULL,
  coords = NULL,
  time = NULL,
  groups = NULL,
  target = NULL,
  v = 5,
  repeats = 10,
  seed = NULL,
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame containing predictors and response.

- formula:

  A formula specifying the model (e.g., `y ~ .`).

- model_fn:

  A function that fits a model. Should accept `formula` and `data`
  arguments and return a fitted model with a `predict` method. Default
  uses `lm`.

- predict_fn:

  A function to generate predictions. Should accept `model` and
  `newdata` arguments. Default uses `predict`.

- metric:

  A character string specifying the metric to compute. One of `"rmse"`,
  `"mae"`, `"rsq"`, `"auc"`, `"accuracy"`. Default: `"rmse"` for
  regression, `"accuracy"` for classification.

- diagnosis:

  A `BorgDiagnosis` object. If NULL, will be computed automatically
  using the provided structure hints.

- coords:

  Character vector of length 2 specifying coordinate column names.

- time:

  Character string specifying the time column name.

- groups:

  Character string specifying the grouping column name.

- target:

  Character string specifying the response variable name. If NULL,
  extracted from formula.

- v:

  Integer. Number of CV folds. Default: 5.

- repeats:

  Integer. Number of times to repeat CV. Default: 10 for stable
  estimates.

- seed:

  Integer. Random seed for reproducibility.

- verbose:

  Logical. Print progress messages. Default: TRUE.

## Value

A `borg_comparison` object (S3 class) containing:

- random_cv:

  Data frame of metrics from random CV (one row per repeat)

- blocked_cv:

  Data frame of metrics from blocked CV (one row per repeat)

- summary:

  Summary statistics comparing the two approaches

- inflation:

  Estimated metric inflation from using random CV

- diagnosis:

  The BorgDiagnosis object used

- p_value:

  P-value from paired t-test comparing approaches

## Details

This function provides the "smoking gun" evidence for reviewers. It runs
cross-validation twice on the same data:

1.  **Random CV**: Standard k-fold CV ignoring data structure

2.  **Blocked CV**: Structure-aware CV based on BORG diagnosis

The difference in metrics demonstrates empirically how much random CV
inflates performance estimates when data dependencies exist.

For stable estimates, the comparison is repeated multiple times
(default: 10) and a paired t-test assesses whether the difference is
statistically significant.

## See also

[`borg_diagnose`](https://gillescolling.com/BORG/reference/borg_diagnose.md)
for dependency detection,
[`borg_cv`](https://gillescolling.com/BORG/reference/borg_cv.md) for
generating blocked CV folds.

## Examples

``` r
# Spatial data example
set.seed(42)
n <- 200
spatial_data <- data.frame(
  x = runif(n, 0, 100),
  y = runif(n, 0, 100)
)
# Create spatially autocorrelated response
spatial_data$response <- spatial_data$x * 0.5 + rnorm(n, sd = 5)

# Compare CV approaches
comparison <- borg_compare_cv(
  spatial_data,
  formula = response ~ x + y,
  coords = c("x", "y"),
  repeats = 5  # Use more repeats in practice
)

print(comparison)
plot(comparison)
```
