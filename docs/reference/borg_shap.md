# Spatial SHAP Values

Approximates SHAP values using block-conditional expectations instead of
naive row permutation. Standard SHAP marginalizes over all observations,
which creates impossible feature combinations when data is spatially
structured. Spatial SHAP marginalizes within spatial blocks, preserving
realistic covariate relationships.

## Usage

``` r
borg_shap(
  model,
  data,
  target,
  coords = NULL,
  predictors = NULL,
  n_blocks = 10,
  n_samples = 50,
  explain_idx = NULL,
  seed = 42
)
```

## Arguments

- model:

  A fitted model with a
  [`predict()`](https://rdrr.io/r/stats/predict.html) method.

- data:

  Data frame with predictors.

- target:

  Character. Target variable name (excluded from SHAP).

- coords:

  Character vector of length 2. Coordinate column names. If provided,
  uses spatial blocking for marginal expectations.

- predictors:

  Character vector. Variables to compute SHAP for. If `NULL`, uses all
  numeric columns except target and coords.

- n_blocks:

  Integer. Number of spatial blocks for marginal expectations. Default:
  10.

- n_samples:

  Integer. Number of background samples per block for marginal
  expectations. Default: 50.

- explain_idx:

  Integer vector. Row indices to explain. If `NULL`, explains all rows
  (can be slow).

- seed:

  Integer. Random seed. Default: 42.

## Value

A list with class `"borg_shap"` containing:

- shap_values:

  Matrix (n_explain x n_predictors) of SHAP values

- baseline:

  Mean prediction (expected value)

- feature_importance:

  Named vector of mean \|SHAP\| per feature

- predictors:

  Feature names

- method:

  "spatial_block" or "standard"

Has [`print()`](https://rdrr.io/r/base/print.html) and `autoplot()`
methods.

## Details

### Algorithm

For each observation \\x_i\\ and feature \\j\\:

1.  Identify the spatial block containing \\x_i\\.

2.  Sample background points from *other* blocks.

3.  Compute marginal contribution: replace feature \\j\\ with values
    from background points, keeping all other features fixed.

4.  Average the change in prediction.

This approximates the Shapley value while respecting spatial structure.

### Why not standard SHAP?

Standard kernel SHAP or marginal SHAP samples replacement values
uniformly from the dataset. For spatial data, this creates combinations
that never occur in reality (e.g., tropical temperature with arctic
precipitation), biasing the SHAP values.

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(100), y = runif(100),
                 a = rnorm(100), b = rnorm(100))
d$z <- 3 * d$a - d$b + rnorm(100, sd = 0.5)
model <- lm(z ~ a + b, data = d)
shap <- borg_shap(model, d, target = "z", coords = c("x", "y"),
                   explain_idx = 1:20)
shap
```
