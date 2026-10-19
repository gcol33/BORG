# Block-Permutation Variable Importance

Computes permutation importance that respects spatial structure. Instead
of permuting individual rows (which breaks spatial autocorrelation and
inflates importance), permutes values within spatial blocks.

## Usage

``` r
borg_importance(
  model,
  data,
  target,
  coords = NULL,
  predictors = NULL,
  n_blocks = 10,
  n_rep = 10,
  metric = c("rmse", "mae"),
  seed = 42
)
```

## Arguments

- model:

  A fitted model with a
  [`predict()`](https://rdrr.io/r/stats/predict.html) method.

- data:

  Data frame with predictor columns.

- target:

  Character. Target variable name.

- coords:

  Character vector of length 2. Coordinate column names. If `NULL`,
  falls back to standard row permutation.

- predictors:

  Character vector. Variables to assess. If `NULL`, uses all columns
  except target and coords.

- n_blocks:

  Integer. Number of spatial blocks for permutation. Default: 10.

- n_rep:

  Integer. Number of permutation repeats per variable. Default: 10.

- metric:

  Character. `"rmse"` (default) or `"mae"`.

- seed:

  Integer. Random seed. Default: 42.

## Value

A data frame with class `"borg_importance"` containing:

- variable:

  Predictor name

- importance:

  Mean increase in error after permutation

- importance_sd:

  SD across repeats

- rank:

  Importance rank (1 = most important)

Has an `autoplot()` method.

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(100), y = runif(100),
                 a = rnorm(100), b = rnorm(100))
d$z <- d$a * 2 + rnorm(100, sd = 0.5)
model <- lm(z ~ a + b, data = d)
imp <- borg_importance(model, d, target = "z", coords = c("x", "y"))
imp
```
