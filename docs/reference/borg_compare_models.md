# Compare Multiple Models with Spatial CV

Evaluates multiple models (or formulas) using the same blocked CV folds,
producing a side-by-side comparison table.

## Usage

``` r
borg_compare_models(
  data,
  folds,
  models,
  metric = c("rmse", "mae", "rsq", "auc", "tss", "kappa"),
  fit_fun = stats::lm
)
```

## Arguments

- data:

  Data frame.

- folds:

  A `borg_cv` object or fold list.

- models:

  Named list of either:

  - Formulas: `list(lm = y ~ x, full = y ~ x + z)`

  - Fitted models: `list(lm = lm_fit, rf = rf_fit)`

- metric:

  Character. Default: `"rmse"`.

- fit_fun:

  Function. Used when `models` contains formulas. Default: `lm`.

## Value

A data frame with class `"borg_model_comparison"` containing model name,
mean metric, SD, and rank. Has `autoplot()` method.

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(100), y = runif(100),
                 a = rnorm(100), b = rnorm(100))
d$z <- d$a * 2 + rnorm(100, sd = 0.5)
cv <- borg_cv(d, coords = c("x", "y"), target = "z")
comp <- borg_compare_models(d, cv,
  models = list(simple = z ~ a, full = z ~ a + b))
comp
```
