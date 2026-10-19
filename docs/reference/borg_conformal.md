# Conformal Prediction with Spatial Dependence

Constructs distribution-free prediction intervals with finite-sample
coverage guarantees, adjusted for spatial autocorrelation. Standard
conformal prediction assumes exchangeability, which spatial data
violates. This function uses spatially-blocked calibration residuals as
nonconformity scores, producing intervals that maintain coverage even
under dependence.

## Usage

``` r
borg_conformal(
  model,
  data,
  new = NULL,
  target,
  coords = NULL,
  alpha = 0.1,
  method = c("split", "block_jackknife", "block_cv"),
  folds = NULL,
  n_blocks = 10,
  type = c("regression", "classification"),
  seed = 42
)
```

## Arguments

- model:

  A fitted model with a
  [`predict()`](https://rdrr.io/r/stats/predict.html) method.

- data:

  Data frame used to compute nonconformity scores (calibration set).

- new:

  Data frame of new locations for prediction. If `NULL`, computes
  intervals for `data` via leave-one-block-out.

- target:

  Character. Target variable name in `data`.

- coords:

  Character vector of length 2. Coordinate column names. If provided,
  uses spatial blocking for calibration.

- alpha:

  Numeric. Miscoverage level. Default: 0.1 (90% intervals).

- method:

  Character. Conformal method: `"split"` (default) uses a held-out
  calibration set, `"block_jackknife"` uses leave-one-block-out,
  `"block_cv"` uses blocked cross-validation residuals.

- folds:

  A `borg_cv` object or fold list. If provided, residuals are computed
  from these folds (respecting spatial structure). Overrides internal
  blocking.

- n_blocks:

  Integer. Number of spatial blocks if `folds` is NULL and `coords` is
  provided. Default: 10.

- type:

  Character. For classification: `"regression"` (default) or
  `"classification"`. Classification uses adaptive prediction sets.

- seed:

  Integer. Random seed. Default: 42.

## Value

A data frame with class `"borg_conformal"` containing:

- prediction:

  Point prediction

- lower:

  Lower bound of prediction interval

- upper:

  Upper bound of prediction interval

- width:

  Interval width (upper - lower)

- x and y:

  Coordinates (if `coords` provided and `new` has them)

Attributes include `alpha`, `method`, `coverage_estimate`, and
`quantile_score` (the nonconformity threshold).

## Details

### Why spatial conformal?

Standard split conformal prediction computes residuals on a random
calibration set. Under spatial autocorrelation, nearby calibration
points produce correlated residuals, leading to underestimated interval
widths and actual coverage below the nominal level.

By using spatially-blocked calibration (where residuals come from
predictions on spatially separated test folds), the effective sample
size of nonconformity scores is honest, and coverage guarantees hold
approximately even under dependence.

### Methods

- `"split"`:

  Splits data into training and calibration sets using spatial blocks.
  Fast, but uses only part of the data.

- `"block_jackknife"`:

  Leave-one-block-out: refit the model excluding each block, predict on
  the held-out block. More data-efficient but slower.

- `"block_cv"`:

  Use pre-computed blocked CV residuals from a `borg_cv` object.
  Requires `folds`.

## References

Mao, H., Martin, R., & Reich, B. J. (2024). Valid prediction inference
with spatial conformal methods. *arXiv preprint arXiv:2403.14058*.

Johnstone, C., & Cox, D. (2023). Conformal prediction with spatial data.

Vovk, V., Gammerman, A., & Shafer, G. (2005). *Algorithmic Learning in a
Random World*. Springer.

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(200), y = runif(200), a = rnorm(200))
d$z <- 2 * d$a + sin(d$x * 10) + rnorm(200, sd = 0.5)
model <- lm(z ~ a + x + y, data = d)

# Split conformal with spatial blocking
conf <- borg_conformal(model, d, target = "z", coords = c("x", "y"))
conf

# Predict on new locations
new <- data.frame(x = runif(50), y = runif(50), a = rnorm(50))
pred <- borg_conformal(model, d, new = new, target = "z",
                        coords = c("x", "y"))
```
