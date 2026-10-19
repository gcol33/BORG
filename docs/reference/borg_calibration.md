# Model Calibration Diagnostics

Assesses whether a model's predictions are well-calibrated. For
classification, checks if predicted probabilities match observed
frequencies. For regression, checks if predicted quantiles have correct
coverage. Optionally uses spatial-aware binning to avoid autocorrelation
artifacts in calibration curves.

## Usage

``` r
borg_calibration(
  predicted,
  actual,
  type = NULL,
  n_bins = 10,
  coords = NULL,
  strategy = c("uniform", "quantile", "spatial")
)
```

## Arguments

- predicted:

  Numeric vector. Predicted values (probabilities for classification,
  point predictions for regression).

- actual:

  Numeric or factor vector. Observed outcomes (0/1 for classification,
  continuous for regression).

- type:

  Character. `"classification"` (default if actual is 0/1 or factor) or
  `"regression"`.

- n_bins:

  Integer. Number of bins for calibration curve. Default: 10.

- coords:

  Data frame or matrix with coordinate columns. If provided, uses
  spatial-aware binning (ensures bins are not spatially clustered).

- strategy:

  Character. Binning strategy: `"uniform"` (equal-width, default),
  `"quantile"` (equal-count), or `"spatial"` (spatial blocks as bins,
  requires `coords`).

## Value

A list with class `"borg_calibration"` containing:

- calibration_curve:

  Data frame with columns: bin_midpoint, observed_freq, predicted_mean,
  n, ci_lower, ci_upper

- ece:

  Expected Calibration Error (weighted mean \|observed - predicted\|)

- mce:

  Maximum Calibration Error (worst bin)

- brier_score:

  Brier score (classification) or calibration MSE (regression)

- type:

  Model type

- reliability_slope:

  Slope of observed ~ predicted regression (1.0 = perfect calibration)

- reliability_intercept:

  Intercept (0.0 = no bias)

- n_bins:

  Number of bins used

- assessment:

  Character: "well_calibrated", "moderate", or "poorly_calibrated"

Has [`print()`](https://rdrr.io/r/base/print.html) and `autoplot()`
methods.

## Examples

``` r
# Classification
set.seed(42)
probs <- runif(500)
outcomes <- rbinom(500, 1, probs^0.8)  # slightly miscalibrated
cal <- borg_calibration(probs, outcomes)
cal

# Regression
x <- rnorm(200)
y <- 2 * x + rnorm(200, sd = 0.5)
preds <- 2.1 * x  # slightly biased
cal_reg <- borg_calibration(preds, y, type = "regression")
cal_reg
```
