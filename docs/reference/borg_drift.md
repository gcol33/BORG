# Detect Distribution Shift Between Training and Deployment Data

Goes beyond Area of Applicability (AOA) by quantifying *how* the
distribution changed and *which features* drifted. Combines univariate
tests (Kolmogorov-Smirnov per feature) with a multivariate classifier
two-sample test (train a model to distinguish train from deployment; AUC
\> 0.5 indicates shift).

## Usage

``` r
borg_drift(
  train,
  new,
  predictors = NULL,
  alpha = 0.05,
  n_perm = 100,
  seed = 42
)
```

## Arguments

- train:

  Data frame of training data.

- new:

  Data frame of deployment/prediction data.

- predictors:

  Character vector. Predictor column names. If `NULL`, uses all shared
  numeric columns.

- alpha:

  Numeric. Significance level for per-feature KS tests. Default: 0.05.

- n_perm:

  Integer. Number of permutations for the classifier two-sample test
  p-value. Default: 100. Set to 0 to skip.

- seed:

  Integer. Random seed. Default: 42.

## Value

A list with class `"borg_drift"` containing:

- feature_drift:

  Data frame with per-feature KS statistic, p-value, effect_size
  (Cohen's d), shifted (logical), and direction ("higher", "lower",
  "similar")

- n_shifted:

  Number of features with significant drift

- classifier_auc:

  AUC of train-vs-deployment classifier (0.5 = no shift, 1.0 = complete
  separation)

- classifier_pvalue:

  Permutation p-value for AUC

- overall_severity:

  Character: "none", "mild", "moderate", "severe"

- summary:

  One-sentence summary

Has [`print()`](https://rdrr.io/r/base/print.html) and `autoplot()`
methods.

## Details

### Univariate tests

For each feature, a two-sample Kolmogorov-Smirnov test detects
distributional differences. Effect size is measured by Cohen's d
(standardized mean difference). P-values are Bonferroni-corrected.

### Multivariate classifier test

A logistic regression is trained to distinguish training from deployment
observations. If the data distributions are identical, the classifier
achieves AUC ~ 0.5. Higher AUC indicates multivariate shift that may not
be captured by univariate tests alone. Statistical significance is
assessed via permutation.

## References

Ginsberg, T., Liang, Z., & Krishnan, R. G. (2023). A learning based
hypothesis test for harmful covariate shift. *ICLR*.

Lopez-Paz, D., & Oquab, M. (2017). Revisiting classifier two-sample
tests. *ICLR*.

## Examples

``` r
set.seed(42)
train <- data.frame(a = rnorm(200), b = rnorm(200), c = rnorm(200))
# Deployment: feature 'a' has shifted
deploy <- data.frame(a = rnorm(100, mean = 1), b = rnorm(100),
                      c = rnorm(100))
drift <- borg_drift(train, deploy)
drift
```
