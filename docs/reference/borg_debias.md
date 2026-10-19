# Spatial+ Debiasing for Spatial Confounding

Implements the Spatial+ approach of Dupont et al. (2022) to address
spatial confounding. Spatially structured predictors share variance with
the spatial process, inflating their coefficients. Spatial+ removes the
spatial component from each predictor by regressing it on smooth spatial
coordinates, then uses the residuals as debiased predictors.

## Usage

``` r
borg_debias(
  data,
  predictors = NULL,
  coords,
  target = NULL,
  method = c("gam_approx", "tps"),
  df = 6,
  keep_original = FALSE
)
```

## Arguments

- data:

  Data frame with predictors and coordinates.

- predictors:

  Character vector. Predictor column names to debias. If `NULL`, uses
  all numeric columns except `coords` and `target`.

- coords:

  Character vector of length 2. Coordinate column names.

- target:

  Character. Target variable name (excluded from debiasing).

- method:

  Character. Spatial smoothing method: `"gam_approx"` (default) uses
  polynomial basis (no mgcv dependency), `"tps"` uses thin plate spline
  approximation.

- df:

  Integer. Degrees of freedom for the spatial smooth. Default: 6. Higher
  values capture more spatial structure but risk removing real signal.

- keep_original:

  Logical. If TRUE, returns both original and debiased columns (debiased
  columns have suffix `_debiased`). Default: FALSE.

## Value

A list with class `"borg_debias"` containing:

- data:

  Debiased data frame (original columns replaced or augmented)

- spatial_r2:

  Named numeric vector. R-squared of spatial smooth for each predictor
  (how much spatial structure was removed)

- predictors:

  Debiased predictor column names

- method:

  Smoothing method used

- df:

  Degrees of freedom

- assessment:

  Character: "minimal" (less than 10 percent spatial variance),
  "moderate" (10-40 percent), or "substantial" (over 40 percent)

Has [`print()`](https://rdrr.io/r/base/print.html) and `autoplot()`
methods.

## Details

### When to use

Use when your predictors have spatial structure (e.g. climate variables
that vary smoothly over space). If
[`borg_diagnose()`](https://gillescolling.com/BORG/reference/borg_diagnose.md)
detects spatial autocorrelation in both residuals and predictors,
spatial confounding is likely. Debiasing is especially important for
inference (coefficient interpretation) rather than prediction.

### How it works

For each predictor \\X_j\\:

1.  Fit \\X_j \sim f(s_1, s_2)\\ where \\f\\ is a smooth function of
    coordinates.

2.  Replace \\X_j\\ with the residuals \\X_j - \hat{f}(s_1, s_2)\\.

The residuals contain only the non-spatial variation in \\X_j\\.

## References

Dupont, E., Wood, S. N., & Augustin, N. H. (2022). Spatial+: A novel
approach to spatial confounding. *Biometrics*, 78(4), 1279-1290.
[doi:10.1111/biom.13656](https://doi.org/10.1111/biom.13656)

## Examples

``` r
set.seed(42)
d <- data.frame(
  x = runif(200, 0, 100), y = runif(200, 0, 100),
  temp = NA, elev = rnorm(200)
)
# temp has spatial structure
d$temp <- sin(d$x / 20) + cos(d$y / 20) + rnorm(200, sd = 0.3)
d$z <- 0.5 * d$temp + d$elev + rnorm(200, sd = 0.5)

db <- borg_debias(d, coords = c("x", "y"), target = "z")
db
```
