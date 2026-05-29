# BORG

*great in cross-validation, useless in the field*

[![CRAN status](https://www.r-pkg.org/badges/version/BORG)](https://CRAN.R-project.org/package=BORG)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/BORG)](https://cran.r-project.org/package=BORG)
[![Monthly downloads](https://cranlogs.r-pkg.org/badges/BORG)](https://cran.r-project.org/package=BORG)
[![R-CMD-check](https://github.com/gcol33/BORG/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gcol33/BORG/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/gcol33/BORG/graph/badge.svg)](https://app.codecov.io/gh/gcol33/BORG)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Cross-validation that respects spatial, temporal, and grouped structure, with the dependency detected for you.**

Hand BORG your data and the columns that carry structure (coordinates, a time
stamp, a grouping ID). It tests for spatial autocorrelation (Moran's I),
temporal autocorrelation (ACF / Ljung-Box), and clustering (ICC), then builds
folds that hold the structure out instead of splitting through it. When a
dependency is present, random k-fold is disabled unless you ask for it
explicitly. The point is the number you report from CV being the number you get
in the field.

```r
library(BORG)

spatial_data <- data.frame(
  lon = runif(200, -10, 10),
  lat = runif(200, -10, 10),
  response = rnorm(200)
)

# diagnose the dependency and get folds that respect it
result <- borg(spatial_data, coords = c("lon", "lat"), target = "response")
result$diagnosis@recommended_cv
#> "spatial_block"
```

## Diagnose first, then enforce

Spatial CV tools such as `blockCV`, `CAST`, and `spatialsample` give you the
blocking schemes but leave the decision to you: you have to know the data are
autocorrelated, pick a block size, and choose to use it. BORG runs the
diagnosis first. If it finds structure, it estimates how much random CV would
inflate your metric, refuses random folds, and generates a scheme matched to
the dependency it found. Overriding is one explicit argument away, and the
override is logged.

```r
borg_diagnose(spatial_data, coords = c("lon", "lat"), target = "response")
#> Dependency:  SPATIAL (moderate severity)
#> Moran's I:   0.18 (p < 0.001)
#> Strategy:    spatial_block
```

## Cross-validation schemes

`borg_cv()` covers the schemes used in dependent-data evaluation:

- **Spatial**: spatial blocking, environmental blocking, checkerboard,
  hexagonal, leave-location-out, spatial-plus (buffered), and KNNDM matched to
  prediction points.
- **Temporal**: chronological splits with an embargo, expanding and sliding
  windows, and de Prado's purged CV.
- **Grouped**: leave-group-out, so no cluster appears in both train and test.
- **Mixed**: spatial-temporal and spatial-group combinations.

Output is a plain list of train/test indices by default, or an `rsample`,
`caret`, or `mlr3` object for direct use in those frameworks.

```r
# tidymodels-compatible folds
folds <- borg(spatial_data, coords = c("lon", "lat"), output = "rsample")
```

## Show the inflation, not just the warning

`borg_compare_cv()` runs random and structure-aware CV on the same data and
model, and reports the gap with a paired t-test. This is the evidence a
reviewer asks for when you switch a paper from random to blocked CV.

```r
comparison <- borg_compare_cv(
  spatial_data,
  formula = response ~ lon + lat,
  coords  = c("lon", "lat"),
  repeats = 10
)
print(comparison)
plot(comparison)
```

`borg_power()` answers the other half of that conversation: blocking costs you
effective sample size, so it reports the design effect, the effective n, and
the minimum effect you can still detect.

## Leakage detection

Before structure, BORG checks the split itself. `borg()` in validation mode and
`borg_inspect()` catch the leaks that quietly inflate a held-out score:

- index overlap and duplicate rows across train and test (fast hash-based
  checks in C++),
- preprocessing fitted on the full data (`scale`, `prcomp`, `caret::preProcess`,
  `recipes` steps),
- target leakage from a feature near-perfectly correlated with the outcome,
- group leakage and look-ahead in time.

```r
# scaled before splitting: the test means leaked into the scaler
data_scaled <- scale(iris[, 1:4])
borg_inspect(data_scaled, train_idx = 1:100, test_idx = 101:150)
#> INVALID — Hard violation: preprocessing_leak
```

Hard violations (overlap, duplicates, preprocessing leak, target leak, group
leak, look-ahead) invalidate the result and block it. Soft inflations (a
near-threshold proxy feature, test points close to training) warn and let you
proceed with the caveat recorded.

## Area of applicability and prediction maps

`borg_aoa()` and `borg_di()` flag prediction locations that sit outside the
feature space the model was trained on, following Meyer & Pebesma (2021).
`borg_extract()`, `borg_thin()`, `borg_predict_raster()`, and `borg_leaflet()`
cover the surrounding spatial workflow, from raster extraction to an
interactive map of folds.

## Methods text for the manuscript

`summary()` on a BORG result writes a methods paragraph with the test
statistics filled in (Moran's I, ACF, ICC, and their p-values), in APA, Nature,
or Ecology style. `borg_certificate()` and `borg_export()` write the same
assessment as a machine-readable YAML or JSON record for an audit trail.

```r
summary(result)
#> Model performance was evaluated using spatial block cross-validation
#> (k = 5 folds). Spatial autocorrelation was detected (Moran's I = 0.18,
#> p < 0.001) ...
```

## Installation

```r
install.packages("pak")           # development version
pak::pak("gcol33/BORG")
```

## Documentation

- [BORG website](https://gillescolling.com/BORG/)
- [Function reference](https://gillescolling.com/BORG/reference/)
- [Source on GitHub](https://github.com/gcol33/BORG)

## Support

> "Software is like sex: it's better when it's free." — Linus Torvalds

I'm a PhD student who builds R packages in my free time because I believe good tools should be free and open. I started these projects for my own work and figured others might find them useful too.

If this package saved you some time, buying me a coffee is a nice way to say thanks. It helps with my coffee addiction.

[![Buy Me A Coffee](https://img.shields.io/badge/-Buy%20me%20a%20coffee-FFDD00?logo=buymeacoffee&logoColor=black)](https://buymeacoffee.com/gcol33)

## License

MIT (see the LICENSE.md file)

## Citation

```bibtex
@software{BORG,
  author = {Colling, Gilles},
  title = {BORG: Bounded Outcome Risk Guard for Model Evaluation},
  year = {2026},
  url = {https://github.com/gcol33/BORG}
}
```
