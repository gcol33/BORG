
# BORG

*cross-validation for data that isn’t independent*

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/BORG)](https://CRAN.R-project.org/package=BORG)
[![R-CMD-check](https://github.com/gcol33/BORG/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gcol33/BORG/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/gcol33/BORG/graph/badge.svg)](https://app.codecov.io/gh/gcol33/BORG)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

BORG (Bounded Outcome Risk Guard) checks how a model is evaluated on
spatial, temporal and grouped data: it tests whether observations depend
on each other, builds cross-validation folds that keep dependent
observations apart, and checks train/test splits for leakage, such as
preprocessing fitted on the test rows.

Dependence is tested from the columns that carry structure: Moran’s I on
the coordinates for spatial autocorrelation, the autocorrelation
function and the Ljung-Box test on a time column, and the intraclass
correlation for groups. The result is a diagnosis with a severity and a
recommended fold scheme, and the folds are built from that scheme. When
dependence is found, random k-fold cross-validation is switched off
unless you request it, and the override is recorded in the result. You
can then use the folds directly or compare them with random folds before
deciding which estimate to report.

BORG supports:

- **Fold schemes for dependent data.**
  [`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.html)
  builds spatial blocks, environmental blocks, checkerboard and
  hexagonal blocks, leave-location-out, buffered spatial folds, KNNDM
  folds matched to prediction points, chronological splits with an
  embargo, expanding and sliding windows, purged cross-validation,
  leave-group-out and spatial-temporal combinations.
- **Folds for modelling frameworks.** The folds come as a list of train
  and test indices, or as objects for
  [rsample](https://rsample.tidymodels.org),
  [caret](https://github.com/topepo/caret/) and
  [mlr3](https://cran.r-project.org/package=mlr3).
- **Random against blocked cross-validation.**
  [`borg_compare_cv()`](https://gillescolling.com/BORG/reference/borg_compare_cv.html)
  runs both on the same data and model and tests the difference with a
  paired t-test;
  [`borg_power()`](https://gillescolling.com/BORG/reference/borg_power.html)
  reports the effective sample size after blocking and the smallest
  effect still detectable.
- **Leakage checks.**
  [`borg_inspect()`](https://gillescolling.com/BORG/reference/borg_inspect.html)
  checks splits and fitted objects for overlapping indices, duplicate
  rows (hash-based checks in C++), preprocessing fitted beyond the
  training rows, target leakage, group leakage and look-ahead in time.
- **Area of applicability.** `borg_aoa()` and `borg_di()` mark
  prediction locations outside the predictor space of the training data,
  following Meyer and Pebesma (2021).
- **Methods text.** `summary()` on a result writes a methods paragraph
  with the test statistics filled in, in APA, Nature or Ecology style,
  and `borg_certificate()` and `borg_export()` write the assessment as
  YAML or JSON.

The diagnostics follow evaluation principles described by Roberts et
al. (2017), Kaufman et al. (2012), Kapoor and Narayanan (2023) and
Linnenbrink et al. (2024), with the references in the DESCRIPTION. [Risk
Taxonomy](https://gillescolling.com/BORG/articles/risk-taxonomy.html)
lists each check, what it detects and how to fix it.

## Installation

Install the released version from CRAN. Windows and macOS get a binary
package and need no build tools:

``` r
install.packages("BORG")
```

The development version compiles C++ code. Install the build tools for
your operating system first:

| Operating system | Build tools |
|----|----|
| Windows | [Rtools](https://cran.r-project.org/bin/windows/Rtools/) matching your R version |
| macOS | Run `xcode-select --install` in Terminal |
| Debian / Ubuntu | Install `r-base-dev` |

Check that R can find the compiler:

``` r
pkgbuild::has_build_tools(debug = TRUE)
```

Then install BORG from GitHub:

``` r
# install.packages("pak")
pak::pak("gcol33/BORG")
```

BORG needs no system libraries beyond the compiler. Its C++ code links
only to Rcpp, which is installed from CRAN with the package.

## Usage

1.  Diagnose the data and build folds with `borg()`.
2.  Compare random and blocked cross-validation with
    `borg_compare_cv()`.
3.  Check an existing split or fitted preprocessing with
    `borg_inspect()`.

The input is a data frame, an sf object or a terra `SpatVector`.
`coords` names the two coordinate columns, `time` the time column and
`groups` the grouping column; `target` names the response. With an sf or
`SpatVector` input the coordinates are taken from the geometry.

``` r
library(BORG)

set.seed(1)
n <- 200
d <- data.frame(lon = runif(n, 0, 20), lat = runif(n, 0, 20))
d$response <- sin(d$lon / 3) + cos(d$lat / 3) + rnorm(n, sd = 0.3)

result <- borg(d, coords = c("lon", "lat"), target = "response")
result
#> BORG Result
#> ===========
#> 
#> Dependency:  SPATIAL (moderate severity)
#> Strategy:    spatial_block
#> Folds:       5
#> 
#> Random CV would inflate metrics by ~8%
#> 
#> Fold sizes:  train 148-174, test 26-52
#> 
#> Access components:
#>   $diagnosis  - BorgDiagnosis object
#>   $folds      - List of train/test index vectors
#>   $cv         - Full borg_cv object
```

The response varies smoothly in space, so the diagnosis finds spatial
autocorrelation and builds five spatial blocks instead of random folds.

`borg_compare_cv()` fits the same model on random and on blocked folds
and reports the gap:

``` r
cmp <- borg_compare_cv(
  d,
  formula = response ~ lon + lat,
  coords = c("lon", "lat"),
  repeats = 5,
  seed = 1,
  verbose = FALSE
)
cmp
#> CV Comparison Results
#> =====================
#> 
#> Metric: rmse
#> Random CV:  0.8806 (SD: 0.0040)
#> Blocked CV: 1.2735 (SD: 0.0000)
#> 
#> Inflation: 30.8% deflated***
#> p-value: 2.522e-09 (paired t-test, n=5 repeats)
#> 
#> Random CV significantly deflates rmse estimates.
```

`summary()` turns the diagnosis into a paragraph for the methods
section:

``` r
summary(result)
#> 
#> Spatial autocorrelation was detected in the data (Moran's I = 0.29, p < 0.001) with an estimated autocorrelation range of 10.9 units. Model performance was evaluated using spatial block cross-validation (k = 5 folds). Random cross-validation was avoided as it would be expected to inflate performance estimates by approximately 8%. Cross-validation strategy was determined using the BORG package (version 0.3.2) for R.
```

## Fold schemes and modelling frameworks

`borg()` chooses the scheme from the diagnosis, and `borg_cv()` takes a
scheme by name through `strategy`. `output = "rsample"` returns folds
that tidymodels functions accept:

``` r
folds <- borg(d, coords = c("lon", "lat"), target = "response",
              output = "rsample")
```

[Spatial
Cross-Validation](https://gillescolling.com/BORG/articles/spatial-cv.html)
and [Temporal
Cross-Validation](https://gillescolling.com/BORG/articles/temporal-cv.html)
go through each scheme, and [Framework
Integration](https://gillescolling.com/BORG/articles/frameworks.html)
covers caret, mlr3,
[ENMeval](https://cran.r-project.org/package=ENMeval) and
[biomod2](https://cran.r-project.org/package=biomod2).

## Checking a split for leakage

With `train_idx` and `test_idx`, `borg()` and `borg_inspect()` check an
existing split. Passed a fitted preprocessing object and the data,
`borg_inspect()` checks whether its parameters were estimated on rows
beyond the training set. Here a principal component analysis was fitted
on all rows:

``` r
x <- iris[, 1:4]
pca <- prcomp(x, scale. = TRUE)
borg_inspect(pca, train_idx = 1:100, test_idx = 101:150, data = x)
#> BorgRisk Assessment
#> ===================
#> 
#> Status: INVALID (3 hard violations) — Resistance is futile
#>   Hard violations:  3
#>   Soft inflations:  0
#>   Train indices:    100 rows
#>   Test indices:     50 rows
#>   Inspected at:     2026-10-23 01:28:36
#> 
#> --- HARD VIOLATIONS (must fix) ---
#> 
#> [1] preprocessing_leak
#>     PCA centering was computed on data beyond training set (mean difference: 0.897)
#>     Source: prcomp
#>     Affected: 50 indices (first 5: 101, 102, 103, 104, 105)
#>     Fix: Refit preprocessing on training data only. Auto-fix: borg_assimilate(workflow)
#> 
#> [2] preprocessing_leak
#>     PCA scaling was computed on data beyond training set (sd difference: 0.31575)
#>     Source: prcomp
#>     Affected: 50 indices (first 5: 101, 102, 103, 104, 105)
#>     Fix: Refit preprocessing on training data only. Auto-fix: borg_assimilate(workflow)
#> 
#> [3] preprocessing_leak
#>     PCA loadings were computed on data beyond training set (max loading difference: 0.1729)
#>     Source: prcomp
#>     Affected: 50 indices (first 5: 101, 102, 103, 104, 105)
#>     Fix: Refit preprocessing on training data only. Auto-fix: borg_assimilate(workflow)
```

Hard violations (overlap, duplicates, preprocessing leakage, target
leakage, group leakage, look-ahead) mark the evaluation invalid. Soft
inflations, such as a predictor close to the threshold for target
leakage or test points close to training points, give a warning and are
recorded.

## Area of applicability and prediction maps

`borg_aoa()` and `borg_di()` compute the dissimilarity index and the
area of applicability of Meyer and Pebesma (2021) for prediction
locations. `borg_extract()`, `borg_thin()`, `borg_predict_raster()` and
`borg_leaflet()` cover raster extraction, spatial thinning, prediction
maps and an interactive map of the folds. [Area of Applicability and
Extrapolation
Detection](https://gillescolling.com/BORG/articles/aoa-extrapolation.html)
shows them on simulated data.

## Documentation

Start with [Quick
Start](https://gillescolling.com/BORG/articles/quickstart.html):
simulated data with known dependence, from the diagnosis to folds,
leakage checks and the comparison of random and blocked
cross-validation.

- [Spatial
  Cross-Validation](https://gillescolling.com/BORG/articles/spatial-cv.html):
  spatial blocks, block size optimization, environmental blocking,
  buffered folds and KNNDM.
- [Temporal
  Cross-Validation](https://gillescolling.com/BORG/articles/temporal-cv.html):
  temporal blocks, expanding windows, embargo periods and concept drift.
- [Risk
  Taxonomy](https://gillescolling.com/BORG/articles/risk-taxonomy.html):
  each leakage check, what it detects and how to fix it.
- [Framework
  Integration](https://gillescolling.com/BORG/articles/frameworks.html):
  folds for tidymodels, caret, mlr3, ENMeval and biomod2.
- [Model Comparison with Blocked
  Cross-Validation](https://gillescolling.com/BORG/articles/model-comparison.html):
  the inflation from random folds, model ranking, confidence intervals
  and null tests.
- [Feature Selection with Blocked
  Cross-Validation](https://gillescolling.com/BORG/articles/feature-selection.html):
  block-permutation importance and forward selection.
- [Area of Applicability and Extrapolation
  Detection](https://gillescolling.com/BORG/articles/aoa-extrapolation.html):
  where predictions leave the training data.
- [Post-Fit
  Diagnostics](https://gillescolling.com/BORG/articles/diagnostics.html):
  residual structure, calibration, null tests, conformal prediction and
  bootstrap uncertainty.
- [Full reference](https://gillescolling.com/BORG/reference/): every
  function.

## Getting help

Questions and bug reports go to the [GitHub
issues](https://github.com/gcol33/BORG/issues). A bug report is easiest
to act on with a small reproducible example, for instance made with
[reprex](https://reprex.tidyverse.org/).

## Contributing

Pull requests are welcome; see the [contributing
guide](https://gillescolling.com/BORG/CONTRIBUTING.html).

## Support

> “Software is like sex: it’s better when it’s free.” — Linus Torvalds

I’m a PhD student who builds R packages in my free time because I
believe good tools should be free and open. I started these projects for
my own work and figured others might find them useful too.

If this package saved you some time, buying me a coffee is a nice way to
say thanks. It helps with my coffee addiction.

[![Buy Me A
Coffee](https://img.shields.io/badge/-Buy%20me%20a%20coffee-FFDD00?logo=buymeacoffee&logoColor=black)](https://buymeacoffee.com/gcol33)

## License

BORG is licensed under MIT.

## Citation

To cite BORG in publications use:

``` text
Colling G (2026). _BORG: Bounded Outcome Risk Guard for Model
Evaluation_. doi:10.32614/CRAN.package.BORG
<https://doi.org/10.32614/CRAN.package.BORG>. R package version 0.3.2,
<https://gillescolling.com/BORG/>.
```

BibTeX:

``` bibtex
@Manual{colling2026borg,
  title = {BORG: Bounded Outcome Risk Guard for Model Evaluation},
  author = {Gilles Colling},
  year = {2026},
  note = {R package version 0.3.2},
  doi = {10.32614/CRAN.package.BORG},
  url = {https://gillescolling.com/BORG/},
}
```

Download the citation as
[BibTeX](https://gillescolling.com/BORG/BORG.bib) or
[RIS](https://gillescolling.com/BORG/BORG.ris).
