# BORG

[![CRAN status](https://www.r-pkg.org/badges/version/BORG)](https://CRAN.R-project.org/package=BORG)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/BORG)](https://cran.r-project.org/package=BORG)
[![Monthly downloads](https://cranlogs.r-pkg.org/badges/BORG)](https://cran.r-project.org/package=BORG)
[![R-CMD-check](https://github.com/gcol33/BORG/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gcol33/BORG/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/gcol33/BORG/graph/badge.svg)](https://app.codecov.io/gh/gcol33/BORG)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Bounded Outcome Risk Guard for Model Evaluation**

BORG catches data leakage that inflates your model's performance — before you report the wrong number.

## Quick Start

```r
library(BORG)

# You scaled the data, then split it. Looks fine?
data_scaled <- scale(iris[, 1:4])
train_idx <- 1:100
test_idx <- 101:150

borg_inspect(data_scaled, train_idx = train_idx, test_idx = test_idx)
#> INVALID — Hard violation: preprocessing_leak
#> "Normalization parameters were computed on data beyond training set"
```

The test set means leaked into the scaler. Your reported accuracy is wrong.
BORG finds this automatically — for scaling, PCA, recipes, caret pipelines,
and more.

## Statement of Need

A model shows 95% accuracy on test data, then drops to 60% in production. The usual cause: data leakage. Information from the test set contaminated training, and the reported metrics were wrong.

A [Princeton meta-analysis](https://reproducible.cs.princeton.edu/) found leakage errors in **648 published papers across 30 fields**. In civil war prediction research, correcting leakage revealed that "complex ML models do not perform substantively better than decades-old Logistic Regression." The reported gains were artifacts.

BORG addresses this problem by **automatically detecting** six categories of leakage — index overlap, duplicate rows, preprocessing leakage, target leakage, group leakage, and temporal violations — across common R frameworks (base R, caret, tidymodels, mlr3). Beyond detection, BORG diagnoses data dependencies (spatial, temporal, clustered), generates appropriate cross-validation schemes, and produces publication-ready methods paragraphs with test statistics.

These features make the package useful in domains like:

- ecological and environmental modeling (spatial/temporal autocorrelation),
- clinical research (repeated measures, patient clustering),
- any predictive modeling workflow where evaluation integrity matters.

## Features

### Core Validation

- **`borg()`**: Main entry point for all validation
  - Validates train/test splits against data
  - Detects preprocessing leakage (scaling, PCA fitted on full data)
  - Checks for target leakage (features derived from outcome)
  - Validates grouped data (same patient in train and test)
  - Validates temporal data (test predates training)
  - Validates spatial data (test points too close to training)

- **`borg_inspect()`**: Detailed inspection of specific objects
  - Works with `caret::preProcess`, `recipes::recipe`, `prcomp`
  - Checks `rsample` resampling objects
  - Validates fitted models (`lm`, `glm`, `ranger`, etc.)

- **`borg_diagnose()`**: Analyze data for dependency structure
  - Detects spatial autocorrelation (Moran's I)
  - Detects temporal autocorrelation (ACF/Ljung-Box)
  - Detects clustered structure (ICC)
  - Recommends appropriate CV strategy

### Empirical Evidence & Power Analysis

- **`borg_compare_cv()`**: Run random and blocked CV side by side on the same data
  - Produces the "smoking gun" evidence for reviewers
  - Paired t-test quantifies metric inflation
  - `plot()` for visual comparison

- **`borg_power()`**: Estimate power loss from switching to blocked CV
  - Design effect from Moran's I, ACF, or ICC
  - Reports effective sample size and minimum detectable effect
  - Answers "is my dataset large enough for blocked CV?"

### Publication Support

- **`summary()`**: Generate publication-ready methods paragraphs
  - Includes test statistics (Moran's I, ACF, ICC with p-values)
  - Three citation styles: APA, Nature, Ecology
  - Integrates `borg_compare_cv()` inflation estimates when available
- **`borg_certificate()`** / **`borg_export()`**: Machine-readable validation certificates in YAML/JSON for audit trails

### Risk Categories

| Category | Impact | Response |
|----------|--------|----------|
| **Hard Violation** | Results invalid | Blocks evaluation |
| **Soft Inflation** | Results biased | Warns, allows with caution |

**Hard Violations:**
- `index_overlap` - Same row in train and test
- `duplicate_rows` - Identical observations across sets
- `preprocessing_leak` - Scaler/PCA fitted on full data
- `target_leakage` - Feature with |r| > 0.99 with target
- `group_leakage` - Same group in train and test
- `temporal_leak` - Test data predates training

**Soft Inflation:**
- `proxy_leakage` - Feature with |r| 0.95-0.99 with target
- `spatial_proximity` - Test points close to training
- `spatial_overlap` - Test inside training convex hull

## Installation

```r
# Install from GitHub
# install.packages("pak")
pak::pak("gcol33/BORG")

# Or using devtools
# install.packages("devtools")
devtools::install_github("gcol33/BORG")
```

## Usage Examples

### Validate a Train/Test Split

```r
library(BORG)

# Clean split — passes validation
result <- borg(iris, train_idx = 1:100, test_idx = 101:150)
result
#> Status: VALID
#>   Hard violations: 0
#>   Soft inflations: 0

# Overlapping indices — caught immediately
borg(iris, train_idx = 1:100, test_idx = 51:150)
#> INVALID — index_overlap: Train and test indices overlap (50 shared indices)
```

### Catch Leaky Preprocessing Pipelines

```r
# caret preProcess fitted on ALL data (common mistake)
library(caret)
pp <- preProcess(mtcars, method = c("center", "scale"))
borg_inspect(pp, train_idx = 1:25, test_idx = 26:32, data = mtcars)
#> Hard violation: preprocessing_leak
#> "preProcess centering parameters were computed on data beyond training set"
```

### Target Leakage Detection

```r
# Feature highly correlated with outcome
leaky_data <- data.frame(
 x = rnorm(100),
 outcome = rnorm(100)
)
leaky_data$leaked <- leaky_data$outcome + rnorm(100, sd = 0.01)

borg_inspect(leaky_data, train_idx = 1:70, test_idx = 71:100, target = "outcome")
#> Hard violation: target_leakage_direct
```

### Grouped Data Validation

```r
# Clinical data with patient IDs
clinical <- data.frame(
  patient_id = rep(1:10, each = 10),
  measurement = rnorm(100)
)

# Random split ignoring patients
set.seed(123)
idx <- sample(100)
train_idx <- idx[1:70]
test_idx <- idx[71:100]

borg_inspect(clinical, train_idx, test_idx, groups = "patient_id")
#> Hard violation: group_leakage
```

### Spatial Data Validation

```r
spatial_data <- data.frame(
  lon = runif(200, -10, 10),
  lat = runif(200, -10, 10),
  response = rnorm(200)
)

# Let BORG diagnose and generate appropriate CV folds
result <- borg(spatial_data, coords = c("lon", "lat"), target = "response", v = 5)
result$diagnosis@recommended_cv
#> "spatial_block"
```

### Empirical CV Comparison

```r
# Prove to reviewers that random CV inflates metrics
comparison <- borg_compare_cv(
  spatial_data,
  formula = response ~ lon + lat,
  coords = c("lon", "lat"),
  repeats = 10
)
print(comparison)
plot(comparison)
```

### Generate Methods Text for Papers

```r
# summary() writes a publication-ready methods paragraph
result <- borg(spatial_data, coords = c("lon", "lat"), target = "response")
summary(result)
#> Model performance was evaluated using spatial block cross-validation
#> (k = 5 folds). Spatial autocorrelation was detected in the data
#> (Moran's I = 0.12, p < 0.001)...

# Three citation styles
summary(result, style = "nature")
summary(result, style = "ecology")
```

### Framework Integration

BORG works with common ML frameworks:

```r
# caret
library(caret)
pp <- preProcess(mtcars[, -1], method = c("center", "scale"))
borg_inspect(pp, train_idx = 1:25, test_idx = 26:32, data = mtcars)

# tidymodels
library(recipes)
rec <- recipe(mpg ~ ., data = mtcars) |>
  step_normalize(all_numeric_predictors()) |>
  prep()
borg_inspect(rec, train_idx = 1:25, test_idx = 26:32, data = mtcars)
```

## Interface Summary

| Function | Purpose |
|----------|---------|
| `borg()` | Main entry point — diagnose data or validate splits |
| `borg_inspect()` | Detailed inspection of objects |
| `borg_diagnose()` | Analyze data dependencies |
| `borg_validate()` | Validate complete workflow |
| `borg_assimilate()` | Assimilate leaky pipelines into compliance |
| `borg_compare_cv()` | Empirical random vs blocked CV comparison |
| `borg_power()` | Power analysis after blocking |
| `plot()` | Visualize results |
| `summary()` | Generate methods text for papers |
| `borg_certificate()` | Create validation certificate |
| `borg_export()` | Export certificate to YAML/JSON |

## Documentation

- [Quick Start](https://gillescolling.com/BORG/articles/quickstart.html)
- [Risk Taxonomy](https://gillescolling.com/BORG/articles/risk-taxonomy.html)
- [Framework Integration](https://gillescolling.com/BORG/articles/frameworks.html)

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
