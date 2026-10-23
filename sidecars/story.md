# Story: BORG

## Claim
Checks model evaluation on spatial, temporal and grouped data: diagnoses dependence, builds cross-validation folds that respect it, and checks splits for leakage.

## Audience
Ecologists and data scientists evaluating predictive models on dependent data; may know blockCV, CAST, spatialsample, rsample, caret or mlr3.

## Relation
- to: none (sits beside blockCV, CAST, spatialsample; exports folds to rsample, caret, mlr3, ENMeval, biomod2)
- home: vignettes/frameworks.Rmd

## Arcs

### A1 Diagnose, then build folds
- answer: borg() tests Moran's I / ACF-Ljung-Box / ICC, recommends a scheme, disables random CV unless allow_random
- home: vignettes/quickstart.Rmd, vignettes/spatial-cv.Rmd, vignettes/temporal-cv.Rmd
- also: README (opening, Usage), DESCRIPTION

### A2 How much random CV inflates the metric
- answer: borg_compare_cv() (paired t-test), borg_power() (effective n, design effect)
- home: vignettes/model-comparison.Rmd
- also: README (Usage)

### A3 Leakage in an existing split
- answer: borg_inspect()/borg() validation mode: overlap, duplicates, preprocessing (prcomp, caret::preProcess, recipes), target, group, look-ahead
- home: vignettes/risk-taxonomy.Rmd
- also: README

### A4 Area of applicability and maps
- answer: borg_aoa(), borg_di() after Meyer and Pebesma (2021); extraction, thinning, prediction rasters, leaflet
- home: vignettes/aoa-extrapolation.Rmd
- also: README

### A5 Reporting
- answer: summary() methods paragraph (APA, Nature, Ecology); borg_certificate(), borg_export()
- home: ?summary.borg_result
- also: README (Usage)

## Documents
- README / landing: claim, mechanism, Usage (A1, A2, A5), sections for A3, A4, routing
- Quick Start: tutorial on simulated data
- Spatial, Temporal, Risk Taxonomy, Frameworks, Model Comparison, Feature Selection, AOA, Post-Fit Diagnostics
