## Resubmission

This is a resubmission (previous submission ~January 2026 received no
response). Changes since last submission:

* Fixed Rd markup errors in `borg_conformal` and `borg_debias` caused by
  escaped percent signs (`\%`) under roxygen2 markdown mode.
* Fixed non-ASCII em-dash characters in `R/borg_explain.R`.
* Fixed failing example in `borg_rset()`.
* Bumped minimum R version from 3.5 to 4.1.0 (package uses native pipe `|>`).
* Added ggplot2 `autoplot()` methods and sf/terra spatial input support.
* Unified API: harmonized parameters and extracted shared helpers.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* local: Windows 11 x64, R 4.5.2
* win-builder: R-devel
* mac-builder: macOS, R-release

## Package details

BORG detects data leakage in machine learning evaluation workflows.
It validates train/test splits, cross-validation schemes, and preprocessing
pipelines for spatial, temporal, and grouped data structures.

The package has Suggested packages because it integrates with multiple
ML frameworks (caret, tidymodels/rsample/recipes, mlr3) and model types
(ranger, xgboost, lightgbm, parsnip). All Suggested packages are
conditionally loaded with `requireNamespace()` checks; the core
functionality works with base R only.

## Downstream dependencies

No reverse dependencies (new package).
