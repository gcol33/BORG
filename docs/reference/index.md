# Package index

## Core Functions

Primary interface for evaluation risk detection

- [`borg_guard()`](https://gillescolling.com/BORG/reference/borg_guard.md)
  : Guard Model Evaluation Against Information Reuse
- [`borg_inspect()`](https://gillescolling.com/BORG/reference/borg_inspect.md)
  : Inspect R Objects for Evaluation Risks
- [`borg_validate()`](https://gillescolling.com/BORG/reference/borg_validate.md)
  : Validate Complete Evaluation Workflow

## Remediation

Automatic pipeline correction

- [`borg_rewrite()`](https://gillescolling.com/BORG/reference/borg_rewrite.md)
  : Automatically Rewrite Leaky Evaluation Pipelines

## Visualization

Plot functions for split analysis

- [`plot_split()`](https://gillescolling.com/BORG/reference/plot_split.md)
  : Plot Train/Test Split Distribution
- [`plot_risk()`](https://gillescolling.com/BORG/reference/plot_risk.md)
  : Plot Risk Assessment Summary
- [`plot_temporal()`](https://gillescolling.com/BORG/reference/plot_temporal.md)
  : Plot Temporal Validation
- [`plot_spatial()`](https://gillescolling.com/BORG/reference/plot_spatial.md)
  : Plot Spatial Split
- [`plot_groups()`](https://gillescolling.com/BORG/reference/plot_groups.md)
  : Plot Group-Based Split

## Audit Functions

Prediction and CV auditing

- [`audit_predictions()`](https://gillescolling.com/BORG/reference/audit_predictions.md)
  : Audit Predictions for Data Leakage
- [`cv_leakage_report()`](https://gillescolling.com/BORG/reference/cv_leakage_report.md)
  : Generate CV Leakage Report

## Classes

Result objects

- [`show(`*`<BorgRisk>`*`)`](https://gillescolling.com/BORG/reference/BorgRisk.md)
  : BorgRisk S4 Class
- [`as.data.frame(`*`<BorgRisk>`*`)`](https://gillescolling.com/BORG/reference/as.data.frame.BorgRisk.md)
  : Coerce BorgRisk to Data Frame
- [`print(`*`<borg_cv_report>`*`)`](https://gillescolling.com/BORG/reference/print.borg_cv_report.md)
  : Print CV Leakage Report
