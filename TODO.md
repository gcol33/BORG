# BORG v0.2.0 TODO

## Completed

- [x] Add `plot_groups()` visualization function
- [x] Update NEWS.md with v0.1.0 features
- [x] Rebuild pkgdown site
- [x] Feature importance audit (`audit_importance()`)
- [x] tidymodels tune integration (`tune_results` inspector)
- [x] CRAN preparation (0 errors, 0 warnings, 0 notes)
- [x] Add suggested fixes to risk reports (`.suggest_fix()` + show/as.data.frame)
- [x] Framework hooks (`borg_auto_check()`, `borg_register_hooks()`)
- [x] Improved ggplot2 autoplot methods (lollipop BorgRisk, temporal/groups, BorgDiagnosis thresholds)
- [x] Better terra/sf spatial integration (tidyterra `geom_spatvector()`, native sf geometry)
- [x] Renamed `borg_rewrite()` to `borg_assimilate()` with 6 rewriters

## Remaining

- [ ] MLOps logging (mlflow, vetiver)
- [ ] Shiny dashboard for interactive exploration
- [ ] CRAN submission
