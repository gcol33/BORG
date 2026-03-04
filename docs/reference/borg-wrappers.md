# BORG-Guarded Cross-Validation Functions

These functions wrap common cross-validation functions from popular ML
frameworks, adding automatic BORG validation. They block random CV when
data dependencies are detected.

## Details

BORG provides guarded versions of:

- [`borg_vfold_cv()`](https://gillescolling.com/BORG/reference/borg_vfold_cv.md):
  Wraps
  [`rsample::vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html)

- [`borg_group_vfold_cv()`](https://gillescolling.com/BORG/reference/borg_group_vfold_cv.md):
  Wraps
  [`rsample::group_vfold_cv()`](https://rsample.tidymodels.org/reference/group_vfold_cv.html)

- [`borg_initial_split()`](https://gillescolling.com/BORG/reference/borg_initial_split.md):
  Wraps
  [`rsample::initial_split()`](https://rsample.tidymodels.org/reference/initial_split.html)

When dependencies are detected, these functions either:

1.  Block the operation and suggest
    [`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md)
    instead

2.  Automatically switch to an appropriate blocked CV strategy
