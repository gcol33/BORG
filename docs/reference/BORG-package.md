# BORG: Bounded Outcome Risk Guard for Model Evaluation

Comprehensive toolkit for valid spatial, temporal, and grouped model
evaluation. Automatically detects data dependencies (spatial
autocorrelation, temporal structure, clustered observations), generates
appropriate cross-validation schemes (spatial blocking, checkerboard,
hexagonal, KNNDM, environmental blocking, leave-location-out, purged
CV), and validates evaluation pipelines for leakage. Includes area of
applicability (AOA) assessment following Meyer & Pebesma (2021)
[doi:10.1111/2041-210X.13650](https://doi.org/10.1111/2041-210X.13650) ,
forward feature selection with blocked CV, spatial thinning,
block-permutation variable importance, extrapolation detection, and
interactive visualizations. Integrates with 'tidymodels', 'caret',
'mlr3', 'ENMeval', and 'biomod2'. Based on evaluation principles
described in Roberts et al. (2017)
[doi:10.1111/ecog.02881](https://doi.org/10.1111/ecog.02881) , Kaufman
et al. (2012)
[doi:10.1145/2382577.2382579](https://doi.org/10.1145/2382577.2382579) ,
Kapoor & Narayanan (2023)
[doi:10.1016/j.patter.2023.100804](https://doi.org/10.1016/j.patter.2023.100804)
, and Linnenbrink et al. (2024)
[doi:10.5194/gmd-17-5897-2024](https://doi.org/10.5194/gmd-17-5897-2024)
.

BORG automatically detects and enforces valid model evaluation by
identifying information reuse between training and evaluation data. It
guards against:

- Data leakage through preprocessing (normalization, imputation, PCA)

- Look-ahead bias in temporal evaluation

- Spatial autocorrelation violations in block CV

- Target leakage through features derived from outcomes

- Train-test contamination through shared identifiers

## Value

No return value. This is a package-level documentation page.

## Main Functions

- [`borg`](https://gillescolling.com/BORG/reference/borg.md):

  Primary interface for guarding evaluation pipelines

- [`borg_diagnose`](https://gillescolling.com/BORG/reference/borg_diagnose.md):

  Diagnose data dependency structure

- [`borg_cv`](https://gillescolling.com/BORG/reference/borg_cv.md):

  Generate valid CV schemes based on diagnosis

- [`borg_inspect`](https://gillescolling.com/BORG/reference/borg_inspect.md):

  Inspect R objects for leakage signals

- [`borg_validate`](https://gillescolling.com/BORG/reference/borg_validate.md):

  Validate a complete evaluation workflow

- [`borg_assimilate`](https://gillescolling.com/BORG/reference/borg_assimilate.md):

  Assimilate leaky pipelines into compliance

## Risk Classification

BORG classifies evaluation risks as:

- hard_violation:

  Evaluation is fundamentally invalid. Must be blocked. Examples:
  preprocessing on full data, train-test ID overlap, target leakage.

- soft_inflation:

  Results are biased but bounded. Performance estimates are misleading
  but model ranking may be preserved. Examples: insufficient spatial
  block size, post-hoc subgroup analysis.

## Supported Frameworks

BORG integrates with:

- caret: `trainControl`, `train`, `preProcess`

- rsample: `vfold_cv`, `initial_split`, `rolling_origin`

- recipes: `recipe`, `prep`, `bake`

- mlr3: `Task`, `Learner`, `Resampling`

- Base R: manual index-based splitting

## Options

BORG respects the following options:

- `borg.auto_check`:

  If TRUE, automatically validate splits when using supported
  frameworks. Default: FALSE.

- `borg.strict`:

  If TRUE, throw errors on hard violations. If FALSE, return warnings.
  Default: TRUE.

- `borg.verbose`:

  If TRUE, print diagnostic messages. Default: FALSE.

## See also

Useful links:

- <https://github.com/gcol33/BORG>

- <https://gillescolling.com/BORG/>

- Report bugs at <https://github.com/gcol33/BORG/issues>

## Author

**Maintainer**: Gilles Colling <gilles.colling051@gmail.com>
([ORCID](https://orcid.org/0000-0003-3070-6066)) \[copyright holder\]

Authors:

- Gilles Colling <gilles.colling051@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-3070-6066)) \[copyright holder\]
