# BORG: Bounded Outcome Risk Guard for Model Evaluation

Automatically detects and enforces valid model evaluation by identifying
information reuse between training and evaluation data. Guards against
data leakage, look-ahead bias, and invalid cross-validation schemes that
inflate performance estimates. Supports temporal, spatial, and grouped
evaluation structures.

BORG automatically detects and enforces valid model evaluation by
identifying information reuse between training and evaluation data. It
guards against:

- Data leakage through preprocessing (normalization, imputation, PCA)

- Look-ahead bias in temporal evaluation

- Spatial autocorrelation violations in block CV

- Target leakage through features derived from outcomes

- Train-test contamination through shared identifiers

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

- [`borg_rewrite`](https://gillescolling.com/BORG/reference/borg_rewrite.md):

  Automatically rewrite leaky pipelines

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

- Report bugs at <https://github.com/gcol33/BORG/issues>

## Author

**Maintainer**: Gilles Colling <gilles.colling051@gmail.com>
