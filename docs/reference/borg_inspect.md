# Inspect R Objects for Evaluation Risks

`borg_inspect()` examines R objects for signals of information reuse
that would invalidate model evaluation. It returns a structured
assessment of detected risks.

## Usage

``` r
borg_inspect(
  object,
  train_idx = NULL,
  test_idx = NULL,
  data = NULL,
  target_col = NULL,
  spatial_cols = NULL,
  ...
)
```

## Arguments

- object:

  An R object to inspect. Supported types include:

  - Preprocessing: `preProcess`, `recipe`, `prcomp`

  - CV objects: `trainControl`, `rsplit`, `vfold_cv`

  - Model objects: `train`, `lm`, `glm`

  - Data frames with train/test split information

- train_idx:

  Integer vector of training row indices. Required for data-level
  inspection.

- test_idx:

  Integer vector of test row indices. Required for data-level
  inspection.

- data:

  Optional data frame. Required when inspecting preprocessing objects to
  compare parameters against train-only statistics.

- target_col:

  Optional name of the target/outcome column. If provided, checks for
  target leakage (features highly correlated with target).

- spatial_cols:

  Optional character vector of coordinate column names. If provided,
  checks spatial separation between train and test.

- ...:

  Additional arguments passed to type-specific inspectors.

## Value

A [`BorgRisk`](https://gillescolling.com/BORG/reference/BorgRisk.md)
object containing:

- risks:

  List of detected risk objects

- n_hard:

  Count of hard violations

- n_soft:

  Count of soft inflation warnings

- is_valid:

  TRUE if no hard violations detected

## Details

`borg_inspect()` dispatches to type-specific inspectors based on the
class of the input object. Each inspector looks for specific leakage
patterns:

- Preprocessing objects:

  Checks if parameters (mean, sd, loadings) were computed on data that
  includes test indices

- CV objects:

  Validates that train/test indices do not overlap and that grouping
  structure is respected

- Feature engineering:

  Checks if encodings, embeddings, or derived features used test data
  during computation

## See also

[`borg_validate`](https://gillescolling.com/BORG/reference/borg_validate.md)
for complete workflow validation, `borg_guard` for automated enforcement
during evaluation.

## Examples

``` r
# Inspect a preprocessing object
data(mtcars)
train_idx <- 1:25
test_idx <- 26:32

# BAD: preProcess fitted on full data (will detect leak)
pp_bad <- scale(mtcars[, -1])

# GOOD: preProcess fitted on train only
pp_good <- scale(mtcars[train_idx, -1])
```
