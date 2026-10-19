# Buffered Leave-One-Out Cross-Validation

For each observation, holds it out as the test set and removes all
training points within a buffer distance. This ensures spatial
independence between train and test in each iteration, following the
approach recommended by Roberts et al. (2017).

## Usage

``` r
borg_spatial_loo(
  data,
  coords,
  buffer = NULL,
  target = NULL,
  diagnosis = NULL,
  max_iter = NULL,
  seed = 42L,
  verbose = FALSE
)
```

## Arguments

- data:

  A data frame.

- coords:

  Character vector of length 2. Coordinate column names.

- buffer:

  Numeric. Exclusion buffer distance (in coordinate units). Training
  points within this distance of the test point are removed. If NULL,
  uses the estimated autocorrelation range from
  [`borg_diagnose()`](https://gillescolling.com/BORG/reference/borg_diagnose.md).

- target:

  Character. Response variable name (used for diagnosis if `buffer` is
  NULL).

- diagnosis:

  A `BorgDiagnosis` object. If NULL and `buffer` is NULL, runs
  [`borg_diagnose()`](https://gillescolling.com/BORG/reference/borg_diagnose.md)
  to estimate the autocorrelation range.

- max_iter:

  Integer or NULL. Maximum number of LOO iterations. If NULL, uses all
  `n` observations. For large datasets, set this to subsample. Default:
  NULL.

- seed:

  Integer. Random seed for subsampling. Default: 42.

- verbose:

  Logical. Print progress. Default: FALSE.

## Value

A list with class `"borg_spatial_loo"` containing:

- folds:

  List of length `n` (or `max_iter`), each with `train` and `test`
  integer index vectors.

- buffer:

  Buffer distance used.

- n_excluded:

  Integer vector: number of training points excluded per iteration.

- effective_train_size:

  Integer vector: training set size per iteration after exclusion.

## References

Roberts, D.R., et al. (2017). Cross-validation strategies for data with
temporal, spatial, hierarchical, or phylogenetic structure. *Ecography*,
40(8), 913-929.
[doi:10.1111/ecog.02881](https://doi.org/10.1111/ecog.02881)

## See also

[`borg_cv`](https://gillescolling.com/BORG/reference/borg_cv.md),
[`borg_thin`](https://gillescolling.com/BORG/reference/borg_thin.md)

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(50), y = runif(50), z = rnorm(50))
loo <- borg_spatial_loo(d, coords = c("x", "y"), buffer = 0.2)
length(loo$folds)  # 50 iterations
mean(loo$n_excluded)  # avg points excluded per iteration
```
