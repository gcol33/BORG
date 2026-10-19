# mlr3 Resampling for BORG Cross-Validation

Creates a native `mlr3::Resampling` object from BORG cross-validation
folds, enabling direct use with mlr3 benchmarking infrastructure.

## Usage

``` r
borg_to_mlr3(data = NULL, folds = NULL, cv_obj = NULL, id = "borg")
```

## Arguments

- data:

  A data frame. Required when using `folds` directly, not needed when
  using `cv_obj`.

- folds:

  A list of lists, each with `train` and `test` integer index vectors.

- cv_obj:

  A `borg_cv` object. If provided, `folds` are extracted from it.

- id:

  Character. Resampling identifier. Default: `"borg"`.

## Value

An `mlr3::Resampling` R6 object that can be used directly with
`mlr3::resample()`, `mlr3::benchmark()`, and other mlr3 infrastructure.

## Details

The returned resampling is pre-instantiated — it contains fixed
train/test splits that respect BORG's spatial/temporal blocking. Calling
`$instantiate()` is not needed (and will not overwrite the existing
splits).

## See also

[`borg_cv`](https://gillescolling.com/BORG/reference/borg_cv.md),
[`borg_rset`](https://gillescolling.com/BORG/reference/borg_rset.md)

## Examples

``` r
# \donttest{
if (requireNamespace("mlr3", quietly = TRUE)) {
  d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
  cv <- borg_cv(d, coords = c("x", "y"), target = "z")
  resampling <- borg_to_mlr3(cv_obj = cv)
  resampling$iters  # number of folds
}
# }
```
