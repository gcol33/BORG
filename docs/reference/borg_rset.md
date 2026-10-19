# Convert BORG Folds to an rsample rset Object

Creates a proper `rset` object from BORG cross-validation folds,
enabling direct use with
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html),
[`tune::fit_resamples()`](https://tune.tidymodels.org/reference/fit_resamples.html),
and other tidymodels infrastructure without manual conversion.

## Usage

``` r
borg_rset(data = NULL, folds = NULL, cv_obj = NULL)
```

## Arguments

- data:

  A data frame. Required in all cases because `borg_cv` objects do not
  store the original data.

- folds:

  A list of lists, each with `train` and `test` integer index vectors.
  Typically from
  [`borg_cv`](https://gillescolling.com/BORG/reference/borg_cv.md)`()$folds`.
  Not needed when `cv_obj` is provided.

- cv_obj:

  A `borg_cv` object. If provided, `folds` are extracted from it.

## Value

An `rset` object (inheriting from `tbl_df`) compatible with the
tidymodels ecosystem. Each row has an `rsplit` column containing
train/test index information and an `id` column.

## Details

The returned object works directly with
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html),
[`tune::fit_resamples()`](https://tune.tidymodels.org/reference/fit_resamples.html),
[`rsample::assessment()`](https://rsample.tidymodels.org/reference/as.data.frame.rsplit.html),
and
[`rsample::analysis()`](https://rsample.tidymodels.org/reference/as.data.frame.rsplit.html).
The folds preserve BORG's spatial/temporal blocking structure.

## See also

[`borg_cv`](https://gillescolling.com/BORG/reference/borg_cv.md),
[`borg_trainControl`](https://gillescolling.com/BORG/reference/borg_trainControl.md)

## Examples

``` r
# \donttest{
set.seed(42)
d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
cv <- borg_cv(d, coords = c("x", "y"), target = "z")
rset <- borg_rset(data = d, cv_obj = cv)
class(rset)  # "borg_rset" "rset" "tbl_df" ...
# }
```
