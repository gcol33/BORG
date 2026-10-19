# Null Model Significance Test

Tests whether model performance is significantly better than random
expectation by permuting the response variable and re-evaluating with
the same CV scheme. Computes z-scores and p-values.

## Usage

``` r
borg_null_test(
  data,
  folds,
  formula,
  metric = c("rmse", "mae", "rsq"),
  fit_fun = stats::lm,
  n_null = 99L,
  seed = 42L,
  verbose = FALSE
)
```

## Arguments

- data:

  Data frame.

- folds:

  A `borg_cv` object or fold list.

- formula:

  Model formula.

- metric:

  Character. `"rmse"` (default), `"mae"`, or `"rsq"`.

- fit_fun:

  Function. Default: `lm`.

- n_null:

  Integer. Number of null permutations. Default: 99.

- seed:

  Integer. Random seed. Default: 42.

- verbose:

  Logical. Default: FALSE.

## Value

A list with class `"borg_null_test"` containing:

- empirical:

  Empirical CV metric value

- null_distribution:

  Numeric vector of null metric values

- z_score:

  Z-score of empirical vs null

- p_value:

  One-sided p-value

- significant:

  Logical. Whether model is significantly better

Has an `autoplot()` method showing null distribution with empirical
value.

## References

Kass, J. M., Muscarella, R., Galante, P. J., Bohl, C. L.,
Pinilla-Buitrago, G. E., Boria, R. A., Soley-Guardia, M., & Anderson, R.
P. (2021). ENMeval 2.0: Redesigned for customizable and reproducible
modeling of species' niches and distributions. *Methods in Ecology and
Evolution*, 12(9), 1602-1608.
[doi:10.1111/2041-210X.13628](https://doi.org/10.1111/2041-210X.13628)

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
d$z <- d$x * 3 + rnorm(100)
cv <- borg_cv(d, coords = c("x", "y"), target = "z")
nt <- borg_null_test(d, cv, z ~ x + y, n_null = 19)
nt
```
