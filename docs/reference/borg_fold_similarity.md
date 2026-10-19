# Per-Fold Environmental Similarity (MESS)

Computes the Multivariate Environmental Similarity Surface (MESS) for
each CV fold, showing how representative each fold's test set is
relative to the training set in environmental space.

## Usage

``` r
borg_fold_similarity(data, folds, predictors = NULL)
```

## Arguments

- data:

  Data frame.

- folds:

  A `borg_cv` object or fold list.

- predictors:

  Character vector. If `NULL`, all numeric columns used.

## Value

A data frame with per-fold similarity metrics.

## References

Elith, J., Kearney, M., & Phillips, S. (2010). The art of modelling
range-shifting species. *Methods in Ecology and Evolution*, 1(4),
330-342.
[doi:10.1111/j.2041-210X.2010.00036.x](https://doi.org/10.1111/j.2041-210X.2010.00036.x)
