# BORG-Guarded group_vfold_cv

A guarded version of
[`rsample::group_vfold_cv()`](https://rsample.tidymodels.org/reference/group_vfold_cv.html)
that validates group-based CV is appropriate for the data structure.

## Usage

``` r
borg_group_vfold_cv(
  data,
  group,
  v = NULL,
  balance = c("groups", "observations"),
  coords = NULL,
  time = NULL,
  target = NULL,
  ...
)
```

## Arguments

- data:

  A data frame.

- group:

  Character. Column name for grouping.

- v:

  Integer. Number of folds. Default: number of groups.

- balance:

  Character. How to balance folds: "groups" or "observations".

- coords:

  Character vector. Coordinate columns for spatial check.

- time:

  Character. Time column for temporal check.

- target:

  Character. Target variable for dependency detection.

- ...:

  Additional arguments passed to
  [`rsample::group_vfold_cv()`](https://rsample.tidymodels.org/reference/group_vfold_cv.html).

## Value

An `rset` object from rsample.

## Examples

``` r
if (FALSE) { # \dontrun{
# Clustered data - group CV is appropriate
data <- data.frame(
  site = rep(1:20, each = 5),
  x = rnorm(100),
  y = rnorm(100)
)
folds <- borg_group_vfold_cv(data, group = "site", v = 5)
} # }
```
