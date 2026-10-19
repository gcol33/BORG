# Convert BORG Folds to ENMeval Partition Format

Converts BORG CV folds to the partition format expected by ENMeval's
`ENMevaluate()` function (a vector of fold assignments).

## Usage

``` r
borg_to_enmeval(borg_cv)
```

## Arguments

- borg_cv:

  A `borg_cv` object.

## Value

A named list with:

- occs.grp:

  Integer vector of fold assignments for occurrence points

- bg.grp:

  Integer vector for background points (all assigned to fold 0)

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
cv <- borg_cv(d, coords = c("x", "y"), target = "z")
parts <- borg_to_enmeval(cv)
table(parts$occs.grp)
```
