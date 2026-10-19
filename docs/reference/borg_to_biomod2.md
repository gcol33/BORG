# Convert BORG Folds to biomod2 Format

Converts BORG CV folds to the data split table format expected by
biomod2's `BIOMOD_Modeling()` function.

## Usage

``` r
borg_to_biomod2(borg_cv)
```

## Arguments

- borg_cv:

  A `borg_cv` object.

## Value

A matrix where each column is a CV run and each row is an observation.
Values are `TRUE` (calibration) or `FALSE` (validation), matching
biomod2's `DataSplitTable` format.

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
cv <- borg_cv(d, coords = c("x", "y"), target = "z")
split_table <- borg_to_biomod2(cv)
dim(split_table)
```
