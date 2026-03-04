# BORG-Guarded vfold_cv

A guarded version of
[`rsample::vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html)
that checks for data dependencies before creating folds. If spatial,
temporal, or clustered dependencies are detected, random CV is blocked.

## Usage

``` r
borg_vfold_cv(
  data,
  v = 10,
  repeats = 1,
  strata = NULL,
  coords = NULL,
  time = NULL,
  groups = NULL,
  target = NULL,
  allow_override = FALSE,
  auto_block = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame.

- v:

  Integer. Number of folds. Default: 10.

- repeats:

  Integer. Number of repeats. Default: 1.

- strata:

  Character. Column name for stratification.

- coords:

  Character vector of length 2. Coordinate columns for spatial check.

- time:

  Character. Time column for temporal check.

- groups:

  Character. Group column for clustered check.

- target:

  Character. Target variable for dependency detection.

- allow_override:

  Logical. If TRUE, allow random CV with explicit confirmation. Default:
  FALSE.

- auto_block:

  Logical. If TRUE, automatically switch to blocked CV when dependencies
  detected. If FALSE, throw error. Default: FALSE.

- ...:

  Additional arguments passed to
  [`rsample::vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html).

## Value

If no dependencies detected or `allow_override = TRUE`, returns an
`rset` object from rsample. If dependencies detected and
`auto_block = TRUE`, returns BORG-generated blocked CV folds.

## See also

[`borg_cv`](https://gillescolling.com/BORG/reference/borg_cv.md) for
direct blocked CV generation.

## Examples

``` r
if (FALSE) { # \dontrun{
library(rsample)

# Safe: no dependencies
data <- data.frame(x = rnorm(100), y = rnorm(100))
folds <- borg_vfold_cv(data, v = 5)

# Blocked: spatial dependencies detected
spatial_data <- data.frame(
  lon = runif(100, -10, 10),
  lat = runif(100, -10, 10),
  response = rnorm(100)
)
# This will error:
# borg_vfold_cv(spatial_data, coords = c("lon", "lat"))

# Use auto_block to automatically switch to spatial CV:
folds <- borg_vfold_cv(spatial_data, coords = c("lon", "lat"),
                       target = "response", auto_block = TRUE)
} # }
```
