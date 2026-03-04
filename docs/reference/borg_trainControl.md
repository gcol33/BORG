# BORG-Guarded trainControl

A guarded version of
[`caret::trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html)
that validates CV settings against data dependencies.

## Usage

``` r
borg_trainControl(
  data,
  method = "cv",
  number = 10,
  coords = NULL,
  time = NULL,
  groups = NULL,
  target = NULL,
  allow_override = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame. Required for dependency checking.

- method:

  Character. Resampling method.

- number:

  Integer. Number of folds or iterations.

- coords:

  Character vector. Coordinate columns for spatial check.

- time:

  Character. Time column for temporal check.

- groups:

  Character. Group column for clustered check.

- target:

  Character. Target variable.

- allow_override:

  Logical. Allow random CV despite dependencies.

- ...:

  Additional arguments passed to
  [`caret::trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html).

## Value

A `trainControl` object, potentially modified for blocked CV.

## Examples

``` r
if (FALSE) { # \dontrun{
library(caret)

# This will warn/error if dependencies detected
ctrl <- borg_trainControl(
  data = spatial_data,
  method = "cv",
  number = 5,
  coords = c("lon", "lat")
)
} # }
```
