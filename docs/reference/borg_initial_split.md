# BORG-Guarded initial_split

A guarded version of
[`rsample::initial_split()`](https://rsample.tidymodels.org/reference/initial_split.html)
that checks for temporal ordering when time structure is specified.

## Usage

``` r
borg_initial_split(
  data,
  prop = 3/4,
  strata = NULL,
  time = NULL,
  coords = NULL,
  groups = NULL,
  target = NULL,
  ...
)
```

## Arguments

- data:

  A data frame.

- prop:

  Numeric. Proportion of data for training. Default: 0.75.

- strata:

  Character. Column name for stratification.

- time:

  Character. Time column - if provided, ensures chronological split.

- coords:

  Character vector. Coordinate columns for spatial check.

- groups:

  Character. Group column for clustered check.

- target:

  Character. Target variable.

- ...:

  Additional arguments passed to
  [`rsample::initial_split()`](https://rsample.tidymodels.org/reference/initial_split.html).

## Value

An `rsplit` object.

## Details

When `time` is specified, this function ensures the split respects
temporal ordering (training data comes before test data). For spatial
data, it warns if random splitting may cause issues.

## Examples

``` r
if (FALSE) { # \dontrun{
# Temporal data - ensures chronological split
ts_data <- data.frame(
  date = seq(as.Date("2020-01-01"), by = "day", length.out = 100),
  value = cumsum(rnorm(100))
)
split <- borg_initial_split(ts_data, prop = 0.8, time = "date")
} # }
```
