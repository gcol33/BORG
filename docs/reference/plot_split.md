# Plot Train/Test Split Distribution

Visualizes the distribution of training and test indices, optionally
highlighting temporal or group structure.

## Usage

``` r
plot_split(
  train_idx,
  test_idx,
  n_total = NULL,
  temporal = NULL,
  groups = NULL,
  title = "Train/Test Split"
)
```

## Arguments

- train_idx:

  Integer vector of training indices.

- test_idx:

  Integer vector of test indices.

- n_total:

  Total number of observations. If NULL, inferred from indices.

- temporal:

  Optional numeric/Date vector for temporal ordering.

- groups:

  Optional vector of group assignments.

- title:

  Plot title.

## Value

A base R plot (invisibly returns NULL).

## Examples

``` r
train_idx <- 1:70
test_idx <- 71:100
plot_split(train_idx, test_idx)

# With temporal structure
dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 100
)
plot_split(train_idx, test_idx, temporal = dates)
```
