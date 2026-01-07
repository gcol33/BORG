# Plot Temporal Validation

Visualizes temporal train/test split with gap analysis.

## Usage

``` r
plot_temporal(temporal, train_idx, test_idx, title = "Temporal Split")
```

## Arguments

- temporal:

  Numeric or Date vector of timestamps.

- train_idx:

  Integer vector of training indices.

- test_idx:

  Integer vector of test indices.

- title:

  Plot title.

## Value

A base R plot (invisibly returns NULL).

## Examples

``` r
dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 100)
train_idx <- 1:70
test_idx <- 71:100
plot_temporal(dates, train_idx, test_idx)
```
