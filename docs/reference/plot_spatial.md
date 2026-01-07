# Plot Spatial Split

Visualizes spatial train/test distribution.

## Usage

``` r
plot_spatial(x, y, train_idx, test_idx, title = "Spatial Split")
```

## Arguments

- x:

  Numeric vector of x-coordinates (e.g., longitude).

- y:

  Numeric vector of y-coordinates (e.g., latitude).

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
set.seed(42)
x <- runif(100, -10, 10)
y <- runif(100, -10, 10)
train_idx <- which(x < 0)
test_idx <- which(x >= 0)
plot_spatial(x, y, train_idx, test_idx)
```
