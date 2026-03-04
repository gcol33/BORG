# Plot CV Comparison Results

Creates a visualization comparing random vs blocked CV performance.

## Usage

``` r
# S3 method for class 'borg_comparison'
plot(x, type = c("boxplot", "density", "paired"), ...)
```

## Arguments

- x:

  A `borg_comparison` object from `borg_compare_cv`.

- type:

  Character. Plot type: `"boxplot"` (default), `"density"`, or
  `"paired"`.

- ...:

  Additional arguments passed to plotting functions.
