# Autoplot Method for borg_fold_perf Objects

Autoplot Method for borg_fold_perf Objects

## Usage

``` r
# S3 method for class 'borg_fold_perf'
autoplot(object, ...)
```

## Arguments

- object:

  A `borg_fold_perf` object.

- ...:

  Additional arguments (currently unused).

## Value

A `ggplot` object. If spatial centroids are available, shows a map of
per-fold performance. Otherwise, a bar chart.
