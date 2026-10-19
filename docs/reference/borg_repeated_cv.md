# Repeated Blocked Cross-Validation

Repeats [`borg_cv`](https://gillescolling.com/BORG/reference/borg_cv.md)
multiple times with different random seeds and aggregates the results.
Repeated CV provides better variance estimates and more stable
performance metrics than a single CV run, particularly important for
spatial/temporal blocking where fold assignment depends on the random
seed.

## Usage

``` r
borg_repeated_cv(
  data,
  repeats = 10L,
  v = 5L,
  seeds = NULL,
  aggregate = TRUE,
  ...
)
```

## Arguments

- data:

  A data frame.

- repeats:

  Integer. Number of repetitions. Default: 10.

- v:

  Integer. Number of folds per repetition. Default: 5.

- seeds:

  Integer vector of length `repeats`, or NULL (auto-generated). Controls
  fold assignment for each repetition.

- aggregate:

  Logical. If TRUE (default), returns aggregated metrics. If FALSE,
  returns all individual fold results.

- ...:

  Additional arguments passed to
  [`borg_cv`](https://gillescolling.com/BORG/reference/borg_cv.md).

## Value

A list with class `"borg_repeated_cv"` containing:

- folds:

  List of length `repeats`, each element a list of `v` train/test fold
  pairs.

- repeats:

  Number of repetitions.

- v:

  Number of folds.

- strategy:

  CV strategy used.

- seeds:

  Seeds used for each repetition.

- diagnosis:

  The `BorgDiagnosis` from the first run.

Has [`print()`](https://rdrr.io/r/base/print.html) and `autoplot()`
methods.

## Details

Each repetition calls
[`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md) with
a different seed, producing different fold assignments. The diagnosis is
computed once and reused across repetitions for consistency.

For aggregation with a model, use
[`borg_fold_performance()`](https://gillescolling.com/BORG/reference/borg_fold_performance.md)
on each repetition's folds and combine the results.

## See also

[`borg_cv`](https://gillescolling.com/BORG/reference/borg_cv.md),
[`borg_fold_performance`](https://gillescolling.com/BORG/reference/borg_fold_performance.md)

## Examples

``` r
set.seed(42)
d <- data.frame(
  x = runif(200), y = runif(200), z = rnorm(200)
)
rcv <- borg_repeated_cv(d, repeats = 3, v = 5,
                        coords = c("x", "y"), target = "z")
rcv
length(rcv$folds)  # 3 repetitions
length(rcv$folds[[1]])  # 5 folds each
```
