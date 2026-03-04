# Plot BORG Result Objects

S3 plot method for borg_result objects from
[`borg()`](https://gillescolling.com/BORG/reference/borg.md).

## Usage

``` r
# S3 method for class 'borg_result'
plot(
  x,
  type = c("split", "risk", "temporal", "groups"),
  fold = 1,
  time = NULL,
  groups = NULL,
  title = NULL,
  ...
)
```

## Arguments

- x:

  A `borg_result` object from
  [`borg()`](https://gillescolling.com/BORG/reference/borg.md).

- type:

  Character. Plot type: `"split"` (default), `"risk"`, `"temporal"`, or
  `"groups"`.

- fold:

  Integer. Which fold to plot (for split visualization). Default: 1.

- time:

  Column name or values for temporal plots.

- groups:

  Column name or values for group plots.

- title:

  Optional custom plot title.

- ...:

  Additional arguments passed to internal plot functions.

## Value

Invisibly returns NULL. Called for plotting side effect.

## Examples

``` r
set.seed(42)
data <- data.frame(
  x = runif(100, 0, 100),
  y = runif(100, 0, 100),
  response = rnorm(100)
)
result <- borg(data, coords = c("x", "y"), target = "response")
plot(result)  # Split visualization for first fold
```
