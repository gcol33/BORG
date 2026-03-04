# Summarize BORG Result

Generate a methods section summary for publication from a borg_result
object.

## Usage

``` r
# S3 method for class 'borg_result'
summary(
  object,
  comparison = NULL,
  v = 5,
  style = c("apa", "nature", "ecology"),
  include_citation = TRUE,
  ...
)
```

## Arguments

- object:

  A `borg_result` object from
  [`borg()`](https://gillescolling.com/BORG/reference/borg.md).

- comparison:

  Optional. A `borg_comparison` object.

- v:

  Integer. Number of CV folds. Default: 5.

- style:

  Character. Citation style.

- include_citation:

  Logical. Include BORG citation.

- ...:

  Additional arguments (currently unused).

## Value

Character string with methods text (invisibly).

## Examples

``` r
set.seed(42)
data <- data.frame(
  x = runif(100, 0, 100),
  y = runif(100, 0, 100),
  response = rnorm(100)
)
result <- borg(data, coords = c("x", "y"), target = "response")
summary(result)
```
