# Summarize BORG Diagnosis

Generate a methods section summary for publication from a BorgDiagnosis
object.

## Usage

``` r
# S3 method for class 'BorgDiagnosis'
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

  A `BorgDiagnosis` object.

- comparison:

  Optional. A `borg_comparison` object from
  [`borg_compare_cv()`](https://gillescolling.com/BORG/reference/borg_compare_cv.md)
  to include empirical inflation estimates.

- v:

  Integer. Number of CV folds used. Default: 5.

- style:

  Character. Citation style: `"apa"` (default), `"nature"`, `"ecology"`.

- include_citation:

  Logical. Include BORG package citation. Default: TRUE.

- ...:

  Additional arguments (currently unused).

## Value

Character string with methods section text (invisibly). Also prints the
text to the console.

## Examples

``` r
set.seed(42)
data <- data.frame(
  x = runif(100, 0, 100),
  y = runif(100, 0, 100),
  response = rnorm(100)
)
diagnosis <- borg_diagnose(data, coords = c("x", "y"), target = "response",
                           verbose = FALSE)
summary(diagnosis)
```
