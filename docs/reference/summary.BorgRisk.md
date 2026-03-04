# Summarize BORG Risk Assessment

Print a summary of detected risks.

## Usage

``` r
# S3 method for class 'BorgRisk'
summary(object, ...)
```

## Arguments

- object:

  A `BorgRisk` object from
  [`borg_inspect()`](https://gillescolling.com/BORG/reference/borg_inspect.md).

- ...:

  Additional arguments (currently unused).

## Value

The object invisibly.

## Examples

``` r
data <- data.frame(x = 1:100, y = 101:200)
risk <- borg_inspect(data, train_idx = 1:60, test_idx = 51:100)
summary(risk)
```
