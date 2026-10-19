# Autoplot Method for BorgRisk Objects

Creates a ggplot2 visualization of detected risks from a BORG
validation.

## Usage

``` r
# S3 method for class 'BorgRisk'
autoplot(object, max_risks = 10, show_fixes = TRUE, ...)
```

## Arguments

- object:

  A [`BorgRisk`](https://gillescolling.com/BORG/reference/BorgRisk.md)
  object from
  [`borg_inspect()`](https://gillescolling.com/BORG/reference/borg_inspect.md)
  or [`borg()`](https://gillescolling.com/BORG/reference/borg.md) in
  validation mode.

- max_risks:

  Integer. Maximum number of risks to display. Default: 10.

- show_fixes:

  Logical. If TRUE (default), annotate each risk with a suggested fix
  from
  [`borg_assimilate()`](https://gillescolling.com/BORG/reference/borg_assimilate.md)
  or manual action. If FALSE, show the risk description instead.

- ...:

  Additional arguments (currently unused).

## Value

A `ggplot` object.

## Details

Requires the ggplot2 package.

## Examples

``` r
# \donttest{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  data <- data.frame(x = 1:100, y = 101:200)
  result <- borg_inspect(data, train_idx = 1:60, test_idx = 51:100)
  ggplot2::autoplot(result)
}
# }
```
