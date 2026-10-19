# Willmott's Index of Agreement

Computes Willmott's d (original, refined d1, and modified dr) for
spatial model assessment.

## Usage

``` r
borg_willmott(actual, predicted)
```

## Arguments

- actual:

  Numeric vector of observed values.

- predicted:

  Numeric vector of predicted values.

## Value

A list with `d` (original), `d1` (refined), and `dr` (modified).

## References

Willmott, C. J. (1981). On the validation of models. *Physical
Geography*, 2(2), 184-194.
[doi:10.1080/02723646.1981.10642213](https://doi.org/10.1080/02723646.1981.10642213)
