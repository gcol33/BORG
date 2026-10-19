# Explain Risks in Plain Language with Actionable Recommendations

Takes a
[`BorgRisk`](https://gillescolling.com/BORG/reference/BorgRisk.md) or
`borg_result` object and returns a prioritized, human-readable action
plan. Each risk is translated into what went wrong, why it matters, how
to fix it, and the expected inflation magnitude.

## Usage

``` r
borg_explain_risk(x, style = c("console", "markdown", "list"), verbose = FALSE)
```

## Arguments

- x:

  A [`BorgRisk`](https://gillescolling.com/BORG/reference/BorgRisk.md)
  or `borg_result` object.

- style:

  Character. Output style: `"console"` (default, prints to terminal),
  `"markdown"` (returns markdown string), or `"list"` (returns
  structured list).

- verbose:

  Logical. If `TRUE`, include background explanations. Default: `FALSE`.

## Value

Depending on `style`:

- "console":

  Invisible `x`, prints explanation.

- "markdown":

  Character string with markdown-formatted explanation.

- "list":

  A list of per-risk explanation objects, each with fields: `risk_type`,
  `severity`, `plain_english`, `why_it_matters`, `how_to_fix`,
  `inflation_est`.

## See also

[`borg`](https://gillescolling.com/BORG/reference/borg.md),
[`BorgRisk`](https://gillescolling.com/BORG/reference/BorgRisk.md),
[`borg_assimilate`](https://gillescolling.com/BORG/reference/borg_assimilate.md)

## Examples

``` r
# \donttest{
d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
result <- borg(d, coords = c("x", "y"), target = "z",
               train_idx = 1:70, test_idx = 71:100)
borg_explain_risk(result)
# }
```
