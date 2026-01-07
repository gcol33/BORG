# Plot Risk Assessment Summary

Visualizes the risks detected by BORG validation.

## Usage

``` r
plot_risk(risk, title = "BORG Risk Assessment", max_risks = 10)
```

## Arguments

- risk:

  A BorgRisk object from borg_inspect, borg_validate, or borg_guard.

- title:

  Plot title.

- max_risks:

  Maximum number of risks to display.

## Value

A base R plot (invisibly returns NULL).

## Examples

``` r
data <- data.frame(x = 1:100, y = 101:200)
result <- borg_inspect(data, train_idx = 1:60, test_idx = 51:100)
plot_risk(result)
```
