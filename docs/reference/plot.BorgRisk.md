# Plot BORG Objects

S3 plot method for BORG risk assessment objects.

## Usage

``` r
# S3 method for class 'BorgRisk'
plot(x, title = NULL, max_risks = 10, ...)
```

## Arguments

- x:

  A `BorgRisk` object from
  [`borg_inspect()`](https://gillescolling.com/BORG/reference/borg_inspect.md)
  or [`borg()`](https://gillescolling.com/BORG/reference/borg.md).

- title:

  Optional custom plot title.

- max_risks:

  Maximum number of risks to display. Default: 10.

- ...:

  Additional arguments (currently unused).

## Value

Invisibly returns NULL. Called for plotting side effect.

## Details

Displays a visual summary of detected risks:

- Hard violations shown in red

- Soft inflation risks shown in yellow/orange

- Green "OK" when no risks detected

## Examples

``` r
# No risks
data <- data.frame(x = 1:100, y = 101:200)
result <- borg_inspect(data, train_idx = 1:70, test_idx = 71:100)
plot(result)

# With overlap violation
result_bad <- borg_inspect(data, train_idx = 1:60, test_idx = 51:100)
plot(result_bad)
```
