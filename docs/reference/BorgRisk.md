# BorgRisk S4 Class

Holds the result of
[`borg_inspect`](https://gillescolling.com/BORG/reference/borg_inspect.md)
or
[`borg_validate`](https://gillescolling.com/BORG/reference/borg_validate.md):
a structured assessment of evaluation risks detected in a workflow or
object.

This class stores identified risks, their classification (hard violation
vs soft inflation), affected data indices, and recommended remediation
actions.

## Usage

``` r
# S4 method for class 'BorgRisk'
show(object)
```

## Arguments

- object:

  A `BorgRisk` object to be printed.

## Slots

- `risks`:

  A list of detected risk objects, each containing:

  type

  :   Character string: risk category (e.g., "preprocessing_leak")

  severity

  :   Character string: "hard_violation" or "soft_inflation"

  description

  :   Character string: human-readable description

  affected_indices

  :   Integer vector: row/column indices affected

  source_object

  :   Character string: name of the leaky object

- `n_hard`:

  Integer. Count of hard violations detected.

- `n_soft`:

  Integer. Count of soft inflation risks detected.

- `is_valid`:

  Logical. TRUE if no hard violations detected.

- `train_indices`:

  Integer vector. Row indices in training set.

- `test_indices`:

  Integer vector. Row indices in test set.

- `timestamp`:

  POSIXct. When the inspection was performed.

- `call`:

  Language object. The original call that triggered inspection.

## See also

[`borg_inspect`](https://gillescolling.com/BORG/reference/borg_inspect.md),
[`borg_validate`](https://gillescolling.com/BORG/reference/borg_validate.md),
[`borg`](https://gillescolling.com/BORG/reference/borg.md)

## Examples

``` r
# Create an empty BorgRisk object (no risks detected)
show(new("BorgRisk",
  risks = list(),
  n_hard = 0L,
  n_soft = 0L,
  is_valid = TRUE,
  train_indices = 1:80,
  test_indices = 81:100,
  timestamp = Sys.time(),
  call = quote(borg_inspect(x))
))
```
