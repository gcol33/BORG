# Export Validation Certificate

Write a BORG validation certificate to a YAML or JSON file for
machine-readable documentation.

## Usage

``` r
borg_export(diagnosis, data, file, comparison = NULL, cv = NULL)
```

## Arguments

- diagnosis:

  A `BorgDiagnosis` object.

- data:

  The data frame that was analyzed.

- file:

  Character. Output file path. Extension determines format (.yaml/.yml
  for YAML, .json for JSON).

- comparison:

  Optional. A `borg_comparison` object.

- cv:

  Optional. A `borg_cv` object.

## Value

Invisibly returns the certificate object.

## See also

[`borg_certificate`](https://gillescolling.com/BORG/reference/borg_certificate.md)
for creating certificates.

## Examples

``` r
if (FALSE) { # \dontrun{
diagnosis <- borg_diagnose(data, coords = c("x", "y"), target = "response")
borg_export(diagnosis, data, "validation.yaml")
borg_export(diagnosis, data, "validation.json")
} # }
```
