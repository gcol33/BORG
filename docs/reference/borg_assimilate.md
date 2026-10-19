# Assimilate Leaky Evaluation Pipelines

`borg_assimilate()` attempts to automatically fix detected evaluation
risks by restructuring the pipeline to eliminate information leakage.

## Usage

``` r
borg_assimilate(workflow, risks = NULL, fix = "all")
```

## Arguments

- workflow:

  A list containing the evaluation workflow (same structure as
  [`borg_validate`](https://gillescolling.com/BORG/reference/borg_validate.md)).

- risks:

  Optional
  [`BorgRisk`](https://gillescolling.com/BORG/reference/BorgRisk.md)
  object from a previous inspection. If NULL,
  [`borg_validate()`](https://gillescolling.com/BORG/reference/borg_validate.md)
  is called first.

- fix:

  Character vector specifying which risk types to attempt to fix.
  Default: `"all"` attempts all rewritable violations. Other options:
  `"preprocessing"`, `"feature_engineering"`, `"thresholds"`.

## Value

A list containing:

- workflow:

  The rewritten workflow (modified in place where possible)

- fixed:

  Character vector of risk types that were successfully fixed

- unfixable:

  Character vector of risk types that could not be fixed

- report:

  `BorgRisk` object from post-rewrite validation

## Details

`borg_assimilate()` can automatically fix certain types of leakage:

- Preprocessing on full data:

  Refits preprocessing objects using only training indices

- Feature engineering leaks:

  Recomputes target encodings, embeddings, and derived features using
  train-only data

- Threshold optimization:

  Moves threshold selection to training/validation data

Some violations cannot be automatically fixed:

- Train-test index overlap (requires new split)

- Target leakage in original features (requires domain intervention)

- Temporal look-ahead in features (requires feature re-engineering)

## See also

[`borg_validate`](https://gillescolling.com/BORG/reference/borg_validate.md)
for validation without assimilation,
[`borg`](https://gillescolling.com/BORG/reference/borg.md) for proactive
enforcement.

## Examples

``` r
# \donttest{
# Attempt to fix a leaky workflow
workflow <- list(
  data = data.frame(x = rnorm(100), y = rnorm(100)),
  train_idx = 1:70,
  test_idx = 71:100
)
result <- borg_assimilate(workflow)

if (length(result$unfixable) > 0) {
  message("Some risks require manual intervention:")
  print(result$unfixable)
}
# }
```
