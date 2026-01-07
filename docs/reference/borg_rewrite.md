# Automatically Rewrite Leaky Evaluation Pipelines

`borg_rewrite()` attempts to automatically fix detected evaluation risks
by restructuring the pipeline to avoid information leakage.

## Usage

``` r
borg_rewrite(workflow, risks = NULL, fix = "all")
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

`borg_rewrite()` can automatically fix certain types of leakage:

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
for validation without rewriting,
[`borg_guard`](https://gillescolling.com/BORG/reference/borg_guard.md)
for proactive enforcement.

## Examples

``` r
if (FALSE) { # \dontrun{
# Attempt to fix a leaky workflow
result <- borg_rewrite(my_workflow)

if (length(result$unfixable) > 0) {
  warning("Some risks could not be automatically fixed")
  print(result$unfixable)
}

# Use the fixed workflow
fixed_workflow <- result$workflow
} # }
```
