# Validate Complete Evaluation Workflow

`borg_validate()` performs post-hoc validation of an entire evaluation
workflow, checking all components for information leakage.

## Usage

``` r
borg_validate(workflow, strict = FALSE)
```

## Arguments

- workflow:

  A list containing the evaluation workflow components:

  data

  :   The full dataset

  train_idx

  :   Integer vector of training indices

  test_idx

  :   Integer vector of test indices

  preprocess

  :   Optional preprocessing object(s)

  model

  :   The fitted model object

  predictions

  :   Model predictions on test data

  metrics

  :   Computed evaluation metrics

- strict:

  Logical. If TRUE, any hard violation causes an error. Default: FALSE
  (returns report only).

## Value

A [`BorgRisk`](https://gillescolling.com/BORG/reference/BorgRisk.md)
object containing a comprehensive assessment of the workflow.

## Details

`borg_validate()` inspects each component of an evaluation workflow:

1.  **Split validation**: Checks train/test index isolation

2.  **Preprocessing audit**: Traces preprocessing parameters to verify
    train-only origin

3.  **Feature audit**: Checks for target leakage and proxy features

4.  **Model audit**: Validates that model used only training data

5.  **Threshold audit**: Checks if any thresholds were optimized on test
    data

## See also

[`borg_guard`](https://gillescolling.com/BORG/reference/borg_guard.md)
for proactive enforcement,
[`borg_inspect`](https://gillescolling.com/BORG/reference/borg_inspect.md)
for single-object inspection.

## Examples

``` r
if (FALSE) { # \dontrun{
# Validate an existing workflow
result <- borg_validate(list(
  data = my_data,
  train_idx = train_idx,
  test_idx = test_idx,
  preprocess = my_recipe,
  model = my_model,
  predictions = preds,
  metrics = list(rmse = 0.5, mae = 0.3)
))

# Check validity
if (!result@is_valid) {
  print(result)  # Shows detailed risk report
}
} # }
```
