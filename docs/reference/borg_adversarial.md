# Adversarial Validation

Trains a binary classifier to distinguish training data from prediction
locations. High classification accuracy (AUC \> 0.7) indicates the
prediction domain differs substantially from training, and
spatial/blocked CV is essential. Low accuracy (AUC ~ 0.5) means random
CV may suffice.

## Usage

``` r
borg_adversarial(train, prediction, predictors = NULL, v = 5L)
```

## Arguments

- train:

  Data frame of training data.

- prediction:

  Data frame of prediction locations (same predictor columns).

- predictors:

  Character vector. If `NULL`, all shared numeric columns.

- v:

  Integer. Number of CV folds for adversarial classifier. Default: 5.

## Value

A list with class `"borg_adversarial"` containing:

- auc:

  Cross-validated AUC of the adversarial classifier

- dissimilarity:

  Dissimilarity score (0 to 100 percent)

- recommendation:

  Suggested CV strategy based on dissimilarity

- importance:

  Variable importance for distinguishing domains

## Details

Uses logistic regression as the adversarial classifier (no external
dependencies). The AUC is computed via cross-validation on the combined
train+prediction dataset with a binary label (0=train, 1=prediction).

Interpretation:

- AUC \< 0.6: Low dissimilarity. Random CV likely adequate.

- AUC 0.6-0.8: Moderate dissimilarity. Spatial CV recommended.

- AUC \> 0.8: High dissimilarity. Spatial CV essential; check AOA.

## Examples

``` r
set.seed(42)
train <- data.frame(a = rnorm(100), b = rnorm(100))
pred <- data.frame(a = rnorm(50, mean = 2), b = rnorm(50))
av <- borg_adversarial(train, pred)
av
```
