# Distance Distribution Diagnostics

Computes nearest-neighbor distance distributions in geographic space
and/or feature space between training data, CV test sets, and optional
prediction locations. Diagnostic for whether CV folds are representative
of the prediction task.

## Usage

``` r
borg_geodist(
  data,
  folds,
  prediction_points = NULL,
  coords = NULL,
  predictors = NULL,
  type = c("both", "geo", "feature")
)
```

## Arguments

- data:

  Data frame of training/modelling data.

- folds:

  A `borg_cv` object or fold list.

- prediction_points:

  Optional data frame of prediction locations.

- coords:

  Character vector of length 2. Coordinate columns for geographic
  distance. If `NULL`, only feature-space distances are computed.

- predictors:

  Character vector. Feature columns for feature-space distance. If
  `NULL`, uses all shared numeric columns excluding coords.

- type:

  Character. `"geo"` (geographic only), `"feature"` (feature space
  only), or `"both"` (default).

## Value

A list with class `"borg_geodist"` containing:

- cv_distances:

  Data frame of NN distances: test-to-train per fold

- prediction_distances:

  NN distances: prediction-to-train (if provided)

- sample_distances:

  NN distances: within training data (reference)

- ks_statistic:

  KS test statistic comparing CV vs prediction distances

Has an `autoplot()` method showing overlaid density curves.

## Details

If the CV test-to-train distance distribution differs strongly from the
prediction-to-train distribution, the CV does not mimic the real
prediction scenario and performance estimates may be misleading.

## References

Meyer, H., & Pebesma, E. (2022). Machine learning-based global maps of
ecological variables and the challenge of assessing them. *Nature
Communications*, 13, 2208.
[doi:10.1038/s41467-022-29838-9](https://doi.org/10.1038/s41467-022-29838-9)

## Examples

``` r
set.seed(42)
d <- data.frame(x = runif(100, 0, 50), y = runif(100, 0, 50),
                 a = rnorm(100), z = rnorm(100))
cv <- borg_cv(d, coords = c("x", "y"), target = "z")
pred <- data.frame(x = runif(50, 0, 100), y = runif(50, 0, 100),
                    a = rnorm(50))
gd <- borg_geodist(d, cv, prediction_points = pred, coords = c("x", "y"))
gd
```
