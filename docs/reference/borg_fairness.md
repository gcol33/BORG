# Detect Performance Disparities Across Subgroups Under Blocked CV

Compares model performance across spatial, temporal, or categorical
subgroups under both random and blocked CV. Leakage can mask disparate
performance — a model that looks uniformly good under random CV may show
large subgroup-level gaps when evaluated honestly.

## Usage

``` r
borg_fairness(
  data,
  model,
  target,
  group = NULL,
  coords = NULL,
  n_groups = 4L,
  folds_blocked = NULL,
  folds_random = NULL,
  metric = c("rmse", "mae", "rsq"),
  v = 5L,
  ...
)
```

## Arguments

- data:

  A data frame with predictors and response.

- model:

  A fitted model object with a `predict` method.

- target:

  Character. Response variable name.

- group:

  Character. Column name defining subgroups (e.g., region, time period,
  site type). If NULL and `coords` is provided, subgroups are derived
  from spatial clusters.

- coords:

  Character vector of length 2. Coordinate column names. Used for
  spatial subgroup clustering if `group` is NULL.

- n_groups:

  Integer. Number of spatial clusters when auto-grouping. Default: 4.

- folds_blocked:

  A list of train/test folds from
  [`borg_cv`](https://gillescolling.com/BORG/reference/borg_cv.md). If
  NULL, generated automatically.

- folds_random:

  A list of random CV folds. If NULL, generated automatically.

- metric:

  Character. Metric to compute: `"rmse"`, `"mae"`, or `"rsq"`. Default:
  `"rmse"`.

- v:

  Integer. Number of folds if generating automatically. Default: 5.

- ...:

  Additional arguments passed to `predict`.

## Value

A list with class `"borg_fairness"` containing:

- subgroup_metrics:

  Data frame with columns: group, metric_random, metric_blocked, n_obs,
  disparity (blocked - random), and relative_disparity (percent change).

- overall:

  List with overall random and blocked metrics.

- max_disparity:

  The largest subgroup-level performance gap between random and blocked
  CV.

- worst_group:

  The subgroup with the worst blocked CV performance.

- hidden_by_leakage:

  Logical. TRUE if any subgroup's blocked performance is substantially
  worse than random CV suggested.

- metric:

  The metric used.

## Details

This function evaluates whether leakage disproportionately affects
certain data subgroups. A model may have uniform performance under
random CV but show large disparities when spatial or temporal
independence is enforced.

For example, in ecological modelling, a random-CV RMSE of 0.5 across all
regions may hide the fact that spatially blocked CV yields RMSE of 0.3
in data-rich regions but 1.2 in undersampled regions.

## See also

[`borg_cv`](https://gillescolling.com/BORG/reference/borg_cv.md),
[`borg_compare_cv`](https://gillescolling.com/BORG/reference/borg_compare_cv.md)

## Examples

``` r
# \donttest{
set.seed(42)
d <- data.frame(x = runif(200), y = runif(200))
d$z <- sin(d$x * 3) + rnorm(200, sd = 0.3)
model <- lm(z ~ x + y, data = d)
fair <- borg_fairness(d, model, target = "z", coords = c("x", "y"))
fair
# }
```
