# Draw a Clustered Spatial Sample

Generates spatially clustered sample points using a parent-offspring
process: cluster centres are placed uniformly across the domain, and
each sample point is drawn near a randomly assigned centre. Clustered
designs are the common ecological sampling pattern and produce the
strong spatial dependence that random cross-validation understates,
making this useful for simulations, teaching, and stress-testing
evaluation schemes.

## Usage

``` r
borg_clustered_sample(area, n, n_clusters = 5, radius = NULL, seed = NULL)
```

## Arguments

- area:

  The sampling domain. One of: a numeric vector
  `c(xmin, xmax, ymin, ymax)`; a data frame or matrix of coordinates
  (its bounding box is used); or an `sf` polygon or `SpatVector`
  polygon, in which case points are constrained to lie inside it.

- n:

  Integer. Total number of sample points to draw.

- n_clusters:

  Integer. Number of cluster centres (parents). Default: 5.

- radius:

  Numeric. Cluster spread: the maximum offset of a point from its
  centre, in coordinate units. If `NULL`, set to 5\\ diagonal.

- seed:

  Integer or `NULL`. Random seed for reproducibility. Default: `NULL`
  (no seeding).

## Value

A data frame with columns `x`, `y`, and `cluster` (the integer id of the
assigned centre). Attributes `n_clusters` and `radius` record the
design. Drops directly into
[`borg_diagnose()`](https://gillescolling.com/BORG/reference/borg_diagnose.md)
and [`borg_cv()`](https://gillescolling.com/BORG/reference/borg_cv.md)
via `coords = c("x", "y")`.

## See also

[`borg_simulate`](https://gillescolling.com/BORG/reference/borg_simulate.md),
[`borg_diagnose`](https://gillescolling.com/BORG/reference/borg_diagnose.md)

## Examples

``` r
set.seed(1)
s <- borg_clustered_sample(c(0, 100, 0, 100), n = 120, n_clusters = 6)
nrow(s)
length(unique(s$cluster))
```
