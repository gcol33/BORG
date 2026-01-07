# Plot Group-Based Split

Visualizes group membership and train/test assignment, highlighting any
groups that appear in both sets (group leakage).

## Usage

``` r
plot_groups(
  groups,
  train_idx,
  test_idx,
  title = "Group-Based Split",
  max_groups = 20
)
```

## Arguments

- groups:

  Vector of group assignments for each observation.

- train_idx:

  Integer vector of training indices.

- test_idx:

  Integer vector of test indices.

- title:

  Plot title.

- max_groups:

  Maximum number of groups to display (for readability).

## Value

A base R plot (invisibly returns NULL).

## Examples

``` r
# Create grouped data
groups <- rep(paste0("Patient_", 1:10), each = 10)
train_idx <- which(groups %in% paste0("Patient_", 1:7))
test_idx <- which(groups %in% paste0("Patient_", 8:10))
plot_groups(groups, train_idx, test_idx)

# With group leakage
train_idx_bad <- 1:70
test_idx_bad <- 61:100  # Overlaps with Patient_7
plot_groups(groups, train_idx_bad, test_idx_bad)
```
