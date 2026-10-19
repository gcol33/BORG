# Local Moran's I for Residuals

Computes local Moran's I statistics to identify spatial clusters of
high/low residuals (hotspots and coldspots).

## Usage

``` r
borg_local_moran(residuals, x, y, k = 8L)
```

## Arguments

- residuals:

  Numeric vector of model residuals.

- x:

  Numeric. X-coordinates.

- y:

  Numeric. Y-coordinates.

- k:

  Integer. Number of nearest neighbors for spatial weights. Default: 8.

## Value

A data frame with columns: x, y, local_i, p_value, cluster_type.
