# Cache and Retrieve BORG Diagnoses

Caches
[`BorgDiagnosis`](https://gillescolling.com/BORG/reference/BorgDiagnosis.md)
objects keyed by a hash of the input data, so expensive computations
(variograms, distance matrices, autocorrelation tests) are not repeated
across iterations.

## Usage

``` r
borg_cache_get(data, coords = NULL, target = NULL, envir = .borg_cache_env)

borg_cache_set(
  data,
  diagnosis,
  coords = NULL,
  target = NULL,
  envir = .borg_cache_env
)

borg_cache_clear(envir = .borg_cache_env)

borg_cache_info(envir = .borg_cache_env)
```

## Arguments

- data:

  A data frame. Used to compute the cache key.

- coords:

  Character vector. Included in the cache key to distinguish diagnoses
  of the same data with different coordinate columns.

- target:

  Character. Included in the cache key.

- envir:

  Environment for in-memory cache. Default: the package namespace cache.

- diagnosis:

  A `BorgDiagnosis` object to cache (for `borg_cache_set`), or NULL.

## Value

- `borg_cache_get`:

  A `BorgDiagnosis` or NULL if not cached.

- `borg_cache_set`:

  Invisible NULL. Stores the diagnosis.

- `borg_cache_clear`:

  Invisible NULL. Clears all cached diagnoses.

- `borg_cache_info`:

  A data frame with cache key, timestamp, and data dimensions for all
  cached entries.

## Examples

``` r
d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
diag <- borg_diagnose(d, coords = c("x", "y"), target = "z")

# Cache it
borg_cache_set(d, diag, coords = c("x", "y"), target = "z")

# Retrieve (fast, no recomputation)
cached <- borg_cache_get(d, coords = c("x", "y"), target = "z")
identical(diag, cached)  # TRUE

# Clear all
borg_cache_clear()
```
