# Register BORG Hooks

Registers BORG validation hooks that automatically check data
dependencies when using common ML framework functions. This is an
experimental feature.

## Usage

``` r
borg_register_hooks(
  frameworks = c("rsample", "caret", "mlr3"),
  action = c("error", "warn", "message")
)
```

## Arguments

- frameworks:

  Character vector. Which frameworks to hook into. Options: "rsample",
  "caret", "mlr3". Default: all available.

- action:

  Character. What to do when dependencies detected: "error" (block),
  "warn" (warn but proceed), "message" (info only).

## Value

Invisible NULL. Called for side effect.

## Details

This function uses R's trace mechanism to add BORG checks to framework
functions. The hooks are session-specific and do not persist.

To remove hooks, use
[`borg_unregister_hooks()`](https://gillescolling.com/BORG/reference/borg_unregister_hooks.md).

## Examples

``` r
# \donttest{
if (requireNamespace("rsample", quietly = TRUE)) {
  # Register hooks for rsample
  borg_register_hooks("rsample")

  # Now vfold_cv() will check for dependencies
  spatial_data <- data.frame(
    lon = runif(50), lat = runif(50), response = rnorm(50)
  )
  options(borg.check_data = spatial_data)
  options(borg.check_coords = c("lon", "lat"))

  # Remove hooks
  borg_unregister_hooks()
}
# }
```
