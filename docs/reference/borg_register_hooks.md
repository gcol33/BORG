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
if (FALSE) { # \dontrun{
# Register hooks for rsample
borg_register_hooks("rsample")

# Now vfold_cv() will check for dependencies
# (requires borg.check_data to be set)
options(borg.check_data = my_spatial_data)
options(borg.check_coords = c("lon", "lat"))

rsample::vfold_cv(my_spatial_data)  # Will warn/error

# Remove hooks
borg_unregister_hooks()
} # }
```
