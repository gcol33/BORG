# Enable/Disable BORG Auto-Check Mode

Configures BORG to automatically validate train/test splits when using
supported ML frameworks. When enabled, BORG will intercept common
modeling functions and validate indices before training proceeds.

## Usage

``` r
borg_auto_check(enable = TRUE, strict = TRUE, verbose = FALSE)
```

## Arguments

- enable:

  Logical. If TRUE, enable auto-check mode. If FALSE, disable.

- strict:

  Logical. If TRUE, throw errors on violations. If FALSE, warn.

- verbose:

  Logical. If TRUE, print diagnostic messages.

## Value

Invisibly returns the previous state of auto-check options.

## Examples

``` r
# Enable auto-checking with strict mode
borg_auto_check(TRUE)

# Disable auto-checking
borg_auto_check(FALSE)

# Enable with warnings instead of errors
borg_auto_check(TRUE, strict = FALSE)
```
