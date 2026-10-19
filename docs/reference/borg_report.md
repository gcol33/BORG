# Generate BORG HTML Diagnostic Report

Creates a self-contained HTML report with embedded plots summarizing the
full BORG analysis: diagnosis, variogram, CV folds, performance, and
risk assessment.

## Usage

``` r
borg_report(
  object,
  file = "borg_report.html",
  title = "BORG Diagnostic Report",
  open = interactive()
)
```

## Arguments

- object:

  A `borg_workflow` or `borg_result` object.

- file:

  Character. Output file path. Default: `"borg_report.html"`.

- title:

  Character. Report title.

- open:

  Logical. Open in browser after generation. Default: `TRUE` in
  interactive sessions.

## Value

Invisible path to the generated HTML file.

## Details

Plots are embedded as base64-encoded PNGs. Requires ggplot2. No
rmarkdown or pandoc dependency.

## Examples

``` r
# \donttest{
set.seed(42)
d <- data.frame(x = runif(100), y = runif(100), z = rnorm(100))
wf <- borg_workflow(d, z ~ x + y, coords = c("x", "y"))
borg_report(wf, file = tempfile(fileext = ".html"), open = FALSE)
# }
```
