# Scan Methods Text for Common Leakage Patterns

Analyzes a methods section (as text) for descriptions of evaluation
practices that commonly lead to data leakage. Useful for reviewing
papers, teaching, or auditing your own methods description.

## Usage

``` r
borg_literature_check(
  text,
  context = c("auto", "spatial", "temporal", "general"),
  strict = FALSE
)
```

## Arguments

- text:

  Character string. The methods section text to analyze. Can be a single
  string or character vector (lines).

- context:

  Character. What kind of data the methods describe. One of `"spatial"`,
  `"temporal"`, `"general"`, or `"auto"` (default, tries to detect from
  text).

- strict:

  Logical. If TRUE, flags borderline practices too. Default: FALSE.

## Value

A list with class `"borg_lit_check"` containing:

- flags:

  Data frame with columns: pattern, severity (`"high"`, `"medium"`,
  `"low"`), matched_text, explanation, and recommendation.

- n_flags:

  Total number of issues found.

- detected_context:

  The data context detected or provided.

- summary:

  One-line summary.

## Details

The scanner looks for textual patterns that describe evaluation
practices known to cause leakage. It does NOT parse actual code or data
— it analyzes the *description* of methods.

### Patterns checked

- Random CV on spatial data:

  Any mention of random/stratified k-fold CV when spatial coordinates
  are mentioned.

- Preprocessing before splitting:

  Normalization, PCA, or feature selection described before train/test
  splitting.

- No spatial blocking:

  Spatial data evaluated without spatial CV or buffer zones.

- Feature selection on full data:

  Variable selection or importance computed before CV.

- Temporal look-ahead:

  Random splits on time series or panel data.

- No embargo:

  Temporal CV without gap/embargo between train and test.

## See also

[`borg_explain_risk`](https://gillescolling.com/BORG/reference/borg_explain_risk.md),
[`borg_report`](https://gillescolling.com/BORG/reference/borg_report.md)

## Examples

``` r
methods_text <- "We used 10-fold cross-validation to evaluate
  species distribution models. Predictors were normalized and
  reduced via PCA before model fitting. Occurrence records
  were collected across 50 sites in Europe."

result <- borg_literature_check(methods_text)
result
```
