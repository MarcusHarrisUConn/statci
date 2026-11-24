# Audit manuscript prose or tables supplied as an in-memory string

In-memory counterpart to
[`audit_manuscript()`](https://marcusharrisphd.com/statci/reference/audit_manuscript.md),
supporting Markdown/Quarto and LaTeX (`.tex`) prose, tables, and
internal arithmetic verification.

## Usage

``` r
audit_manuscript_text(
  text,
  models = NULL,
  tol_extra = 0,
  warn_rounding = TRUE,
  default_model = NULL,
  format = c("auto", "markdown", "tex"),
  check_internal = TRUE
)
```

## Arguments

- text:

  Character scalar or vector of manuscript lines.

- models:

  Optional saved models to cross-check against. One of:

  - A named list of fitted model objects (`lm`, `glm`, `lmerMod`,
    `htest`) or tidy summary data frames;

  - A single fitted model object or tidy summary data frame;

  - Path to an `.rds` file containing a model or named list of models;

  - Path to a directory containing `.rds` model files (keyed by filename
    stem);

  - `NULL` to run internal arithmetic consistency checks only.

- tol_extra:

  Non-negative numeric extra tolerance added on top of the automatic
  rounding half-unit (`0.5 * 10^(-decimals)`). Default `0`.

- warn_rounding:

  Logical; if `TRUE` (default), values off by at most one unit in the
  last reported decimal place are flagged as `"warn"`
  (`rounding_discrepancy`) rather than `"fail"`.

- default_model:

  Optional default model name used for untagged claims when multiple
  models are provided.

- format:

  Document format: `"auto"` (detects LaTeX commands automatically),
  `"markdown"` (Quarto/RMarkdown), or `"tex"` (LaTeX).

- check_internal:

  Logical; if `TRUE` (default), run interval-arithmetic internal
  consistency checks (`t` vs `b / SE`, `95% CI` midpoint & width vs `b`
  and `SE`) on every claim group and table row.

## Value

An S3 object of class `"statci_manuscript_audit"`.

## Examples

``` r
fit <- lm(mpg ~ wt, data = mtcars)
audit_manuscript_text(
  "<!-- statci: model=fit term=wt -->\nEffect: b = -5.34, N = 32.",
  models = list(fit = fit)
)
#> [PASS] statci Manuscript Audit (<text>): 2 claim(s) | 2 pass, 0 warn, 0 fail
```
