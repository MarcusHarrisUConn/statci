# Audit a Quarto, RMarkdown, or LaTeX manuscript against saved R model objects

Parses inline prose and Markdown/LaTeX tables in `.qmd`, `.Rmd`, `.md`,
or `.tex` manuscripts—covering point estimates, standard errors,
p-values, confidence intervals, test statistics (`t`, `z`, `F`, `chi2`,
`r`), odds ratios (`OR`), sample sizes (`N`), \\R^2\\, and AIC/BIC—and
cross-checks them against fitted R model objects (`lm`, `glm`,
`lmerMod`, `htest`, or tidy summary data frames). Also performs
**internal arithmetic cross-checking** (verifying that reported `t`/`z`
matches `b / SE` and that reported `95% CI` bounds are consistent with
`b` and `SE` up to rounding intervals), which works even for untagged
claims or when `models = NULL`.

## Usage

``` r
audit_manuscript(
  manuscript,
  models = NULL,
  tol_extra = 0,
  warn_rounding = TRUE,
  default_model = NULL,
  check_internal = TRUE
)
```

## Arguments

- manuscript:

  Path to a `.qmd`, `.Rmd`, `.md`, or `.tex` file.

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

- check_internal:

  Logical; if `TRUE` (default), run interval-arithmetic internal
  consistency checks (`t` vs `b / SE`, `95% CI` midpoint & width vs `b`
  and `SE`) on every claim group and table row.

## Value

An S3 object of class `"statci_manuscript_audit"` containing `checks`
(model comparison
[tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html)),
`internal_checks` (internal arithmetic
[tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html)),
`summary`, `manuscript`, `models_checked`, and overall `status`
(`"pass"`, `"warn"`, or `"fail"`).

## Details

Supports explicit annotations for zero-ambiguity binding:

- HTML comment anchors: `<!-- statci: model=fit1 term=x1 -->`

- LaTeX comment anchors: `% statci: model=fit1 term=x1`

- Quarto inline spans:
  `[b = 0.42, SE = 0.11, p = .003]{.statci model="fit1" term="x1"}`

## Examples

``` r
fit <- lm(mpg ~ wt, data = mtcars)
txt <- paste0(
  "<!-- statci: model=fit term=wt -->\n",
  "Weight predicted MPG (b = ", round(coef(fit)["wt"], 2),
  ", N = ", nobs(fit), ").\n"
)
audit_manuscript_text(txt, models = list(fit = fit))
#> [PASS] statci Manuscript Audit (<text>): 2 claim(s) | 2 pass, 0 warn, 0 fail
```
