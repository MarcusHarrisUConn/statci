# Lint R or Quarto/RMarkdown source code supplied as a string

Convenience wrapper around
[`stat_lint()`](https://marcusharrisphd.com/statci/reference/stat_lint.md)
for in-memory code strings, unit tests, and programmatic checks.

## Usage

``` r
stat_lint_text(
  text,
  format = c("r", "qmd", "rmd"),
  rules = NULL,
  exclude_rules = NULL,
  skip_eval_false = FALSE
)
```

## Arguments

- text:

  Character scalar or vector of source lines.

- format:

  Source format: `"r"` for R code or `"qmd"` / `"rmd"` for
  Quarto/RMarkdown documents.

- rules:

  Optional character vector of rule IDs or list of `statci_rule`
  objects. Default `NULL` runs all registered rules.

- exclude_rules:

  Optional character vector of rule IDs to skip.

- skip_eval_false:

  Logical; for `"qmd"`/`"rmd"`, skip `eval = FALSE` chunks. Default
  `FALSE`.

## Value

An S3 object of class `"statci_lint"`.

## Examples

``` r
stat_lint_text("set.seed(123)\nx <- rnorm(100)\n")
#> [PASS] statci lint: 1 file(s), 20 rule(s) | 0 error(s), 0 warning(s), 0 info
stat_lint_text("x <- rnorm(100)\n")
#> [FAIL] statci lint: 1 file(s), 20 rule(s) | 1 error(s), 0 warning(s), 0 info
#> ERROR <text:r>:1:6 (unseeded_randomness): Stochastic call `rnorm()` invoked
#> without a preceding `set.seed()` or `withr::with_seed()` call.
```
