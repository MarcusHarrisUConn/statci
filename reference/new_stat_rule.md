# Construct a custom `statci` lint rule

Creates a `statci_rule` object that can be passed to
[`stat_lint()`](https://marcusharrisphd.com/statci/reference/stat_lint.md)
or registered globally with
[`register_stat_rule()`](https://marcusharrisphd.com/statci/reference/register_stat_rule.md).

## Usage

``` r
new_stat_rule(
  id,
  title,
  severity = c("warning", "error", "info"),
  category = "modeling",
  description = "",
  recommendation = "",
  check
)
```

## Arguments

- id:

  Unique snake_case identifier for the rule (character scalar).

- title:

  Short human-readable title (character scalar).

- severity:

  Severity level: `"error"`, `"warning"`, or `"info"`.

- category:

  Methodological category (e.g., `"reproducibility"`, `"parallel_rng"`,
  `"data_leakage"`, `"modeling"`, `"numerics"`, `"inference"`,
  `"hygiene"`).

- description:

  Detailed explanation of the statistical hazard.

- recommendation:

  Actionable remediation guidance.

- check:

  A function `function(ctx)` taking an internal parse context
  (containing `file`, `lines`, `units`, `line_to_chunk`) and returning a
  list of finding records, each with elements `line`, `col`, `message`
  (and optional `chunk`, `code_snippet`).

## Value

An S3 object of class `"statci_rule"`.

## Examples

``` r
my_rule <- new_stat_rule(
  id = "no_attach",
  title = "Avoid attach() in analysis scripts",
  severity = "warning",
  category = "hygiene",
  description = "attach() mutates the search path and causes silent variable masking.",
  recommendation = "Reference columns explicitly via data$col or with().",
  check = function(ctx) {
    out <- list()
    for (u in ctx$units) {
      calls <- xml2::xml_find_all(
        u$xml,
        ".//expr[expr[1]/SYMBOL_FUNCTION_CALL[text()='attach'] and OP-LEFT-PAREN]"
      )
      for (nd in calls) {
        ln <- as.integer(xml2::xml_attr(nd, "line1")) + u$line_offset
        out[[length(out) + 1L]] <- list(
          line = ln,
          col = as.integer(xml2::xml_attr(nd, "col1")),
          message = "Avoid attach(); reference data frames explicitly."
        )
      }
    }
    out
  }
)
my_rule
#> <statci_rule> no_attach [WARNING | hygiene]
#> Avoid attach() in analysis scripts
#> attach() mutates the search path and causes silent variable masking.
```
