# Render a `statci_report` to standalone HTML, GitHub-Flavored Markdown, or JSON

Render a `statci_report` to standalone HTML, GitHub-Flavored Markdown,
or JSON

## Usage

``` r
render_statci_report(
  report,
  output_file = NULL,
  format = c("html", "markdown", "json"),
  title = NULL,
  open = FALSE
)
```

## Arguments

- report:

  A `"statci_report"` object created by
  [`statci_report()`](https://marcusharrisphd.com/statci/reference/statci_report.md).

- output_file:

  Optional output file path. When `NULL`, returns the rendered document
  as a character scalar without writing to disk.

- format:

  Output format: `"html"` (standalone HTML5 dashboard), `"markdown"`
  (GitHub-Flavored Markdown suitable for `$GITHUB_STEP_SUMMARY`), or
  `"json"` (machine-readable JSON; requires `jsonlite`).

- title:

  Optional override for the report title.

- open:

  Logical; if `TRUE`,
  [`interactive()`](https://rdrr.io/r/base/interactive.html), and
  `output_file` is provided, opens the rendered report in the default
  browser.

## Value

If `output_file` is `NULL`, returns the rendered character scalar;
otherwise writes the file and returns `output_file` invisibly.

## Examples

``` r
rep <- statci_report(lint = stat_lint_text("x <- rnorm(10)\n"))
md <- render_statci_report(rep, format = "markdown")
cat(md)
#> # statci Statistical CI/CD Audit Report
#> 
#> **Overall Status:** **FAIL** | **Generated:** 2026-11-24 21:43:32 UTC | **statci:** v0.5.1
#> 
#> ## Executive Summary
#> 
#> | Section | Status | Checks | Passed | Warnings | Failures | Summary |
#> | --- | --- | --- | --- | --- | --- | --- |
#> | AST Statistical Linter | **FAIL** | 20 | 19 | 0 | 1 | 1 file(s), 1 finding(s) (1 error, 0 warning, 0 info) |
#> 
#> ## 1. AST Statistical Linter
#> 
#> | File | Line | Chunk | Severity | Rule | Message |
#> | --- | --- | --- | --- | --- | --- |
#> | &lt;text:r&gt; | 1 | — | ERROR | `unseeded_randomness` | Stochastic call `rnorm()` invoked without a preceding `set.seed()` or `withr::with_seed()` call. |
```
