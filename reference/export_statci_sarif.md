# Export `statci` AST lint results to SARIF 2.1.0 JSON

Serializes a `"statci_lint"` object (or the lint component of a
`"statci_report"`) into a valid Static Analysis Results Interchange
Format (SARIF) 2.1.0 JSON log compatible with GitHub Code Scanning
(`github/codeql-action/upload-sarif@v3`) for inline pull-request
annotations.

## Usage

``` r
export_statci_sarif(
  lint,
  output_file = NULL,
  base_uri = NULL,
  tool_version = NULL
)
```

## Arguments

- lint:

  A `"statci_lint"` object from
  [`stat_lint()`](https://marcusharrisphd.com/statci/reference/stat_lint.md)
  /
  [`stat_lint_dir()`](https://marcusharrisphd.com/statci/reference/stat_lint_dir.md)
  or a `"statci_report"` containing a lint component.

- output_file:

  Optional path to write the `.sarif` JSON file. If `NULL` (default),
  returns the serialized SARIF 2.1.0 JSON string.

- base_uri:

  Optional base project directory path stripped from file paths so
  artifact URIs are clean repository-relative paths.

- tool_version:

  Optional tool version string (defaults to the installed `statci`
  package version).

## Value

If `output_file` is `NULL`, returns a character scalar of SARIF 2.1.0
JSON; otherwise writes the file and returns `output_file` invisibly.

## Examples

``` r
lint_res <- stat_lint_text("x <- rnorm(10)\n")
sarif_json <- export_statci_sarif(lint_res)
parsed <- jsonlite::fromJSON(sarif_json)
parsed$version
#> [1] "2.1.0"
```
