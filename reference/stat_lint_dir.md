# Lint all R, Quarto, and RMarkdown files in one or more directories

Recursively discovers `.R`, `.r`, `.qmd`, and `.Rmd` files under `path`
and combines their diagnostics into a single `"statci_lint"` object.

## Usage

``` r
stat_lint_dir(
  path = ".",
  pattern = "\\.(R|r|qmd|Rmd|rmd)$",
  recursive = TRUE,
  exclude_pattern = "(^|/)(\\.git|renv|packrat|\\.Rproj\\.user)/",
  rules = NULL,
  exclude_rules = NULL,
  skip_eval_false = FALSE
)
```

## Arguments

- path:

  Character vector of directory path(s) to scan.

- pattern:

  Regular expression matching files to lint. Defaults to
  `"\\.(R|r|qmd|Rmd|rmd)$"`.

- recursive:

  Logical; scan subdirectories recursively? Default `TRUE`.

- exclude_pattern:

  Optional regex of paths to skip (e.g., `"renv/|packrat/"`).

- rules:

  Optional subset of rules to run.

- exclude_rules:

  Optional character vector of rule IDs to skip.

- skip_eval_false:

  Logical; skip `eval = FALSE` chunks in `.qmd`/`.Rmd`.

## Value

An S3 object of class `"statci_lint"`.

## Examples

``` r
tmp <- tempfile("statci_dir_")
dir.create(tmp)
writeLines("set.seed(1)\nx <- rnorm(10)", file.path(tmp, "ok.R"))
stat_lint_dir(tmp)
#> [PASS] statci lint: 1 file(s), 20 rule(s) | 0 error(s), 0 warning(s), 0 info
unlink(tmp, recursive = TRUE)
```
