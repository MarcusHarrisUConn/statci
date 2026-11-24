# Incrementally lint only added or modified lines in a Git diff / pull request

Enables adopting `statci` in continuous integration on large legacy
repositories without failing on untouched historical code. Parses either
a supplied unified diff (`diff_text`) or invokes safe read-only
`git diff` against a strictly validated `base_ref`, runs full-file AST
analysis on each modified `.R`/`.qmd`/`.Rmd` file (preserving cross-line
and cross-chunk context such as upstream
[`set.seed()`](https://rdrr.io/r/base/Random.html) calls), and reports
**only findings landing on added or modified lines**.

## Usage

``` r
stat_lint_diff(
  path = ".",
  base_ref = "HEAD",
  head_ref = NULL,
  diff_text = NULL,
  staged = FALSE,
  rules = NULL,
  exclude_rules = NULL,
  skip_eval_false = FALSE,
  pattern = "\\.(R|r|qmd|Rmd|rmd)$"
)
```

## Arguments

- path:

  Root repository or project directory (default `"."`).

- base_ref:

  Git base reference (e.g., `"HEAD"`, `"main"`, `"HEAD~1"`, or commit
  SHA) used when `diff_text` is `NULL` and `staged = FALSE`.

- head_ref:

  Optional Git head reference for two-ref diffs (e.g.,
  `base_ref = "main"`, `head_ref = "HEAD"`). When `NULL` (default),
  diffs `base_ref` against the current working tree.

- diff_text:

  Optional character scalar or vector containing unified diff output
  (`git diff -U0` or `diff -u`). When provided, `git` is not invoked.

- staged:

  Logical; if `TRUE` (and `diff_text` is `NULL`), runs
  `git diff --cached` against staged index changes. Default `FALSE`.

- rules:

  Optional subset of lint rules (forwarded to
  [`stat_lint()`](https://marcusharrisphd.com/statci/reference/stat_lint.md)).

- exclude_rules:

  Optional character vector of rule IDs to skip.

- skip_eval_false:

  Logical; for `.qmd`/`.Rmd` files, skip `eval = FALSE` chunks. Default
  `FALSE`.

- pattern:

  Regular expression matching lintable files (defaults to
  `"\\.(R|r|qmd|Rmd|rmd)$"`).

## Value

An S3 object of class `"statci_lint"` (compatible with
[`statci_report()`](https://marcusharrisphd.com/statci/reference/statci_report.md),
[`render_statci_report()`](https://marcusharrisphd.com/statci/reference/render_statci_report.md),
[`export_statci_sarif()`](https://marcusharrisphd.com/statci/reference/export_statci_sarif.md),
and
[`statci_check_gate()`](https://marcusharrisphd.com/statci/reference/statci_check_gate.md))
with an additional `$diff_meta` list recording `changed_files`,
`changed_lines`, and `n_suppressed_untouched`.

## Details

All Git refs are strictly validated against option injection (leading
`-`, control bytes, shell characters), and all diff file paths are
strictly confined inside `path` (blocking `..` path traversal and
symlink escapes).

## Examples

``` r
tmp <- tempfile("statci_diff_")
dir.create(tmp)
script <- file.path(tmp, "legacy.R")
writeLines(c(
  "legacy_x <- rnorm(10)",
  "set.seed(42)",
  "new_val <- mean(rnorm(5), na.rm = T)"
), script)
diff_u <- paste(
  "--- a/legacy.R",
  "+++ b/legacy.R",
  "@ -2,0 +3,1 @",
  "+new_val <- mean(rnorm(5), na.rm = T)",
  sep = "\n"
)
# Line 1 (legacy unseeded rnorm) is untouched and ignored; only line 3 is flagged!
res <- stat_lint_diff(path = tmp, diff_text = diff_u)
res
#> [PASS] statci lint: 0 file(s), 20 rule(s) | 0 error(s), 0 warning(s), 0 info
unlink(tmp, recursive = TRUE)
```
