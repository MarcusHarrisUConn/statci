# Lint an R script or Quarto/RMarkdown document for statistical hazards

Parses `.R`, `.qmd`, or `.Rmd` source code into an abstract syntax tree
(AST) and evaluates registered statistical and methodological lint
rules.

## Usage

``` r
stat_lint(path, rules = NULL, exclude_rules = NULL, skip_eval_false = FALSE)
```

## Arguments

- path:

  Path to an `.R`, `.r`, `.qmd`, or `.Rmd` file.

- rules:

  Optional subset of rules to run: a character vector of rule IDs, a
  list of `statci_rule` objects, or `NULL` (default) to run all
  registered rules.

- exclude_rules:

  Optional character vector of rule IDs to skip.

- skip_eval_false:

  Logical; for `.qmd`/`.Rmd` files, skip chunks with `eval = FALSE` or
  `#| eval: false`. Default `FALSE`.

## Value

An S3 object of class `"statci_lint"` containing `findings` (a
[tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html)),
`files`, `rules_run`, `summary`, and `status` (`"pass"`, `"warn"`, or
`"fail"`).

## Examples

``` r
res <- stat_lint_text("x <- rnorm(100)\n")
res
#> [FAIL] statci lint: 1 file(s), 20 rule(s) | 1 error(s), 0 warning(s), 0 info
#> ERROR <text:r>:1:6 (unseeded_randomness): Stochastic call `rnorm()` invoked
#> without a preceding `set.seed()` or `withr::with_seed()` call.
```
