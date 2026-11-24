# Register a custom statistical lint rule

Adds a `statci_rule` created with
[`new_stat_rule()`](https://marcusharrisphd.com/statci/reference/new_stat_rule.md)
to the active `statci` rule registry so that subsequent calls to
[`stat_lint()`](https://marcusharrisphd.com/statci/reference/stat_lint.md)
and
[`stat_lint_dir()`](https://marcusharrisphd.com/statci/reference/stat_lint_dir.md)
include it automatically.

## Usage

``` r
register_stat_rule(rule, overwrite = FALSE)
```

## Arguments

- rule:

  A `statci_rule` object created by
  [`new_stat_rule()`](https://marcusharrisphd.com/statci/reference/new_stat_rule.md).

- overwrite:

  Logical; if `TRUE`, allows replacing an existing rule with the same
  `id`.

## Value

Invisibly returns `rule`.

## Examples

``` r
r <- new_stat_rule(
  id = "demo_rule",
  title = "Demo rule",
  severity = "info",
  check = function(ctx) list()
)
register_stat_rule(r)
"demo_rule" %in% list_stat_rules()$id
#> [1] TRUE
unregister_stat_rule("demo_rule")
```
