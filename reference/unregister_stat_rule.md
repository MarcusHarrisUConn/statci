# Remove a rule from the active registry

Remove a rule from the active registry

## Usage

``` r
unregister_stat_rule(id)
```

## Arguments

- id:

  Character scalar rule identifier to remove.

## Value

Invisibly returns `TRUE` if removed, `FALSE` if not present.

## Examples

``` r
r <- new_stat_rule("temp_rule", "Temp", check = function(ctx) list())
register_stat_rule(r)
unregister_stat_rule("temp_rule")
```
