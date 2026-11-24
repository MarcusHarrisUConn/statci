# Plot adversarial sensitivity shifts across scenarios

Plot adversarial sensitivity shifts across scenarios

## Usage

``` r
# S3 method for class 'statci_sensitivity'
plot(x, parameter = NULL, ...)
```

## Arguments

- x:

  A `"statci_sensitivity"` object from
  [`audit_sensitivity()`](https://marcusharrisphd.com/statci/reference/audit_sensitivity.md).

- parameter:

  Optional parameter name to plot (defaults to all).

- ...:

  Unused.

## Value

A `ggplot` object if `ggplot2` is installed, otherwise draws a base R
dotchart and returns `x` invisibly.

## Examples

``` r
sens <- audit_sensitivity(
  data = mtcars,
  fit_fn = function(d) lm(mpg ~ wt, data = d),
  n_boot = 20L,
  max_loo_units = 15L,
  seed = 42L
)
plot(sens)
```
