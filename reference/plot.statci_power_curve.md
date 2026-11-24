# Plot empirical power curve with exact Clopper-Pearson confidence band

Plot empirical power curve with exact Clopper-Pearson confidence band

## Usage

``` r
# S3 method for class 'statci_power_curve'
plot(x, ...)
```

## Arguments

- x:

  A `"statci_power_curve"` object from
  [`audit_power_curve()`](https://marcusharrisphd.com/statci/reference/audit_power_curve.md).

- ...:

  Unused.

## Value

A `ggplot` object if `ggplot2` is installed, otherwise draws a base R
plot and returns `x` invisibly.

## Examples

``` r
pc <- audit_power_curve(
  dgp = function(d) rnorm(30, mean = d, sd = 1),
  test_fn = function(y) t.test(y, mu = 0),
  effect_grid = c(0, 0.3, 0.6),
  n_sim = 30L,
  seed = 42L
)
plot(pc)
```
