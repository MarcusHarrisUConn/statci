# Plot a two-panel publication-ready specification curve and decision matrix

Renders a two-panel visualization aligning each sorted specification's
point estimate and 95% confidence interval (top panel) directly above
its active analytical decision choices (bottom panel).

## Usage

``` r
# S3 method for class 'statci_multiverse'
plot(x, ...)
```

## Arguments

- x:

  A `"statci_multiverse"` object from
  [`audit_multiverse()`](https://marcusharrisphd.com/statci/reference/audit_multiverse.md).

- ...:

  Unused.

## Value

A `ggplot` object if `ggplot2` is installed, otherwise draws a two-panel
base R plot and returns `x` invisibly.

## Examples

``` r
set.seed(1)
df <- data.frame(x = rnorm(60), c1 = rnorm(60))
df$y <- 1 + 0.8 * df$x + rnorm(60)
mv <- audit_multiverse(
  df,
  decisions = list(cov = c("none", "c1"), trim = c("no", "yes")),
  fit_fn = function(d, spec) {
    if (spec$trim == "yes") d <- d[-1, ]
    if (spec$cov == "none") lm(y ~ x, data = d) else lm(y ~ x + c1, data = d)
  },
  param = "x"
)
plot(mv)
```
