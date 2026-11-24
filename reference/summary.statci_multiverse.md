# Summarize a multiverse specification-curve audit

Summarize a multiverse specification-curve audit

## Usage

``` r
# S3 method for class 'statci_multiverse'
summary(object, ...)
```

## Arguments

- object:

  A `"statci_multiverse"` object from
  [`audit_multiverse()`](https://marcusharrisphd.com/statci/reference/audit_multiverse.md).

- ...:

  Unused.

## Value

Invisibly returns a list containing `overall` (1-row
[tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html))
and `fork_sensitivity`
([tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html)).

## Examples

``` r
df <- data.frame(x = rnorm(50), y = rnorm(50))
mv <- audit_multiverse(
  df,
  decisions = list(model = c("a", "b")),
  fit_fn = function(d, spec) lm(y ~ x, data = d)
)
summary(mv)
#> [PASS] statci Multiverse Specification Curve (x: 2/2 valid specs)
#> median = 0.0349, IQR = 0.0000 [min = 0.0349, max = 0.0349]
#> sign consistency = 100.0%, p <= 0.05 = 0.0%, top driver = NA
#> Decision Fork Variance Decomposition:
#> - model (2 levels): median range = 0.0000, eta^2 = 0.0%
```
