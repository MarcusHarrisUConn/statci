# Audit analytical robustness across a multiverse specification curve

Evaluates a modeling pipeline across the full Cartesian grid of
analytical decision forks (e.g., outlier handling, covariate sets,
missing-data rules, and estimator choices) following specification-curve
/ multiverse analysis. Summarizes specification-level estimates,
standard errors, confidence intervals, p-values, convergence rates,
overall median and interquartile range (IQR), sign consistency,
proportion statistically significant (\\p \le \alpha\\), and per-fork
variance decomposition (`eta_squared`) identifying which analytical
decision drives fragility.

## Usage

``` r
audit_multiverse(
  data,
  decisions,
  fit_fn,
  extract_fn = NULL,
  param = NULL,
  alpha = 0.05,
  max_specs = 256L,
  min_sign_consistency_warn = 0.9,
  min_sign_consistency_fail = 0.75,
  max_iqr_ratio_warn = 0.5,
  max_iqr_ratio_fail = 1,
  max_fail_rate = 0.1
)
```

## Arguments

- data:

  Analysis dataset (typically a `data.frame`) passed as the first
  argument to `fit_fn`.

- decisions:

  Non-empty named list of analytical decision forks, where each element
  is a vector of candidate choices (e.g.,
  `list(outliers = c("none", "winsor"), covariates = c("minimal", "full"))`).

- fit_fn:

  Function `function(data, spec)` receiving `data` and a named list
  `spec` specifying one choice per decision fork, and returning either a
  fitted model (`lm`, `glm`, `lmerMod`, `htest`), a named list with
  `estimate` (and optional `se`, `ci_lower`, `ci_upper`, `p_value`,
  `converged`), or a numeric scalar/vector.

- extract_fn:

  Optional extractor `function(fit)` returning a named list (`estimate`,
  optional `se`, `ci_lower`, `ci_upper`, `p_value`, `converged`) or
  numeric vector. When `NULL`, `statci` auto-extracts from standard
  model objects and lists.

- param:

  Optional character scalar naming the focal parameter to audit when
  `fit_fn` returns multiple coefficients (defaults to the first
  non-intercept parameter).

- alpha:

  Nominal two-sided significance threshold for computing
  `prop_significant` and Wald confidence intervals (default `0.05`).

- max_specs:

  Maximum allowed Cartesian grid size (default `256L`) to prevent
  accidental combinatorial explosion in CI.

- min_sign_consistency_warn:

  Minimum sign-consistency proportion before emitting `"warn"` (default
  `0.90`).

- min_sign_consistency_fail:

  Minimum sign-consistency proportion before emitting `"fail"` (default
  `0.75`).

- max_iqr_ratio_warn:

  Maximum `IQR / |median|` spread ratio before emitting `"warn"`
  (default `0.50`).

- max_iqr_ratio_fail:

  Maximum `IQR / |median|` spread ratio before emitting `"fail"`
  (default `1.00`).

- max_fail_rate:

  Maximum tolerable specification failure/non-convergence rate before
  emitting `"fail"` (default `0.10`).

## Value

An S3 object of class `"statci_multiverse"` containing `specifications`
(sorted
[tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html)),
`summary` (list of overall robustness metrics), `fork_sensitivity`
([tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html)
ranking each decision fork by variance explained), `decisions`,
`settings`, and overall `status` (`"pass"`, `"warn"`, or `"fail"`).

## Examples

``` r
set.seed(42)
df <- data.frame(x = rnorm(80), c1 = rnorm(80))
df$y <- 1 + 0.9 * df$x + 0.3 * df$c1 + rnorm(80, sd = 0.6)
mv <- audit_multiverse(
  data = df,
  decisions = list(
    outliers = c("none", "winsor_5pct"),
    covariates = c("bivariate", "adjusted")
  ),
  fit_fn = function(d, spec) {
    if (spec$outliers == "winsor_5pct") {
      qs <- quantile(d$y, c(0.05, 0.95))
      d$y <- pmin(pmax(d$y, qs[1]), qs[2])
    }
    if (spec$covariates == "bivariate") lm(y ~ x, data = d) else lm(y ~ x + c1, data = d)
  },
  param = "x"
)
mv
#> [PASS] statci Multiverse Specification Curve (x: 4/4 valid specs)
#> median = 0.8066, IQR = 0.0841 [min = 0.7434, max = 0.8714]
#> sign consistency = 100.0%, p <= 0.05 = 100.0%, top driver = outliers
```
