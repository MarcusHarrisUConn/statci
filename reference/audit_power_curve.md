# Audit an estimator's empirical power curve across an effect-size grid

Simulates datasets across a grid of true effect sizes `effect_grid` and
audits: (1) empirical rejection probability with exact Clopper-Pearson
95% confidence intervals and Monte Carlo standard errors, (2) Type-I
error calibration at `effect = 0` (when present in `effect_grid`), (3)
statistical monotonicity of empirical power as effect magnitude
increases (via MCSE-adjusted two-proportion z-tests so Monte Carlo noise
does not trigger spurious failures), and (4) achievement of
`target_power` at `target_effect`, along with linear-interpolated
minimum detectable effect (`mde`).

## Usage

``` r
audit_power_curve(
  dgp,
  test_fn,
  effect_grid,
  param = NULL,
  n_sim = 100L,
  alpha = 0.05,
  target_power = 0.8,
  target_effect = NULL,
  check_monotonic = TRUE,
  check_null_type1 = TRUE,
  seed = 42L,
  mc_alpha = 0.01
)
```

## Arguments

- dgp:

  Function `function(effect)` generating a dataset for the given numeric
  effect size.

- test_fn:

  Function `function(data)` returning either a scalar numeric p-value, a
  logical scalar (`TRUE` = reject), a list containing `$p_value` or
  `$p.value`, or an `htest`, `lm`, `glm`, or `lmerMod` object.

- effect_grid:

  Numeric vector of at least two effect sizes to evaluate (e.g.,
  `c(0, 0.2, 0.5, 0.8)`).

- param:

  Optional character parameter name when `test_fn` returns an `lm`,
  `glm`, or `lmerMod` object with multiple predictors (defaults to the
  first non-intercept coefficient).

- n_sim:

  Integer number of Monte Carlo replications per grid point (default
  `100L`).

- alpha:

  Nominal two-sided significance level (default `0.05`).

- target_power:

  Target statistical power threshold (default `0.80`).

- target_effect:

  Optional numeric effect size in `effect_grid` at which `target_power`
  must be achieved. If `NULL`, no target-power gate is enforced (though
  `mde` is still estimated).

- check_monotonic:

  Logical; if `TRUE` (default), verify empirical power does not drop
  significantly as `abs(effect)` increases.

- check_null_type1:

  Logical; if `TRUE` (default) and `0` is included in `effect_grid`,
  audit Type-I error calibration against `alpha`.

- seed:

  Integer RNG seed for reproducibility (default `42L`).

- mc_alpha:

  Exact binomial / Monte Carlo failure significance threshold (default
  `0.01`).

## Value

An S3 object of class `"statci_power_curve"` containing `curve` (a
[tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html)),
`monotonicity`, `target_check`, `null_check`, `mde`, `settings`, and
overall `status` (`"pass"`, `"warn"`, or `"fail"`).

## Examples

``` r
pc <- audit_power_curve(
  dgp = function(d) rnorm(40, mean = d, sd = 1),
  test_fn = function(y) t.test(y, mu = 0),
  effect_grid = c(0, 0.25, 0.5, 0.75),
  n_sim = 80L,
  target_effect = 0.5,
  target_power = 0.80,
  seed = 42L
)
pc
#> [PASS] statci Power-Curve Audit (4 grid points, n_sim = 80, MDE(80%) = 0.528)
#> effect = 0.000: power = 8.8% [3.6%, 17.2%] (MCSE = 0.032)
#> effect = 0.250: power = 33.8% [23.6%, 45.2%] (MCSE = 0.053)
#> effect = 0.500: power = 77.5% [66.8%, 86.1%] (MCSE = 0.047)
#> effect = 0.750: power = 100.0% [95.5%, 100.0%] (MCSE = 0.000)
#> [PASS] Null Type-I error: 8.8% (binom p = 0.1218)
#> [PASS] Monotonicity: 0 significant drop(s) (max drop z = 0.00)
#> [PASS] Target power (80% at effect = 0.500): achieved 77.5%
```
