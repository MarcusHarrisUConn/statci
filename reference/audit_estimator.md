# Audit a statistical estimator via Monte Carlo calibration and fuzzing

Evaluates parameter recovery (bias, RMSE, empirical vs. model SE ratio,
and confidence-interval coverage with exact Clopper-Pearson bounds
accounting for Monte Carlo error), Type-I error calibration under a null
DGP, non-convergence and singular-fit rates, and optionally
Simulation-Based Calibration
([`audit_sbc()`](https://marcusharrisphd.com/statci/reference/audit_sbc.md))
and parallel RNG invariance
([`check_parallel_rng()`](https://marcusharrisphd.com/statci/reference/check_parallel_rng.md)).

## Usage

``` r
audit_estimator(
  dgp,
  estimator,
  true_params = NULL,
  n_sim = 200L,
  alpha = 0.05,
  null_dgp = NULL,
  null_params = NULL,
  n_sim_null = n_sim,
  sbc_prior = NULL,
  sbc_dgp = NULL,
  sbc_draws = NULL,
  sbc_n_sim = 100L,
  sbc_bins = 10L,
  check_parallel = FALSE,
  parallel_runner = NULL,
  parallel_cores = c(1L, 2L),
  parallel_n_sim = 8L,
  seed = 42L,
  mc_alpha = 0.01,
  bias_tol_mc = 3,
  se_ratio_bounds = c(0.85, 1.15),
  max_fail_rate = 0.05,
  warn_fail_rate = 0.01,
  max_singular_rate = 0.1
)
```

## Arguments

- dgp:

  Data-generating function. Either `function()` (0 arguments) or
  `function(true_params)` receiving `true_params`.

- estimator:

  Fitting function `function(data)` returning either:

  - A named list with `estimate` (required) and optional `se`,
    `ci_lower`, `ci_upper`, `p_value`, `converged`, `singular`;

  - A fitted `lm`, `glm`, `lmerMod`, `glmerMod`, or `htest` object; or

  - A numeric vector of point estimates.

- true_params:

  Named numeric vector (or scalar) of ground-truth parameter values
  matching names returned by `estimator`. If unnamed, names are matched
  positionally to the estimator output.

- n_sim:

  Integer number of Monte Carlo replications (default `200L`).

- alpha:

  Nominal significance level for confidence intervals (`1 - alpha`) and
  Type-I error checks (default `0.05`).

- null_dgp:

  Optional null data-generating function for dedicated Type-I error
  calibration. If `NULL`, any parameters in `true_params` equal to `0`
  are automatically audited for Type-I error.

- null_params:

  Optional character vector of parameter names to evaluate under
  `null_dgp` (defaults to all parameters returned by `estimator`).

- n_sim_null:

  Integer number of replications for `null_dgp` (defaults to `n_sim`).

- sbc_prior:

  Optional prior sampler for Simulation-Based Calibration (forwarded to
  [`audit_sbc()`](https://marcusharrisphd.com/statci/reference/audit_sbc.md)).

- sbc_dgp:

  Optional likelihood simulator `function(theta)` for SBC (defaults to
  `dgp`).

- sbc_draws:

  Optional posterior draw function `function(data)` for SBC.

- sbc_n_sim:

  Integer SBC replications (default `100L`).

- sbc_bins:

  Integer SBC histogram bins (default `10L`).

- check_parallel:

  Logical; if `TRUE`, also runs
  [`check_parallel_rng()`](https://marcusharrisphd.com/statci/reference/check_parallel_rng.md)
  to verify parallel RNG invariance. Default `FALSE`.

- parallel_runner:

  Optional custom `function(cores, seed)` for
  [`check_parallel_rng()`](https://marcusharrisphd.com/statci/reference/check_parallel_rng.md).
  If `NULL` and `check_parallel = TRUE`, automatically tests `dgp` +
  `estimator` through
  [`statci_parallel_apply()`](https://marcusharrisphd.com/statci/reference/statci_parallel_apply.md).

- parallel_cores:

  Integer vector of core counts to compare (default `c(1L, 2L)`).

- parallel_n_sim:

  Number of tasks used in parallel invariance check (default `8L`).

- seed:

  Integer RNG seed (default `42L`).

- mc_alpha:

  Exact binomial / Monte Carlo failure significance threshold (default
  `0.01`).

- bias_tol_mc:

  Monte Carlo z-score threshold (`|bias| / MCSE_bias`) for failing bias
  recovery (default `3.0`).

- se_ratio_bounds:

  Numeric length-2 target range for `mean(model_SE) / empirical_SE`
  (default `c(0.85, 1.15)`).

- max_fail_rate:

  Maximum tolerable error/non-convergence rate before `"fail"` (default
  `0.05`).

- warn_fail_rate:

  Error/non-convergence rate threshold for `"warn"` (default `0.01`).

- max_singular_rate:

  Maximum tolerable singular-fit rate before `"fail"` (default `0.10`).

## Value

An S3 object of class `"statci_estimator_audit"` containing `recovery`,
`type1`, `convergence`, `sbc`, `parallel_rng`, `settings`, and overall
`status` (`"pass"`, `"warn"`, or `"fail"`).

## Examples

``` r
res <- audit_estimator(
  dgp = function() {
    x <- rnorm(80)
    data.frame(x = x, y = 0.5 * x + rnorm(80))
  },
  estimator = function(df) lm(y ~ x, data = df),
  true_params = c(x = 0.5),
  n_sim = 60L,
  seed = 42L
)
res
#> [PASS] statci Estimator Audit (n_sim = 60, valid = 60, fail_rate = 0.0%,
#> singular_rate = 0.0%)
#> Parameter Recovery:
#> [PASS] x: bias = 0.0057 (z = 0.36), RMSE = 0.1214, SE ratio = 0.930, coverage =
#> 93.3%
```
