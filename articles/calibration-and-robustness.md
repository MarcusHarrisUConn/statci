# Estimator Calibration Fuzzing, SBC, Parallel RNG & Adversarial Sensitivity

Deterministic unit tests confirm that code runs without syntax errors,
but they cannot tell you whether a custom statistical estimator is
unbiased, whether its confidence intervals achieve nominal 95% coverage,
whether a Bayesian posterior sampler is calibrated, whether parallel
execution depends on core count, or whether an applied conclusion
collapses when a single outlier or cluster is removed.

This vignette demonstrates `statci`’s probabilistic verification and
robustness tools:

1.  **Estimator Calibration Fuzzing
    ([`audit_estimator()`](https://marcusharrisphd.com/statci/reference/audit_estimator.md))**
    with Monte Carlo SE-aware thresholds.
2.  **Simulation-Based Calibration
    ([`audit_sbc()`](https://marcusharrisphd.com/statci/reference/audit_sbc.md))**
    for posterior samplers (Talts et al., 2018).
3.  **Parallel RNG Invariance
    ([`check_parallel_rng()`](https://marcusharrisphd.com/statci/reference/check_parallel_rng.md)
    &
    [`statci_parallel_apply()`](https://marcusharrisphd.com/statci/reference/statci_parallel_apply.md))**.
4.  **Adversarial Sensitivity & Robustness Battery
    ([`audit_sensitivity()`](https://marcusharrisphd.com/statci/reference/audit_sensitivity.md))**.

``` r

library(statci)
```

## 1. Estimator Calibration & Fuzzing

[`audit_estimator()`](https://marcusharrisphd.com/statci/reference/audit_estimator.md)
pairs a user-supplied data-generating process (`dgp`) with an
`estimator` and evaluates bias ($`z`$-score vs. Monte Carlo SE), RMSE,
empirical-vs-model SE ratio, exact Clopper-Pearson interval coverage,
Type-I error under null parameters, and convergence/singularity rates.

### Case A: Well-Calibrated OLS Estimator

``` r

ols_audit <- audit_estimator(
  dgp = function() {
    x <- rnorm(100)
    z <- rnorm(100)
    y <- 1.0 + 0.75 * x + 0.0 * z + rnorm(100, sd = 1.0)
    data.frame(x = x, z = z, y = y)
  },
  estimator = function(df) lm(y ~ x + z, data = df),
  true_params = c(x = 0.75, z = 0.0),
  n_sim = 120L,
  seed = 2026L
)
ols_audit
#> [PASS] statci Estimator Audit (n_sim = 120, valid = 120, fail_rate = 0.0%,
#> singular_rate = 0.0%)
#> Parameter Recovery:
#> [PASS] x: bias = 0.0024 (z = 0.29), RMSE = 0.0915, SE ratio = 1.099, coverage =
#> 98.3%
#> [PASS] z: bias = -0.0042 (z = 0.46), RMSE = 0.0998, SE ratio = 1.001, coverage
#> = 95.8%
#> Type-I Error Calibration (nominal = 5.0%):
#> [PASS] z: empirical = 4.2% [1.4%, 9.5%], binom p = 0.8352
```

Because `z = 0.0`,
[`audit_estimator()`](https://marcusharrisphd.com/statci/reference/audit_estimator.md)
automatically evaluates Type-I error calibration on `z` alongside
parameter recovery on both `x` and `z`.

### Case B: Catching a Biased, Anti-Conservative Custom Estimator

Now suppose a custom estimator has subtle bias and underestimates its
standard error by 50%:

``` r

flawed_audit <- audit_estimator(
  dgp = function() rnorm(60, mean = 1.5, sd = 1.0),
  estimator = function(y) {
    list(
      estimate = c(mu = mean(y) + 0.35),
      se = c(mu = 0.5 * sd(y) / sqrt(length(y)))
    )
  },
  true_params = c(mu = 1.5),
  n_sim = 120L,
  seed = 2026L
)
flawed_audit
#> [FAIL] statci Estimator Audit (n_sim = 120, valid = 120, fail_rate = 0.0%,
#> singular_rate = 0.0%)
#> Parameter Recovery:
#> [FAIL] mu: bias = 0.3525 (z = 30.91), RMSE = 0.3738, SE ratio = 0.517, coverage
#> = 2.5%
```

[`audit_estimator()`](https://marcusharrisphd.com/statci/reference/audit_estimator.md)
immediately flags the bias ($`z \gg 3`$), severe SE deflation
(`se_ratio` $`\approx 0.5`$), and collapsed interval coverage.

## 2. Simulation-Based Calibration (SBC)

For Bayesian or posterior-draw estimators,
[`audit_sbc()`](https://marcusharrisphd.com/statci/reference/audit_sbc.md)
draws $`\tilde\theta \sim p(\theta)`$, simulates
$`\tilde y \sim p(y \mid \tilde\theta)`$, computes rank statistics of
$`\tilde\theta`$ among $`L`$ posterior draws, and tests rank uniformity
via Pearson’s $`\chi^2`$ and Kolmogorov-Smirnov tests:

``` r

# Exact Normal-Normal conjugate posterior (calibrated)
sbc_ok <- audit_sbc(
  prior = function() c(mu = rnorm(1, mean = 0, sd = 2)),
  dgp = function(theta) rnorm(20, mean = theta[["mu"]], sd = 1),
  posterior_draws = function(y) {
    post_var <- 1 / (1 / 4 + length(y))
    post_mean <- post_var * sum(y)
    rnorm(99, mean = post_mean, sd = sqrt(post_var))
  },
  n_sim = 100L,
  n_bins = 10L,
  seed = 42L
)
sbc_ok
#> [WARN] Simulation-Based Calibration (SBC): 100/100 reps, L = 99 draws, 10 bins
#> [WARN] mu: chi2 p = 0.9357, KS p = 0.7920, 50% cov = 47.0%, 90% cov = 80.0%

# Miscalibrated posterior with systematic shift and underdispersion
sbc_bad <- audit_sbc(
  prior = function() c(mu = rnorm(1, mean = 0, sd = 2)),
  dgp = function(theta) rnorm(20, mean = theta[["mu"]], sd = 1),
  posterior_draws = function(y) {
    post_var <- 1 / (1 / 4 + length(y))
    post_mean <- post_var * sum(y) + 0.5
    rnorm(99, mean = post_mean, sd = 0.3 * sqrt(post_var))
  },
  n_sim = 100L,
  n_bins = 10L,
  seed = 42L
)
sbc_bad
#> [FAIL] Simulation-Based Calibration (SBC): 100/100 reps, L = 99 draws, 10 bins
#> [FAIL] mu: chi2 p = 0.0000, KS p = 0.0000, 50% cov = 2.0%, 90% cov = 9.0%
```

## 3. Parallel RNG Invariance Across Core Counts

Standard parallel helpers in R assign one RNG stream per *worker
process*, meaning Monte Carlo results silently change when moving from 1
core on a laptop to 2 or 16 cores in CI.
[`statci_parallel_apply()`](https://marcusharrisphd.com/statci/reference/statci_parallel_apply.md)
assigns one independent `"L'Ecuyer-CMRG"` stream per *task*, and
[`check_parallel_rng()`](https://marcusharrisphd.com/statci/reference/check_parallel_rng.md)
verifies exact invariance across core counts:

``` r

par_audit <- check_parallel_rng(
  fun = function(i) mean(rnorm(50)),
  X = seq_len(6L),
  cores = c(1L, 2L),
  seed = 123L
)
par_audit
#> [PASS] Parallel RNG Invariance: cores = 1 vs 2, identical = TRUE, max |diff| =
#> 0.000000
```

## 4. Adversarial Sensitivity & Robustness Battery

[`audit_sensitivity()`](https://marcusharrisphd.com/statci/reference/audit_sensitivity.md)
automatically subjects any modeling pipeline to tail winsorization,
leave-one-cluster-out (LOCO) or leave-one-out jackknife influence,
missingness/imputation stress tests, and cluster or observation
bootstrapping.

Let’s compare a robust dataset against an outlier-fragile dataset where
a single point reverses the estimated slope:

``` r

set.seed(11)
n <- 45L
x <- rnorm(n, sd = 0.6)
y <- -0.7 * x + rnorm(n, sd = 0.3)

# Inject one extreme leverage outlier that flips the OLS slope positive
x_fragile <- x
y_fragile <- y
x_fragile[1] <- 10
y_fragile[1] <- 15
df_fragile <- data.frame(x = x_fragile, y = y_fragile)

sens_fragile <- audit_sensitivity(
  data = df_fragile,
  fit_fn = function(d) lm(y ~ x, data = d),
  winsor_probs = c(0.05),
  missingness_rates = c(0.05),
  n_boot = 40L,
  max_loo_units = 45L,
  seed = 42L
)
sens_fragile
#> [FAIL] statci Adversarial Sensitivity Battery: 1 parameter(s), 5 scenario
#> check(s)
#> [FAIL] x: baseline = 1.2559, max % change = 167.3%, sign flip = TRUE, boot sign
#> stability = 60.0%
as_tibble(sens_fragile)[, c("category", "scenario", "baseline", "perturbed", "sign_flip", "status")]
#> # A tibble: 5 × 6
#>   category      scenario                baseline perturbed sign_flip status
#>   <chr>         <chr>                      <dbl>     <dbl> <lgl>     <chr> 
#> 1 winsorization winsor_5pct                 1.26    -0.647 TRUE      fail  
#> 2 influence     leave_one_out               1.26    -0.788 TRUE      fail  
#> 3 missingness   mcar_5pct_complete_case     1.26    -0.793 TRUE      fail  
#> 4 missingness   mcar_5pct_median            1.26    -0.845 TRUE      fail  
#> 5 bootstrap     bootstrap_40                1.26     0.471 FALSE     fail
```

Notice how both tail winsorization and leave-one-out influence
immediately identify observation `1` as the single driver of the sign
reversal and fail the robustness gate.

## 5. Power-Curve Auditing (`audit_power_curve()`)

[`audit_power_curve()`](https://marcusharrisphd.com/statci/reference/audit_power_curve.md)
audits empirical rejection probabilities across an effect-size grid with
exact Clopper-Pearson 95% confidence bounds, Type-I error verification
at `effect = 0`, MCSE-adjusted monotonicity testing, target-power
verification, and linear-interpolated minimum detectable effect (`mde`):

``` r

pc_audit <- audit_power_curve(
  dgp = function(d) rnorm(40, mean = d, sd = 1),
  test_fn = function(y) t.test(y, mu = 0),
  effect_grid = c(0, 0.25, 0.50, 0.75),
  n_sim = 80L,
  target_effect = 0.50,
  target_power = 0.80,
  seed = 42L
)
pc_audit
#> [PASS] statci Power-Curve Audit (4 grid points, n_sim = 80, MDE(80%) = 0.528)
#> effect = 0.000: power = 8.8% [3.6%, 17.2%] (MCSE = 0.032)
#> effect = 0.250: power = 33.8% [23.6%, 45.2%] (MCSE = 0.053)
#> effect = 0.500: power = 77.5% [66.8%, 86.1%] (MCSE = 0.047)
#> effect = 0.750: power = 100.0% [95.5%, 100.0%] (MCSE = 0.000)
#> [PASS] Null Type-I error: 8.8% (binom p = 0.1218)
#> [PASS] Monotonicity: 0 significant drop(s) (max drop z = 0.00)
#> [PASS] Target power (80% at effect = 0.500): achieved 77.5%
```

## 6. Unified Report Assembly

All verification pillars compose cleanly into a single
[`statci_report()`](https://marcusharrisphd.com/statci/reference/statci_report.md)
ready for standalone HTML or Markdown output in CI:

``` r

final_rep <- statci_report(
  estimator = ols_audit,
  sbc = sbc_ok,
  parallel_rng = par_audit,
  sensitivity = sens_fragile,
  power_curve = pc_audit,
  title = "Calibration & Robustness Verification"
)
final_rep
#> [FAIL] Calibration & Robustness Verification
#> [PASS] Estimator Calibration & Fuzzing: 120/120 valid reps; fail_rate = 0.0%,
#> singular_rate = 0.0%
#> [WARN] Simulation-Based Calibration (SBC): 100/100 reps, L = 99 draws, 10 bins
#> [PASS] Parallel RNG Invariance: cores = 1 vs 2, identical = TRUE
#> [FAIL] Adversarial Sensitivity Battery: 1 parameter(s), 5 perturbation check(s)
#> [PASS] Power-Curve Audit: 4 grid point(s), MDE = 0.528
```
