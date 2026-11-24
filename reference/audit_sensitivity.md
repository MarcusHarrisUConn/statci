# Run an adversarial sensitivity and robustness battery on a statistical pipeline

Automatically stress-tests a fitted modeling pipeline across four
structured perturbation families:

1.  **Outlier winsorization** of continuous predictors/outcomes at tail
    quantiles (e.g., 1% and 5%).

2.  **Leave-one-cluster-out (LOCO) / leave-one-out (LOO)** jackknife
    influence analysis.

3.  **Missingness stress tests** (contrasting complete-case analysis
    against median/mean imputation under observed and injected MCAR
    missingness).

4.  **Nonparametric bootstrap stability** (cluster bootstrap when
    `cluster_col` is given, otherwise row bootstrap).

## Usage

``` r
audit_sensitivity(
  data,
  fit_fn,
  extract_fn = NULL,
  cluster_col = NULL,
  numeric_cols = NULL,
  winsor_probs = c(0.01, 0.05),
  leave_one_out = TRUE,
  max_loo_units = 50L,
  missingness_rates = c(0.05, 0.1),
  imputation_methods = c("complete_case", "median"),
  n_boot = 100L,
  seed = 42L,
  max_pct_change_warn = 0.15,
  max_pct_change_fail = 0.35,
  sign_flip_fail = TRUE,
  boot_ci_level = 0.95
)
```

## Arguments

- data:

  A `data.frame` containing the analysis dataset.

- fit_fn:

  Function `function(df)` fitting the model on a perturbed data frame
  and returning a fitted model or numeric/list result.

- extract_fn:

  Optional function `function(fit)` returning a named numeric vector of
  target parameter estimates. If `NULL`, `statci` automatically extracts
  non-intercept coefficients from `lm`, `glm`, `lmerMod`, `htest`, or
  lists with `$estimate`.

- cluster_col:

  Optional character scalar naming the clustering column (e.g.,
  `"school"`, `"subject_id"`). Enables leave-one-cluster-out (LOCO)
  influence checks and cluster resampling in the bootstrap.

- numeric_cols:

  Optional character vector of numeric columns to winsorize and perturb.
  Defaults to all non-binary numeric columns excluding `cluster_col`.

- winsor_probs:

  Numeric vector of tail probabilities for winsorization (default
  `c(0.01, 0.05)`). Set to `NULL` or `numeric(0)` to skip.

- leave_one_out:

  Logical; run LOO/LOCO influence check? Default `TRUE`.

- max_loo_units:

  Integer cap on units dropped during LOO/LOCO (default `50L`) to keep
  CI runtimes bounded on large datasets.

- missingness_rates:

  Numeric vector of injected MCAR missingness rates (default
  `c(0.05, 0.10)`). Set to `NULL` or `numeric(0)` to skip injected
  missingness.

- imputation_methods:

  Character vector of missing-data handlers to evaluate
  (`"complete_case"`, `"median"`, `"mean"`).

- n_boot:

  Integer number of nonparametric bootstrap resamples (default `100L`).
  Set to `0L` to skip bootstrap.

- seed:

  Integer RNG seed for reproducibility (default `42L`).

- max_pct_change_warn:

  Relative change threshold (`|delta / baseline|`) triggering `"warn"`
  (default `0.15`, i.e., 15%).

- max_pct_change_fail:

  Relative change threshold triggering `"fail"` (default `0.35`, i.e.,
  35%).

- sign_flip_fail:

  Logical; if `TRUE` (default), a sign reversal relative to a non-zero
  baseline triggers `"fail"`.

- boot_ci_level:

  Confidence level for bootstrap percentile intervals (default `0.95`).

## Value

An S3 object of class `"statci_sensitivity"` containing `baseline`,
`results` (tidy per-scenario tibble), `loo_details`, `bootstrap`,
`parameter_summary`, and overall `status` (`"pass"`, `"warn"`,
`"fail"`).

## Examples

``` r
set.seed(1)
df <- data.frame(x = rnorm(80), y = 2 + 1.5 * rnorm(80))
df$y <- 1 + 1.2 * df$x + rnorm(80, sd = 0.5)
sens <- audit_sensitivity(
  data = df,
  fit_fn = function(d) lm(y ~ x, data = d),
  n_boot = 40L,
  max_loo_units = 20L,
  seed = 42L
)
sens
#> [PASS] statci Adversarial Sensitivity Battery: 1 parameter(s), 8 scenario
#> check(s)
#> [PASS] x: baseline = 1.2963, max % change = 4.3%, sign flip = FALSE, boot sign
#> stability = 100.0%
```
