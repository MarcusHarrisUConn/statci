# Audit posterior calibration via Simulation-Based Calibration (SBC)

Implements Simulation-Based Calibration (Talts et al., 2018) for
Bayesian or posterior-sampling estimators. Repeatedly draws true
parameters \\\tilde{\theta} \sim p(\theta)\\, simulates synthetic data
\\\tilde{y} \sim p(y \mid \tilde{\theta})\\, draws \\L\\ posterior
samples, and evaluates uniformity of the rank statistics via Pearson's
chi-square test and a Kolmogorov-Smirnov test, together with empirical
50% and 90% central credible-interval coverage.

## Usage

``` r
audit_sbc(
  prior,
  dgp,
  posterior_draws,
  n_sim = 100L,
  n_bins = 10L,
  seed = 42L,
  mc_alpha = 0.01
)
```

## Arguments

- prior:

  Function of zero arguments returning a numeric vector (or named
  numeric scalar) drawn from the prior distribution \\p(\theta)\\.

- dgp:

  Function `function(theta)` simulating data from the likelihood given
  parameter vector `theta`.

- posterior_draws:

  Function `function(data)` returning posterior draws: either a numeric
  vector of length \\L\\ (for a scalar parameter) or an \\L \times p\\
  numeric matrix/data frame (columns matching parameters).

- n_sim:

  Integer number of SBC replications (default `100L`).

- n_bins:

  Integer number of histogram bins for the chi-square uniformity test
  (default `10L`).

- seed:

  Integer RNG seed for reproducibility (default `42L`).

- mc_alpha:

  Significance threshold for failing uniformity tests (default `0.01`).
  Values between `mc_alpha` and `0.05` emit `"warn"`.

## Value

An S3 object of class `"statci_sbc"` containing `summary` (a
[tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html) per
parameter), `ranks` (matrix of rank statistics), `bin_counts`, `n_sim`,
`n_draws`, `n_bins`, and `status`.

## Examples

``` r
# Exact Normal-Normal conjugate posterior (calibrated -> PASS)
sbc <- audit_sbc(
  prior = function() c(mu = rnorm(1, 0, 2)),
  dgp = function(theta) rnorm(20, mean = theta[["mu"]], sd = 1),
  posterior_draws = function(y) {
    post_var <- 1 / (1 / 4 + length(y))
    post_mean <- post_var * sum(y)
    rnorm(99, mean = post_mean, sd = sqrt(post_var))
  },
  n_sim = 80L,
  n_bins = 10L,
  seed = 42L
)
sbc
#> [WARN] Simulation-Based Calibration (SBC): 80/80 reps, L = 99 draws, 10 bins
#> [WARN] mu: chi2 p = 0.9643, KS p = 0.9541, 50% cov = 47.5%, 90% cov = 78.8%
```
