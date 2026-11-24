# statci

> **Automated statistical CI/CD for R:** 20-rule AST methodological
> linter with incremental Git-diff PR linting & SARIF 2.1.0 export,
> Simulation-Based Calibration (SBC) & power-curve fuzzing, parallel RNG
> invariance testing, Quarto/RMarkdown/LaTeX manuscript auditing,
> adversarial sensitivity, and multiverse specification-curve robustness
> suites.

**Documentation:** <https://marcusharrisphd.com/statci/>

## Why `statci`?

Software teams rely on static analyzers, fuzzers, and continuous
integration to catch subtle defects before release. Quantitative
pipelines and academic manuscripts routinely suffer from silent
statistical bugs that standard R tooling misses:

- **`lintr`** checks formatting and style—not unseeded stochastic calls,
  parallel RNG collisions, duplicate worker seeds, train/test data
  leakage, uncentered multilevel interactions, unvalidated stepwise
  selection, or silent listwise mismatches.
- **`statcheck`** only checks internal APA p-value arithmetic—it cannot
  inspect R code, cross-check prose and tables against saved R model
  objects, or audit estimators via simulation.
- **`testthat`** validates deterministic code logic—not probabilistic
  properties like Type-I error calibration, exact Clopper-Pearson
  interval coverage, power monotonicity, rank uniformity, or robustness
  under data perturbations.

`statci` unifies five verification pillars into a single CRAN-ready
workflow with `.statci.yml` project orchestration, SARIF 2.1.0 GitHub
Code Scanning, standalone HTML/Markdown audit reports, and a
security-hardened GitHub Actions generator.

## Installation

``` r

# Install development version from GitHub
# install.packages("remotes")
remotes::install_github("MarcusHarrisUConn/statci")
```

## Five Verification Pillars

### 1. AST-Based Statistical Linter & Incremental PR Diff Linting (`stat_lint()`, `stat_lint_dir()`, `stat_lint_diff()`, `export_statci_sarif()`)

Parses `.R`, `.qmd`, and `.Rmd` files into full XML abstract syntax
trees (preserving exact original line numbers and cross-chunk scope in
Quarto/RMarkdown documents) and evaluates **20 built-in methodological
rules** plus user-registered custom rules
([`new_stat_rule()`](https://marcusharrisphd.com/statci/reference/new_stat_rule.md),
[`register_stat_rule()`](https://marcusharrisphd.com/statci/reference/register_stat_rule.md)):

| Rule ID | Severity | Hazard Detected |
|----|----|----|
| `unseeded_randomness` | `error` | Top-level stochastic calls (`rnorm`, `sample`, `boot`, `kmeans`, etc.) without prior [`set.seed()`](https://rdrr.io/r/base/Random.html) / [`withr::with_seed()`](https://withr.r-lib.org/reference/with_seed.html) |
| `unsafe_parallel_rng` | `error` | `mclapply` / `parLapply` / `foreach %dopar%` / `future_*` without L’Ecuyer-CMRG, `clusterSetRNGStream()`, `doRNG`, or `future.seed` |
| `duplicate_parallel_seed` | `error` | Constant `set.seed(42)` inside loops or parallel workers generating identical duplicate replications |
| `preprocessing_before_split` | `error` | [`scale()`](https://rdrr.io/r/base/scale.html), [`prcomp()`](https://rdrr.io/r/stats/prcomp.html), `preProcess()`, or imputation applied to the full dataset before train/test splitting |
| `in_sample_prediction_eval` | `warning` | Evaluating RMSE, MAE, AUC, or MSE on training-set predictions rather than held-out test/CV data |
| `uncentered_mlm_interaction` | `warning` | Fixed-effect interactions in `lmer`/`glmer`/`lme`/`glmmTMB`/`brm` using raw uncentered predictors |
| `silent_listwise_mismatch` | `warning` | Comparing models with differing variable sets via [`anova()`](https://rdrr.io/r/stats/anova.html)/[`AIC()`](https://rdrr.io/r/stats/AIC.html)/[`BIC()`](https://rdrr.io/r/stats/AIC.html) on unharmonized data |
| `silent_factor_drop` | `warning` | `as.numeric(factor_var)` level-code coercion or modeling [`subset()`](https://rdrr.io/r/base/subset.html)/[`filter()`](https://rdrr.io/r/stats/filter.html) data without [`droplevels()`](https://rdrr.io/r/base/droplevels.html) |
| `silent_na_coercion` | `warning` | Unchecked [`as.numeric()`](https://rdrr.io/r/base/numeric.html)/[`as.integer()`](https://rdrr.io/r/base/integer.html) coercion of character/parsed strings without [`anyNA()`](https://rdrr.io/r/base/NA.html)/[`is.na()`](https://rdrr.io/r/base/NA.html) verification |
| `unverified_merge_cardinality` | `warning` | `dplyr::*_join()` or [`merge()`](https://rdrr.io/r/base/merge.html) without `relationship =` or [`nrow()`](https://rdrr.io/r/base/nrow.html) guards against silent many-to-many row duplication |
| `missing_drop_false_matrix` | `warning` | 2D matrix/array subsetting (`X[, cols]`, `X[idx, ]`) missing `drop = FALSE`, silently collapsing 1-row/1-col subsets to vectors |
| `implicit_na_rm_masking` | `info` | Unconditional `na.rm = TRUE` in summary statistics without nearby missingness inspection ([`is.na()`](https://rdrr.io/r/base/NA.html), [`anyNA()`](https://rdrr.io/r/base/NA.html), [`complete.cases()`](https://rdrr.io/r/stats/complete.cases.html)) |
| `equality_on_float` | `warning` | Exact `==` / `!=` comparisons on floating-point literals or continuous statistics |
| `unadjusted_multiple_testing` | `warning` | Repeated hypothesis tests inside loops/`lapply`/`map` without [`p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) in scope |
| `stepwise_selection_unvalidated` | `warning` | In-sample [`step()`](https://rdrr.io/r/stats/step.html) / [`MASS::stepAIC()`](https://rdrr.io/pkg/MASS/man/stepAIC.html) / `regsubsets()` without cross-validation or holdout validation |
| `missing_convergence_check` | `warning` | `lmer`/`glmer`/`optim`/`nlminb` fits without programmatic AST checks for `isSingular()` or `$convergence` |
| `global_seed_override_in_fn` | `warning` | Bare [`set.seed()`](https://rdrr.io/r/base/Random.html) inside reusable functions or `\(x)` lambdas without restoring `.Random.seed` |
| `short_logical_literal` | `warning` | Mutable `T`/`F` symbols instead of `TRUE`/`FALSE` |
| `cor_without_use_arg` | `info` | [`cor()`](https://rdrr.io/r/stats/cor.html)/[`cov()`](https://rdrr.io/r/stats/cor.html) called without explicit `use =` missing-data handling |
| `absolute_path_in_script` | `warning` | Hardcoded non-portable paths (`/Users/...`, `C:\...`, `~/...`) breaking CI |

``` r

library(statci)

# Lint a script, Quarto document, or project directory
res <- stat_lint(system.file("examples", "flawed-analysis.R", package = "statci"))
res

# Incrementally lint only added/modified lines in a Git PR without legacy noise
pr_res <- stat_lint_diff(path = ".", base_ref = "origin/main", head_ref = "HEAD")

# Export SARIF 2.1.0 JSON for GitHub Code Scanning inline PR annotations
sarif <- export_statci_sarif(res)
```

### 2. Estimator Calibration, SBC, Power Curves & Parallel RNG (`audit_estimator()`, `audit_sbc()`, `audit_power_curve()`, `check_parallel_rng()`)

Verifies custom or standard estimators using Monte Carlo SE-aware
decision rules and exact Clopper-Pearson binomial intervals:

``` r

est_audit <- audit_estimator(
  dgp = function() {
    x <- rnorm(100)
    z <- rnorm(100)
    data.frame(x = x, z = z, y = 1 + 0.8 * x + rnorm(100))
  },
  estimator = function(df) lm(y ~ x + z, data = df),
  true_params = c(x = 0.8, z = 0.0),
  n_sim = 150L,
  check_parallel = TRUE,
  seed = 42L
)

pc_audit <- audit_power_curve(
  dgp = function(d) rnorm(40, mean = d, sd = 1),
  test_fn = function(y) t.test(y, mu = 0),
  effect_grid = c(0, 0.25, 0.50, 0.75),
  n_sim = 100L,
  target_effect = 0.50,
  target_power = 0.80
)
```

### 3. Manuscript-to-Code Consistency Auditor (`audit_manuscript()`)

Cross-checks prose statistics and tables (`b`, `SE`, `95% CI`, `p`, `N`,
`t`, `F`, `chi2`, `r`, `R^2`, `OR`, `AIC`) in `.qmd`, `.Rmd`, and `.tex`
manuscripts against saved R model objects (`lm`, `glm`, `lmerMod`,
`htest`, or tidy tables)—plus interval-arithmetic internal consistency
checks (`t` vs `b / SE`, `95% CI` symmetry and width):

``` r

fit <- lm(mpg ~ wt, data = mtcars)
audit_manuscript(
  system.file("examples", "sample-manuscript.qmd", package = "statci"),
  models = list(primary = fit)
)
```

### 4. Adversarial Sensitivity & Multiverse Specification Curves (`audit_sensitivity()`, `audit_multiverse()`)

Stress-tests fitted pipelines across tail winsorization,
leave-one-cluster-out (LOCO) / leave-one-out jackknife influence,
missingness + imputation contrasts, cluster/observation bootstrap
stability, and full Cartesian multiverse specification curves with
per-fork variance decomposition (`eta_squared`) and two-panel
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) visualizations:

``` r

sens <- audit_sensitivity(
  data = mtcars,
  fit_fn = function(d) lm(mpg ~ wt + hp, data = d),
  n_boot = 100L,
  seed = 42L
)

mv <- audit_multiverse(
  data = mtcars,
  decisions = list(
    outliers = c("none", "winsor_5pct"),
    covariates = c("bivariate", "adjusted")
  ),
  fit_fn = function(d, spec) {
    if (spec$outliers == "winsor_5pct") {
      qs <- quantile(d$mpg, c(0.05, 0.95))
      d$mpg <- pmin(pmax(d$mpg, qs[1]), qs[2])
    }
    if (spec$covariates == "bivariate") lm(mpg ~ wt, data = d) else lm(mpg ~ wt + hp, data = d)
  },
  param = "wt"
)
plot(mv)
```

### 5. Declarative Config, Unified Reports & GitHub Actions (`read_statci_config()`, `statci_audit_project()`, `use_statci_github_action()`)

``` r

# Run full project audit driven by .statci.yml
rep <- statci_audit_project(".", enforce_gate = TRUE)

# Generate hardened .github/workflows/statci.yaml with optional SARIF upload
use_statci_github_action(lint_paths = c("R", "analysis"), upload_sarif = TRUE)
```

## Documentation & Guides

- [`vignette("getting-started", package = "statci")`](https://marcusharrisphd.com/statci/articles/getting-started.md)
  — AST linting, 20 rules, custom rules, and reports.
- [`vignette("calibration-and-robustness", package = "statci")`](https://marcusharrisphd.com/statci/articles/calibration-and-robustness.md)
  — Estimator calibration, SBC, power curves, parallel RNG invariance,
  and adversarial sensitivity.
- [`vignette("manuscript-auditing", package = "statci")`](https://marcusharrisphd.com/statci/articles/manuscript-auditing.md)
  — Auditing Quarto/RMarkdown/LaTeX prose, tables, APA tests, and
  internal arithmetic.
- [`vignette("multiverse-and-pr-diffs", package = "statci")`](https://marcusharrisphd.com/statci/articles/multiverse-and-pr-diffs.md)
  — Multiverse specification curves
  ([`audit_multiverse()`](https://marcusharrisphd.com/statci/reference/audit_multiverse.md)),
  incremental PR diff linting
  ([`stat_lint_diff()`](https://marcusharrisphd.com/statci/reference/stat_lint_diff.md)),
  and data-integrity AST rules.
- [`vignette("github-actions-ci", package = "statci")`](https://marcusharrisphd.com/statci/articles/github-actions-ci.md)
  — `.statci.yml` config,
  [`statci_audit_project()`](https://marcusharrisphd.com/statci/reference/statci_audit_project.md),
  SARIF 2.1.0, and GitHub Actions.
- See
  [ARCHITECTURE.md](https://marcusharrisphd.com/statci/ARCHITECTURE.md)
  and [SECURITY.md](https://marcusharrisphd.com/statci/SECURITY.md) for
  architecture and security details.

## License

MIT © Marcus Harris
