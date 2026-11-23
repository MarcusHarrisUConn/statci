# statci

> **Automated statistical CI/CD for R:** AST-based methodological linting, Simulation-Based Calibration (SBC) fuzzing, parallel RNG invariance testing, manuscript-to-code auditing, and adversarial sensitivity suites.

## Why `statci`?

Software teams rely on static analyzers, fuzzers, and continuous integration to catch subtle defects before release. Quantitative pipelines and academic manuscripts routinely suffer from silent statistical bugs that standard R tooling misses:

- **`lintr`** checks formatting and style—not unseeded stochastic calls, parallel RNG collisions, train/test data leakage, uncentered multilevel interactions, or silent listwise deletion mismatches.
- **`statcheck`** only checks internal APA p-value arithmetic—it cannot inspect R code, cross-check numbers against saved model objects, or audit estimators via simulation.
- **`testthat`** validates deterministic code logic—not probabilistic properties like Type-I error calibration, Clopper-Pearson interval coverage, rank uniformity, or robustness under data perturbations.

`statci` unifies five verification pillars into a single CRAN-ready workflow with standalone HTML/Markdown audit reports and a ready-to-use GitHub Actions generator.

## Installation

```r
# Install development version from GitHub
# install.packages("remotes")
remotes::install_github("MarcusHarrisUConn/statci")
```

## Five Verification Pillars

### 1. AST-Based Statistical Linter (`stat_lint()`, `stat_lint_dir()`)

Parses `.R`, `.qmd`, and `.Rmd` files into full XML abstract syntax trees (preserving exact original line numbers and cross-chunk scope in Quarto/RMarkdown documents) and evaluates **14 built-in methodological rules** plus user-registered custom rules (`new_stat_rule()`, `register_stat_rule()`):

| Rule ID | Severity | Hazard Detected |
|---|---|---|
| `unseeded_randomness` | `error` | Top-level stochastic calls (`rnorm`, `sample`, `boot`, `kmeans`, etc.) without prior `set.seed()` / `withr::with_seed()` |
| `unsafe_parallel_rng` | `error` | `mclapply` / `parLapply` / `foreach %dopar%` / `future_*` without L'Ecuyer-CMRG, `clusterSetRNGStream()`, `doRNG`, or `future.seed` |
| `duplicate_parallel_seed` | `error` | Constant `set.seed(42)` inside loops or parallel workers generating identical duplicate replications |
| `preprocessing_before_split` | `error` | `scale()`, `prcomp()`, `preProcess()`, or imputation applied to the full dataset before train/test splitting |
| `uncentered_mlm_interaction` | `warning` | Fixed-effect interactions in `lmer`/`glmer`/`lme`/`glmmTMB`/`brm` using raw uncentered predictors |
| `silent_listwise_mismatch` | `warning` | Comparing models with differing variable sets via `anova()`/`AIC()`/`BIC()` on unharmonized data |
| `silent_factor_drop` | `warning` | `as.numeric(factor_var)` level-code coercion or modeling `subset()`/`filter()` data without `droplevels()` |
| `equality_on_float` | `warning` | Exact `==` / `!=` comparisons on floating-point literals or continuous statistics |
| `unadjusted_multiple_testing` | `warning` | Repeated hypothesis tests inside loops/`lapply`/`map` without `p.adjust()` in scope |
| `missing_convergence_check` | `warning` | `lmer`/`glmer`/`optim`/`nlminb` fits without `isSingular()` or `$convergence` checks |
| `global_seed_override_in_fn` | `warning` | Bare `set.seed()` inside reusable functions without restoring `.Random.seed` |
| `short_logical_literal` | `warning` | Mutable `T`/`F` symbols instead of `TRUE`/`FALSE` |
| `cor_without_use_arg` | `info` | `cor()`/`cov()` called without explicit `use =` missing-data handling |
| `absolute_path_in_script` | `warning` | Hardcoded non-portable paths (`/Users/...`, `C:\...`, `~/...`) breaking CI |

```r
library(statci)

# Lint an R script, Quarto document, or project directory
stat_lint_text("
df$x <- scale(df$x)
idx <- sample(nrow(df), 50)
fit <- lme4::lmer(y ~ x * w + (1 + x | school), data = df)
")
```

### 2. Estimator Calibration, SBC & Parallel RNG Harness (`audit_estimator()`, `audit_sbc()`, `check_parallel_rng()`)

Verifies custom or standard estimators using Monte Carlo SE-aware decision rules and exact Clopper-Pearson binomial intervals:

```r
est_audit <- audit_estimator(
  dgp = function() {
    x <- rnorm(100)
    z <- rnorm(100)
    data.frame(x = x, z = z, y = 1 + 0.8 * x + rnorm(100))
  },
  estimator = function(df) lm(y ~ x + z, data = df),
  true_params = c(x = 0.8, z = 0.0),
  n_sim = 200L,
  check_parallel = TRUE,
  seed = 42L
)
est_audit
```

### 3. Manuscript-to-Code Consistency Auditor (`audit_manuscript()`)

Cross-checks prose statistics (`b`, `SE`, `95% CI`, `p = ...`, `p < .001`, `N`, `t`, `r`, `R^2`, `OR`, `AIC`) in `.qmd`/`.Rmd` manuscripts against saved R model objects (`lm`, `glm`, `lmerMod`, `htest`, or tidy tables), catching stale copy-paste numbers, rounding discrepancies, sign flips, and sample-size drift:

```r
fit <- lm(mpg ~ wt, data = mtcars)
audit_manuscript_text(
  "<!-- statci: model=fit term=wt -->\nWeight reduced MPG (b = -5.34, N = 32).",
  models = list(fit = fit)
)
```

### 4. Adversarial Sensitivity & Robustness Battery (`audit_sensitivity()`)

Stress-tests fitted pipelines across tail winsorization, leave-one-cluster-out (LOCO) / leave-one-out jackknife influence, missingness + imputation contrasts, and cluster/observation bootstrap stability:

```r
sens <- audit_sensitivity(
  data = mtcars,
  fit_fn = function(d) lm(mpg ~ wt + hp, data = d),
  n_boot = 100L,
  seed = 42L
)
sens
```

### 5. Unified Reports & GitHub Actions CI (`statci_report()`, `render_statci_report()`, `use_statci_github_action()`)

```r
rep <- statci_report(
  lint = stat_lint_dir("R"),
  estimator = est_audit,
  sensitivity = sens
)

render_statci_report(rep, output_file = "statci-report.html", format = "html")
render_statci_report(rep, output_file = "statci-report.md", format = "markdown")
statci_check_gate(rep, fail_on = "fail")

# Generate .github/workflows/statci.yaml
use_statci_github_action(lint_paths = c("R", "analysis"))
```

## Documentation

- `vignette("getting-started", package = "statci")` — AST linting, custom rules, manuscript auditing, reports, and GitHub Actions setup.
- `vignette("calibration-and-robustness", package = "statci")` — Estimator calibration fuzzing, Simulation-Based Calibration (SBC), parallel RNG invariance, and adversarial sensitivity batteries.
- See [ARCHITECTURE.md](ARCHITECTURE.md) for the statistical decision rules and AST rule engine design.

## License

MIT © Marcus Harris
