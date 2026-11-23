# `statci` Architecture

`statci` brings software-engineering-grade continuous integration, static methodological verification, probabilistic calibration fuzzing, manuscript-to-code auditing, and adversarial sensitivity testing to statistical R pipelines and Quarto/RMarkdown manuscripts.

---

## 1. High-Level Module Map

```
statci/
├── DESCRIPTION / NAMESPACE / LICENSE
├── ARCHITECTURE.md
├── R/
│   ├── statci-package.R       # Package-level docs and shared imports
│   ├── security.R             # Centralized HTML/MD escaping, path/symlink & GHA validators
│   ├── utils.R                # Shared helpers, status algebra, formatting, RNG utils
│   ├── config.R               # .statci.yml strict YAML config & statci_audit_project()
│   ├── lint-parser.R          # .R / .qmd / .Rmd projection, XML AST construction, AST suppression parser
│   ├── lint-rules.R           # Rule S3 constructor, registry, and 17 built-in AST rules
│   ├── lint-engine.R          # stat_lint(), stat_lint_text(), stat_lint_dir(), S3 methods
│   ├── sarif.R                # export_statci_sarif() SARIF 2.1.0 JSON generator
│   ├── audit-estimator.R      # audit_estimator() parameter recovery, coverage, Type-I, convergence
│   ├── audit-sbc.R            # audit_sbc() Simulation-Based Calibration (Talts et al., 2018)
│   ├── audit-power.R          # audit_power_curve() empirical power monotonicity & MDE
│   ├── audit-parallel-rng.R   # check_parallel_rng() and statci_parallel_apply() L'Ecuyer streams
│   ├── audit-manuscript.R     # audit_manuscript() prose/table/LaTeX parser & internal arithmetic
│   ├── audit-sensitivity.R    # audit_sensitivity() winsorization, LOO/LOCO, missingness, bootstrap
│   ├── report.R               # statci_report(), statci_check_gate(), render_statci_report()
│   └── github-action.R        # use_statci_github_action() hardened CI workflow generator
├── tests/testthat/            # Comprehensive edition-3 unit & integration tests
└── vignettes/                 # Executable end-to-end documentation vignettes
```

### Unified Status Algebra

Every verification subsystem emits structured S3 objects whose overall status belongs to a three-level ordered lattice:

$$\text{pass} < \text{warn} < \text{fail}$$

Across components and unified reports (`statci_report()`), statuses compose monotonically via $\max$: any single `fail` elevates the parent section and overall report to `fail`; otherwise any `warn` elevates to `warn`; otherwise `pass`. CI quality gates (`statci_check_gate()`) translate this lattice directly into deterministic pass/error behavior for GitHub Actions.

---

## 2. Pillar 1: AST-Based Statistical Linter

### 2.1 Parsing Strategy for `.R`, `.qmd`, and `.Rmd`

Rather than applying fragile regexes to raw text, `statci` parses code into full R parse trees via `base::parse(keep.source = TRUE)`, converts the parse table to XML via `xmlparsedata::xml_parse_data()`, and queries syntactic structure with XPath (`xml2::xml_find_all()`).

For `.qmd` and `.Rmd` documents, `extract_r_chunks()` identifies fenced R code chunks (`` ```{r ...} `` / `` ```{R ...} ``) and builds a **blank-line-preserving document projection**:

1. Every non-R-chunk line (YAML frontmatter, markdown prose, fence delimiters, non-R chunks) is replaced with an empty line `""`.
2. Code lines inside R chunks stay at their **exact 1-based original file line numbers**.
3. A parallel line-to-chunk map records the chunk label (`{r label}` or Quarto `#| label: ...`) for each code line.

This design solves two classic notebook-linting pitfalls simultaneously:
- **Zero line-offset drift:** `xmlparsedata` `line1`/`col1` attributes match original `.qmd`/`.Rmd` line numbers directly.
- **Cross-chunk scope awareness:** A `set.seed(42)` in an early setup chunk properly satisfies downstream stochastic calls in later chunks, while a `scale(df)` in chunk 1 before a train/test split in chunk 2 is caught as cross-chunk data leakage.

If the combined document projection fails to parse (e.g., an intentional syntax error in one chunk), the parser gracefully falls back to per-chunk parsing with exact line offsets so valid chunks are still linted and the invalid chunk yields a `parse_error` diagnostic.

### 2.2 Inline Suppressions

Authors can silence intentional exceptions using standard line comments:
- `# statci-ignore` — suppresses all rules on that line.
- `# statci-ignore: rule_a, rule_b` — suppresses specific rule IDs on that line.
- `# statci-ignore-next-line` / `# statci-ignore-next-line: rule_a` — suppresses the immediately following line.

### 2.3 Extensible Rule Engine & Built-in Rules

Rules are `statci_rule` S3 objects stored in a package registry environment (`.statci_rule_registry`). Users inspect rules via `list_stat_rules()`, register custom AST rules via `register_stat_rule(new_stat_rule(...))`, and filter rules per invocation via `rules` / `exclude_rules`.

`statci` ships **14 built-in statistical & methodological lint rules**:

| # | Rule ID | Severity | Category | Methodological Hazard Caught |
|---|---------|----------|----------|------------------------------|
| 1 | `unseeded_randomness` | error | reproducibility | Top-level stochastic calls (`rnorm`, `sample`, `boot`, `kmeans`, etc.) without prior `set.seed()`, `withr::with_seed()`, `RNGkind()`, or enclosing seed wrapper |
| 2 | `unsafe_parallel_rng` | error | parallel_rng | `mclapply`/`parLapply`/`foreach %dopar%`/`future_*` without L'Ecuyer-CMRG, `clusterSetRNGStream`, `%dorng%`/`doRNG`, or `future.seed` |
| 3 | `duplicate_parallel_seed` | error | parallel_rng | Constant `set.seed(42)` inside loops or parallel workers producing identical duplicate Monte Carlo draws |
| 4 | `preprocessing_before_split` | error | data_leakage | Global `scale()`, `prcomp()`, `preProcess()`, `mice()`, etc. executed on a dataset before train/test splitting |
| 5 | `uncentered_mlm_interaction` | warning | modeling | Fixed-effect interactions (`*`, `:`) in `lmer`/`glmer`/`lme`/`glmmTMB`/`brm` using raw predictors with no centering call, naming suffix, or prior centering definition |
| 6 | `silent_listwise_mismatch` | warning | modeling | Comparing models with differing predictor sets via `anova()`, `AIC()`, `BIC()`, or `lrtest()` on an unharmonized dataset without `na.omit`/`complete.cases`/`drop_na`/`na.action = na.fail` |
| 7 | `silent_factor_drop` | warning | modeling | `as.numeric()`/`as.integer()` applied directly to a factor variable (coercing level indices), or passing a `subset()`/`filter()` data frame to modeling/contrast functions without `droplevels()`/`fct_drop()` |
| 8 | `equality_on_float` | warning | numerics | Exact `==` / `!=` on floating-point literals or computed continuous expressions instead of `all.equal()`, `near()`, or tolerance checks |
| 9 | `unadjusted_multiple_testing` | warning | inference | Hypothesis tests (`t.test`, `cor.test`, `wilcox.test`, `lm` p-values, etc.) run inside loops/applies without `p.adjust()` or multiple-testing correction in scope |
| 10 | `missing_convergence_check` | warning | modeling | Iterative models (`lmer`, `glmer`, `optim`, `nlminb`, `glmmTMB`) fitted without programmatic AST checks for convergence or `isSingular()` |
| 11 | `global_seed_override_in_fn` | warning | reproducibility | Calling bare `set.seed()` inside reusable functions or `\(...)` lambdas without restoring `.Random.seed` via `on.exit()` or `withr::local_seed()`/`with_seed()` |
| 12 | `short_logical_literal` | warning | hygiene | Using mutable `T`/`F` symbols as logical literals/arguments instead of reserved `TRUE`/`FALSE` (ignoring user-bound numeric variables like `T <- 100`) |
| 13 | `cor_without_use_arg` | info | modeling | Calling `cor()` or `cov()` without an explicit `use =` missing-data policy |
| 14 | `absolute_path_in_script` | warning | reproducibility | Hardcoded machine-specific absolute paths (`/Users/...`, `/home/...`, `C:\...`, `~/...`) in file I/O or `setwd()` calls |

---

## 3. Pillar 2: Estimator Calibration & Fuzzing Harness

Deterministic unit tests cannot detect biased custom estimators, anti-conservative standard errors, broken MCMC/posterior samplers, or parallel RNG stream collisions. `statci` implements finite-sample probabilistic verification accounting explicitly for **Monte Carlo standard error (MCSE)**.

### 3.1 Parameter Recovery, Coverage, SE Ratio & Type-I Error (`audit_estimator()`)

Given a user `dgp` function, an `estimator` function (returning either a named list or a standard R model object `lm`/`glm`/`lmerMod`/`htest`), and `true_params`, `audit_estimator()` runs $M_{\text{total}}$ replications inside a locally scoped RNG state (`withr::local_seed()`), capturing errors, convergence failures, and singular fits cleanly. Let $M$ be the number of converged replications:

1. **Bias Check:**
   $$\widehat{\text{Bias}} = \bar{\hat\theta} - \theta, \qquad \text{MCSE}_{\text{bias}} = \frac{s_{\hat\theta}}{\sqrt{M}}, \qquad z_{\text{bias}} = \frac{|\widehat{\text{Bias}}|}{\text{MCSE}_{\text{bias}}}$$
   - `fail` if $z_{\text{bias}} > \tau_{\text{bias}}$ (default $3.0$) and $|\widehat{\text{Bias}}| > 10^{-8}$;
   - `warn` if $2.0 < z_{\text{bias}} \le \tau_{\text{bias}}$;
   - `pass` otherwise.

2. **Interval Coverage Calibration (Exact Clopper-Pearson Binomial Bounds):**
   For nominal level $1 - \alpha$ and $k = \sum_{m=1}^M \mathbf{1}(\theta \in [\hat L_m, \hat U_m])$, $\hat c = k/M$ with $\text{MCSE}_{\text{cov}} = \sqrt{\hat c(1-\hat c)/M}$. Exact two-sided binomial testing (`stats::binom.test(k, M, p = 1 - alpha)`) yields exact p-value $p_{\text{cov}}$ and Clopper-Pearson interval $[c_L, c_U]$:
   - `fail` if $p_{\text{cov}} < \alpha_{\text{mc}}$ (default `0.01`) and $|\hat c - (1-\alpha)| > 0.015$;
   - `warn` if $p_{\text{cov}} < 0.05$;
   - `pass` otherwise.

3. **Model SE vs. Empirical SE Ratio:**
   Let $R = \overline{\widehat{\text{SE}}} / s_{\hat\theta}$ with asymptotic MCSE $\text{MCSE}_R \approx R / \sqrt{2(M-1)}$. Using configurable target bounds $[R_{\min}, R_{\max}]$ (default `[0.85, 1.15]`):
   - `fail` if the 95% MC interval $[R - 1.96\,\text{MCSE}_R,\, R + 1.96\,\text{MCSE}_R]$ lies completely outside $[R_{\min}, R_{\max}]$;
   - `warn` if the point ratio $R$ lies outside $[R_{\min}, R_{\max}]$ and $|R - 1| / \text{MCSE}_R > 2.0$;
   - `pass` otherwise.

4. **Type-I Error Calibration (Null DGP or Null Parameters):**
   Under `null_dgp` (or any parameter where $\theta_j = 0$), let $k_0 = \sum_{m=1}^{M_0} \mathbf{1}(p_m \le \alpha)$ and $\hat\alpha = k_0 / M_0$. Using `stats::binom.test(k_0, M_0, p = alpha)`:
   - `fail` if $p_{\text{binom}} < \alpha_{\text{mc}}$ and ($\hat\alpha > \alpha$ or $\hat\alpha < \alpha/4$);
   - `warn` if $p_{\text{binom}} < 0.05$;
   - `pass` otherwise.

5. **Convergence & Singularity Auditing:**
   Tracks error rate, non-convergence rate, and singular-fit rate (`lme4::isSingular()` or returned `singular = TRUE`) against configurable thresholds (`max_fail_rate`, `warn_fail_rate`, `max_singular_rate`).

### 3.2 Simulation-Based Calibration (`audit_sbc()`)

Implements Talts et al. (2018): for $s = 1, \dots, S$, draws $\tilde\theta_s \sim p(\theta)$, simulates $\tilde y_s \sim p(y \mid \tilde\theta_s)$, obtains $L$ posterior draws $\{\theta_s^{(l)}\}_{l=1}^L$, and computes rank statistic:
$$r_s = \sum_{l=1}^L \mathbf{1}\!\left(\theta_s^{(l)} < \tilde\theta_s\right) \in \{0, 1, \dots, L\}$$
(with randomized tie-breaking for discrete ties). Under a calibrated posterior sampler, $r_s \sim \text{DiscreteUniform}\{0, \dots, L\}$. `audit_sbc()` bins ranks into $B$ equal-probability bins and evaluates uniformity via both Pearson's $\chi^2$ test (`chisq.test`) and a Kolmogorov-Smirnov test on $u_s = (r_s + 0.5)/(L + 1)$, alongside central 50% and 90% posterior interval coverage.

### 3.3 Parallel RNG Invariance (`check_parallel_rng()` & `statci_parallel_apply()`)

A subtle R parallelization trap is that standard `parallel::mclapply(..., mc.set.seed = TRUE)` or `clusterSetRNGStream(cl)` assigns one stream **per worker**, so running with `cores = 1` vs `cores = 2` vs `cores = 4` produces different Monte Carlo realizations!

- `statci_parallel_apply(X, FUN, cores, seed)` solves this by pre-generating one independent `"L'Ecuyer-CMRG"` stream **per task** $i \in \{1, \dots, |X|\}$ via `parallel::nextRNGStream()`, assigning `.Random.seed` inside each task before invoking `FUN(X[[i]])`. Output is mathematically bit-for-bit identical across any number of cores on macOS, Linux, and Windows (using PSOCK clusters with strict `on.exit(stopCluster(cl))` hygiene).
- `check_parallel_rng()` executes a user-supplied parallel `runner(cores, seed)` (or `fun` via `statci_parallel_apply`) across core counts (default `c(1L, 2L)`, honoring CRAN's 2-core limit) and verifies exact numerical equivalence (`identical()` / `all.equal(tolerance = 0)`), failing immediately on any core-count discrepancy.

---

## 4. Pillar 3: Manuscript-to-Code Consistency Auditor (`audit_manuscript()`)

### 4.1 Document Parsing & Tagging Grammar

`audit_manuscript()` strips YAML frontmatter and fenced code blocks (preserving exact line numbers) and extracts statistical claims from prose in two modes:

1. **Explicitly Tagged Claims (Zero-Ambiguity Binding):**
   - HTML comment anchor preceding text: `<!-- statci: model=fit1 term=x1 -->`
   - Inline Quarto span: `[b = 0.42, SE = 0.11, p = .003]{.statci model="fit1" term="x1"}`
   - Model-level anchor (for $N$, $R^2$, AIC, BIC): `<!-- statci: model=fit1 -->`

2. **Automatic Prose Extraction (Untagged Matching):**
   Recognizes standard APA / statistical typography (including markdown italics `*b*`, `*p*`, LaTeX inline math `$b = 0.42$`, Unicode minus `−`, and leading-dot decimals `.003`):
   - Point estimates (`b`, `B`, `beta`, `β`, `est`, `estimate`, `OR`, `coef`)
   - Standard errors (`SE`, `S.E.`)
   - Confidence intervals (`95% CI [L, U]`, `CI (L, U)`, `95% CI: L to U`)
   - p-values (`p = .003`, `p < .001`, `p <= .05`, `p > .05`)
   - Sample sizes (`N = 250`, `n = 250`)
   - Test & fit statistics (`t(df) = ...`, `z = ...`, `r = ...`, `R^2 = ...`, `AIC = ...`)

### 4.2 Uniform Model Artifact Extraction

Accepts a named list of R models, a single model, an `.rds` file, or a directory of `.rds` files. Supports `lm`, `glm`, `lmerMod`/`glmerMod`/`lmerModLmerTest`, `htest`, and tidy `data.frame`/`tibble` tables.

### 4.3 Rounding-Aware Verification Algebra

For any numeric claim reported with $d$ decimal places (e.g., `0.42` has $d = 2$):
- Let $\delta = 0.5 \times 10^{-d} + \epsilon_{\text{extra}}$ be the rounding half-unit.
- **Sign Flip (`fail`):** $\text{sign}(\text{reported}) \ne \text{sign}(\text{actual})$ while $|\text{actual}| > \delta$ and $\text{reported} \ne 0$.
- **Exact Rounding Match (`pass`):** $|\text{actual} - \text{reported}| \le \delta + 10^{-9}$.
- **Rounding Discrepancy (`warn`):** $\delta < |\text{actual} - \text{reported}| \le 1.5 \times 10^{-d} + \epsilon_{\text{extra}}$ (off by 1 unit in the final decimal place, e.g., truncation instead of half-to-even rounding).
- **Value Mismatch (`fail`):** $|\text{actual} - \text{reported}| > 1.5 \times 10^{-d} + \epsilon_{\text{extra}}$.
- **Inequality p-values (`p < c` / `p <= c` / `p > c`):** Verified directly as exact boolean inequalities against the model's unrounded `p.value`.
- **Sample size (`N`):** Exact integer equality against `nobs(model)`.

---

## 5. Pillar 4: Adversarial Sensitivity & Robustness Battery (`audit_sensitivity()`)

Given `data`, `fit_fn(df)`, and `extract_fn(fit)`, `audit_sensitivity()` automatically executes four orthogonal perturbation suites around the baseline fit:

1. **Outlier Winsorization:** Clamps continuous non-binary, non-cluster numeric predictors/outcomes to $[q_p, q_{1-p}]$ (default $p \in \{0.01, 0.05\}$).
2. **Leave-One-Cluster-Out (LOCO) / Leave-One-Out (LOO) Influence:** Sequentially omits each cluster (`cluster_col`) or observation (capped at `max_loo_units`, prioritizing high-leverage rows for linear models when subsampling), reporting maximum shift, most influential unit, jackknife SE, and any single-unit sign reversal.
3. **Missingness & Imputation Stress Tests:** Evaluates observed-data missingness handling (if `NA`s already exist) plus controlled MCAR perturbation at configured rates (default 5% and 10%) contrasting complete-case deletion against median/mean imputation.
4. **Nonparametric Bootstrap Stability:** Performs cluster bootstrap (resampling whole clusters when `cluster_col` is given) or row bootstrap, reporting bootstrap mean, SE, percentile CI, sign consistency rate, and refit convergence rate.

Each perturbation records baseline estimate, perturbed estimate, absolute shift, relative `% change`, sign-flip indicator, and pass/warn/fail classification.

---

## 6. Pillar 5: Unified Audit Report & GitHub Actions Integration

- **`statci_report()`** aggregates lint, estimator calibration, SBC, parallel RNG, manuscript, and sensitivity objects into a single `statci_report` S3 object with unified summary metrics and session metadata.
- **`statci_check_gate()`** enforces CI exit gates (`fail_on = "fail"` or `"warn"`), throwing a structured error in non-interactive CI runs when thresholds are breached.
- **`render_statci_report()`** generates:
  - Self-contained, offline-capable HTML5 audit dashboards (safely escaped with `htmltools`),
  - GitHub-Flavored Markdown reports suitable for `$GITHUB_STEP_SUMMARY` and PR reviews,
  - Machine-readable JSON artifacts (via `jsonlite`).
- **`use_statci_github_action()`** writes a ready-to-run `.github/workflows/statci.yaml` workflow installing dependencies, running `statci` audits, publishing Markdown summaries to GitHub Actions, and uploading HTML reports as workflow artifacts.
