# NA

## Project 6: `statci` — Statistical CI/CD, Calibration Fuzzing & Manuscript Auditor

**Suggested GitHub Repo:** `statci`  
**GitHub One-Liner:** *Automated statistical CI/CD for R: static bug
linting, Simulation-Based Calibration fuzzing, manuscript-to-code
auditing, and adversarial robustness suites.*  
**Topics:** `r-package`, `ci-cd`, `statistical-verification`,
`simulation-based-calibration`, `quarto`, `static-analysis`,
`open-science`, `reproducibility`

### Project Statement

Software engineers rely on continuous integration, static analyzers, and
fuzz tests to catch silent bugs before shipping code—yet statistical
analyses and quantitative methods packages often reach peer review with
undetected flaws: unseeded stochastic steps, parallel RNG seed
collisions, silent factor-level drops, uncentered Level-1 predictors
causing contextual confounding, biased custom estimators, or copy-pasted
numbers in manuscripts that no longer match the underlying model
objects. As AI accelerates code generation and models grow more
computationally complex, manual spot-checking is no longer enough.

`statci` brings **automated CI/CD and adversarial verification** to
statistical R code and Quarto/RMarkdown manuscripts. It combines an
AST-based statistical linter that catches silent methodological traps,
an automated calibration and fuzzing harness (Simulation-Based
Calibration, Type-I error checks, parallel RNG invariance tests,
non-convergence auditing), a **manuscript-to-code consistency auditor**
that verifies reported numbers against saved R model objects, and an
adversarial sensitivity battery—all packaged with ready-to-use GitHub
Actions templates and clean pass/warn/fail HTML audit reports.

### Why Existing R Tools Aren’t Enough

- `lintr` checks code *style* (spacing, line length), not statistical or
  methodological correctness (centering traps, parallel RNG safety, data
  leakage, unseeded stochasticity).
- `statcheck` only checks internal p-value arithmetic in APA strings via
  regex—it cannot inspect your R code, verify whether numbers match
  actual fitted model objects, or stress-test your estimator via
  simulation.
- `testthat` tests deterministic code logic, not probabilistic
  statistical properties like Type-I error calibration, coverage, or
  estimator stability under data perturbations.

### Core Architectural Pillars

1.  **AST-Based Statistical Linter:** Parses R scripts and Quarto/Rmd
    code chunks to flag methodological hazards—unseeded stochastic
    calls, unsafe parallel RNG (`mclapply`/`foreach` without L’Ecuyer
    streams), missing-data silent listwise mismatches, uncentered
    predictors in multilevel interaction terms, and
    preprocessing-before-split data leakage.
2.  **Estimator Calibration & Fuzzing Harness:** Given any user-supplied
    data-generating function and estimator, automatically runs
    parameter-recovery checks, Simulation-Based Calibration (SBC) /
    Type-I error calibration tests, multi-core RNG invariance checks,
    and non-convergence/singular-fit audits.
3.  **Manuscript-to-Code Consistency Auditor:** Parses Quarto/Rmd
    manuscripts alongside saved R model artifacts to cross-check inline
    estimates, standard errors, p-values, CIs, and sample sizes—catching
    stale copy-paste discrepancies automatically.
4.  **Adversarial Sensitivity & Robustness Battery:** Automatically
    re-runs fitted pipelines under structured stress tests (outlier
    winsorizing, cluster leave-one-out, alternative missingness
    handling, bootstrap perturbations), optionally inside an isolated
    container, and produces a unified pass/warn/fail audit report plus a
    GitHub Actions workflow template.

### v1 Definition of Done

- Extensible AST rule engine shipping 10+ well-tested statistical lint
  rules for R scripts and Quarto documents.
- Working estimator verification suite (Type-I error, coverage/bias
  recovery, parallel RNG invariance, convergence auditing) with clear
  statistical pass/fail thresholds accounting for Monte Carlo error.
- Working Quarto-to-R-object numerical cross-checker and adversarial
  sensitivity runner.
- HTML/Markdown audit report generator, GitHub Actions CI generator
  ([`use_statci_github_action()`](https://marcusharrisphd.com/statci/reference/use_statci_github_action.md)),
  exhaustive `testthat` suite, vignettes, and clean `R CMD check`.

### New-Conversation Handoff Block

``` text
We are starting a new flagship, CRAN-grade R package in this repository: `statci` — an automated statistical CI/CD, calibration fuzzing, manuscript-to-code auditing, and adversarial robustness suite for R.

Project mission: Bring software-engineering-grade continuous verification to statistical R pipelines, methods packages, and Quarto manuscripts:
1. AST-based statistical linter for R scripts and Quarto/Rmd chunks detecting real methodological hazards (unseeded randomness, unsafe parallel RNG, uncentered predictors in multilevel interactions, data leakage, silent factor/missingness drops).
2. Automated estimator verification & fuzzing harness: given a DGP + fitting function, run Type-I error calibration, coverage/bias recovery, parallel RNG invariance across core counts, and non-convergence/singular-fit auditing (accounting for Monte Carlo SE).
3. Manuscript-to-code consistency auditor cross-checking inline numbers, sample sizes, CIs, and p-values in Quarto/Rmd manuscripts against actual R model objects.
4. Adversarial sensitivity battery (outlier perturbations, cluster leave-one-out, missingness/bootstrap checks) outputting a clean pass/warn/fail HTML/Markdown audit report + a GitHub Actions workflow template (`use_statci_github_action()`).

Please start by inspecting this repo, designing the rule engine and verification architecture (`ARCHITECTURE.md`) and phased build plan, and then we will build iteratively to full `testthat` coverage, vignettes, and a clean `R CMD check`.
```
