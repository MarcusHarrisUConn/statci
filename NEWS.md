# statci 0.3.0

## New Features & Workflow Orchestration
- Added declarative `.statci.yml` project configuration (`read_statci_config()`, strictly parsed with `eval.expr = FALSE` and schema validation) and one-call project orchestration (`statci_audit_project()`) supporting rule filtering, severity overrides, manuscript auditing, multi-format report output, and CI gate enforcement.
- Added 3 new high-signal, zero-false-positive AST lint rules bringing the built-in catalog to **17 rules**: `silent_na_coercion`, `stepwise_selection_unvalidated`, and `in_sample_prediction_eval`.
- Upgraded `audit_manuscript()` and `audit_manuscript_text()` to audit Markdown/Quarto pipe tables, LaTeX `.tex` manuscripts and `tabular` tables (`% statci:` comment anchors), APA `F(df1, df2)` and `chi2(df)` statistics, and interval-arithmetic **internal consistency checks** (`t` vs. `b / SE` and `95% CI` midpoint/width vs. `b` and `SE`, even when `models = NULL`).
- Added `audit_power_curve()` (with `print()`, `as_tibble()`, `plot()`, and unified `statci_report()` integration) auditing empirical power monotonicity, null Type-I error calibration, target power achievement with exact Clopper-Pearson bounds, and minimum detectable effect (`mde`).
- Added `export_statci_sarif()` emitting valid SARIF 2.1.0 JSON for GitHub Code Scanning pull-request annotations, plus optional `upload_sarif = TRUE` integration in `use_statci_github_action()`.

## CRAN Submission Readiness & Documentation
- Expanded to 4 executable vignettes: `getting-started`, `calibration-and-robustness`, `manuscript-auditing`, and `github-actions-ci`.
- Added `inst/CITATION`, example scripts/manuscripts/configs in `inst/examples/`, PDF Field Guide source in `inst/guides/statci-guide.Rmd`, `inst/WORDLIST` for zero-misspelling `spelling::spell_check_package()`, `cran-comments.md`, `_pkgdown.yml`, and GitHub Actions workflows for `R CMD check` and `pkgdown`.
- Audited `DESCRIPTION` (<= 65-character Title, formal Talts et al. 2018 and Clopper & Pearson 1934 DOIs), every exported `.Rd` manual page, and strict 2-core / `tempdir()` CRAN hygiene.

# statci 0.2.0

## Security Hardening
- Added centralized security, escaping, and validation module (`R/security.R`) and documented threat model and mitigations in `SECURITY.md`.
- Hardened `use_statci_github_action()` against YAML, shell, and R code injection via strict allowlist validators, workflow `env:` indirection, pinned action versions, and `persist-credentials: false`.
- Hardened `render_statci_report()` with strict HTML/attribute escaping (`html_escape_strict()`), a Content-Security-Policy `<meta>` header, and Markdown table HTML neutralization (`md_escape_cell()`).
- Hardened file/directory scanning against broken symlinks, control bytes, and symlink traversal, and hardened `audit_manuscript()` against ReDoS with linear-time bounded regexes.

## AST Linter & Accuracy Improvements
- Added built-in rule #14 (`duplicate_parallel_seed`), AST `<COMMENT>`-backed suppression parsing, R 4.1+ `\(x)` lambda support, and pure AST convergence/seed-restore inspection.
- Added scientific notation parsing and rounding-unit tolerance, en-dash CI range handling, `-0.00`, and IEEE-754 half-to-even boundary rounding in `audit_manuscript()`.
- Hardened `audit_estimator()`, `audit_sbc()`, and `audit_sensitivity()` for 100% non-convergence, `NaN`/`Inf` outputs, zero-variance estimators, small-bin SBC guards, and unbalanced/single-cluster LOCO datasets.

# statci 0.1.0

- Initial CRAN-grade release of `statci`: 13-rule AST statistical linter for `.R`/`.qmd`/`.Rmd`, estimator calibration & SBC fuzzing harness, parallel RNG invariance checker, manuscript-to-code consistency auditor, adversarial sensitivity battery, standalone HTML/Markdown/JSON report renderer, and GitHub Actions workflow generator.
