# statci 0.4.0

## New Features
- Expanded the built-in AST methodological linter to **20 zero-false-positive rules**, adding 3 high-signal data-integrity rules:
  - `unverified_merge_cardinality`: flags `dplyr::*_join()` and base `merge()` calls lacking `relationship =` or `nrow()` guards against silent many-to-many row duplication.
  - `missing_drop_false_matrix`: flags 2D matrix/array subsetting (`X[, cols]`, `X[idx, ]`) lacking `drop = FALSE` that silently collapses 1-row/1-column matrices into atomic vectors.
  - `implicit_na_rm_masking`: flags unconditional `na.rm = TRUE` in statistical summaries without nearby missingness inspection (`is.na()`, `anyNA()`, `complete.cases()`, `na.omit()`, `drop_na()`).
- Added **Git-diff-aware incremental pull-request linting (`stat_lint_diff()`)** with unified diff parsing (`diff_text` or safe read-only `git diff` across working tree, `--cached`, or two-ref ranges), strict Git ref option-injection validation (`validate_git_ref()`), strict path confinement, and full-file AST context filtering to changed lines only.
- Added **Specification-Curve / Multiverse Robustness Auditing (`audit_multiverse()`)** evaluating modeling pipelines across the full Cartesian grid of analytical decision forks, returning specification-level estimates, CIs, p-values, sign consistency, median/IQR, per-fork variance decomposition (`eta_squared`), S3 `print()`, `summary()`, `as_tibble()`, two-panel `plot()` (faceted `ggplot2` with base-R fallback), and full integration into `statci_report()` and `statci_audit_project()`.
- Added a fifth executable vignette (`vignettes/multiverse-and-pr-diffs.Rmd`) and updated `inst/guides/statci-guide.Rmd` and the `pkgdown` reference site.

## Bug Fixes & Coverage Hardening
- Conducted full `covr` branch audit driving line coverage from 89.45% to **93.18%** across 459 passing tests.
- Fixed single-line `\begin{verbatim}...\end{verbatim}` handling in `audit_manuscript()` where a single-line verbatim block previously left verbatim state active for the rest of a `.tex` document.
- Fixed `.R` syntax error column extraction in `build_parse_context()` to capture exact reported columns instead of defaulting to column 1.
- Fixed missing HTML and Markdown rendering of `manuscript$internal_checks` in `render_statci_report()`.
- Hardened `system2()` argument quoting (`shQuote()`) in `stat_lint_diff()` for repository paths containing spaces, and removed dead internal helpers.

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
