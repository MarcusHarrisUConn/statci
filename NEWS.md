# statci 0.5.1

## Cross-Platform CI & Windows Path Hardening
- Updated `.github/workflows/R-CMD-check.yaml`, `.github/workflows/pkgdown.yaml`, and `use_statci_github_action()` (`R/github-action.R`) to floating major tags (`actions/checkout@v4`, `r-lib/actions/setup-pandoc@v2`, `r-lib/actions/setup-r@v2`, `r-lib/actions/setup-r-dependencies@v2`, `r-lib/actions/check-r-package@v2`, and `actions/upload-artifact@v4`), resolving the Ubuntu `sudo -E` / `R_LIB_FOR_PAK` failure in older pinned `r-lib/actions@v2.11.2` while preserving `persist-credentials: false` and all workflow security hardening.
- Hardened Windows path canonicalization and containment across `R/security.R` (`validate_contained_dest()`, `is_safe_contained_file()`, `is_path_within_root()`), `R/sarif.R`, and `R/lint-diff.R`:
  - Consistently expands Windows 8.3 short paths (`RUNNER~1`) via `normalizePath(..., winslash = "/", mustWork = TRUE)` along existing ancestors instead of comparing against `mustWork = FALSE`.
  - Applies case-insensitive root-prefix checks on Windows (`.Platform$OS.type == "windows"`).
  - Strips CRLF `\r` endings and preserves UTF-8 encoding in `stat_lint_diff()`, and ensures all regression tests use NTFS-compatible filenames and gracefully skip symlink assertions when the OS denies unprivileged symlinks.

# statci 0.5.0

## Security Hardening & Independent Peer Red-Team Remediation
- Remediated all 20 findings (6 High, 10 Medium, 4 Low) from an independent peer adversarial audit and added 115 permanent regression test expectations in `tests/testthat/test-peer-redteam.R` (574 total passing expectations, 93.25% line coverage):
  - **Symlink & path containment (`R/security.R`, `R/config.R`):** Rewrote `validate_contained_dest()` to walk every path segment from canonicalized `root_dir`, rejecting symlinked ancestors pointing outside the project root and dangling symlinks (securing `use_statci_github_action()` and `statci_audit_project(report.output_dir)`). Enforced canonical containment on every `lint.paths` entry in `statci_audit_project()` to prevent external file reads and code-snippet leakage.
  - **Diff parser & safe filenames (`R/lint-diff.R`, `R/security.R`):** Rewrote `parse_unified_diff()` with exact old/new hunk line counters (`rem_old`, `rem_new`) so added `++ ` (`+++ ...`) and deleted `-- ` (`--- ...`) lines inside active hunks never prematurely terminate hunk parsing. Passed `-c core.quotepath=false` to `git diff`, decoded octal C-quoted UTF-8 paths in `diff_text`, guarded `b/` stripping for plain `diff -u`, and added `validate_safe_diff_rel_path()` supporting spaces, parentheses, and UTF-8 filenames safely.
  - **Report & SARIF hardening (`R/report.R`, `R/sarif.R`):** Escaped `ms$parameter` via `md_escape_cell()` (neutralizing backticks and HTML tags) in Markdown multiverse reports. Hardened `export_statci_sarif()` to emit positive integer `startLine`/`startColumn` (`>= 1L`, never `"NA"`), RFC-3986 percent-encode URI path segments (`#`, `?`, `%`, spaces, UTF-8), and never leak absolute host paths when `base_uri = NULL`.
  - **YAML config validation (`R/config.R`):** Rejected top-level and section YAML sequences (unnamed lists), non-finite `manuscript.tol_extra` (`.inf`/`.na`), and `NA` booleans (`warn_rounding: .na`, `check_internal: .na`).

## AST Linter & Auditor Accuracy Fixes
- **AST rules (`R/lint-rules.R`, `R/lint-parser.R`):**
  - Eliminated false positives on positional out-of-sample `predict(fit, test_df)` in `in_sample_prediction_eval` and caught piped `fit |> predict()`, `fit |> fitted()`, and `df |> lm(y ~ x, data = _)`.
  - Eliminated false positives on scalar element lookups (`m[i, 1]`, `coef(summary(fit))[2, 4]`, `df[1, "col"]`) in `missing_drop_false_matrix` while catching single-row slices (`X[1, ]`) and dynamic slices (`X[, 1:p]`).
  - Closed pipe (`|>` / `%>%`) evasions in `silent_na_coercion`, `silent_factor_drop`, and `preprocessing_before_split` (`df2 <- df |> mutate(x = scale(x)); initial_split(df2)`).
  - Hardened `unseeded_randomness` (removed `RNGkind` from seed functions; added higher-order `lapply`/`sapply`/`vapply`/`apply`/`replicate`/`do.call`/`map*` detection), `equality_on_float` (top-level string/NULL literal check only), `short_logical_literal` (lexical function-scope binding tracking), `unverified_merge_cardinality` (strict symbol/pipe binding), `stepwise_selection_unvalidated` (token-boundary split matching), and `implicit_na_rm_masking` (expanded aggregators, positional `na.rm`, prior/matching-symbol guard scoping).
  - Updated Quarto/Rmd chunk extraction (`extract_r_chunks()`) to respect outer non-R documentation fences (```` ````markdown ````), support tilde-fenced `~~~{r}` chunks, and allow blank lines before `#| eval: false`.
- **Verification auditors (`R/audit-*.R`, `R/utils.R`):**
  - `audit_multiverse()`: rejected reserved/duplicate decision fork names, enforced `max_specs` before `expand.grid()`, validated finite `alpha`, preserved `NA` fork choices as `"<NA>"` in variance decomposition and plots, and aligned `most_influential_fork` with `eta_squared > 1e-12 || median_range > 1e-12`.
  - `audit_manuscript()`: respected reported CI confidence levels (`90%`, `95%`, `99%`) in internal arithmetic, parsed outer-pipe-free GFM tables, flagged impossible `p` outside `[0, 1]` and `R^2 > 1`, handled empty `.rds` model lists and regex metacharacters in model names, validated finite `tol_extra`, and ignored resampling prose (`B = 1000 bootstrap samples`).
  - `audit_power_curve()`: preserved `NA` in `list(reject = NA)`, rejected non-finite or out-of-`[0, 1]` p-values as failed replications, validated finite scalar arguments and unique `effect_grid`, and computed finite `mde` magnitude on negative-only grids.
  - `audit_sensitivity()`: handled collinear `lm` models with `NA` coefficients, safely filtered `NA` cluster IDs in LOCO, and preserved numeric/integer cluster column types in cluster bootstrap.
  - `audit_estimator()` & `audit_sbc()`: rejected non-finite `true_params`/`alpha`, failed degenerate zero-SE or all-`NA`-CI estimators when SE/CI fields are returned, and aborted on inconsistent posterior draw counts `L` across SBC replications.

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
