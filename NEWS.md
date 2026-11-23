# statci 0.2.0

## Security Hardening
- Added centralized security, escaping, and validation module (`R/security.R`) and documented threat model and mitigations in `SECURITY.md`.
- Hardened `use_statci_github_action()` against YAML, shell, and R code injection: strictly validates `branches` (new parameter), `r_version`, `lint_paths`, `manuscript`, `models_path`, `audit_script`, `report_dir`, and `file` containment; eliminates untrusted string interpolation into `run:` blocks via workflow `env:` variables; pins GitHub Actions versions and disables checkout credential persistence (`persist-credentials: false`).
- Hardened `render_statci_report()` with strict HTML/attribute escaping (`html_escape_strict()`), a Content-Security-Policy (`default-src 'none'; style-src 'unsafe-inline'`) `<meta>` header, and Markdown table HTML neutralization (`md_escape_cell()`).
- Hardened `stat_lint()`, `stat_lint_dir()`, and `audit_manuscript()` against broken symlinks, control characters, and directory-escaping symlinks.
- Hardened `audit_manuscript()` regex engines against ReDoS catastrophic backtracking with strictly linear-time bounded patterns and per-line length guards.

## AST Linter Improvements
- Added built-in rule #14 (`duplicate_parallel_seed`) catching constant `set.seed()` calls inside loops and parallel workers (now 14 built-in AST rules).
- Replaced raw-line suppression scanning with AST `<COMMENT>`-backed `parse_suppressions_ast()` so string literals containing `"# statci-ignore"` cannot evade linting.
- Added full R 4.1+ shorthand lambda (`\(x) ...`, `<OP-LAMBDA>`) support in `ast_is_inside_function()` and `global_seed_override_in_fn`.
- Hardened `missing_convergence_check`, `global_seed_override_in_fn`, `silent_listwise_mismatch`, and `unadjusted_multiple_testing` to use pure AST queries immune to comments and string literals.
- Handled non-evaluated Quarto/RMarkdown (`eval: false` / `eval = FALSE`) pseudocode chunks gracefully without spurious parse errors.

## Calibration, SBC, Sensitivity & Manuscript Accuracy
- Added full scientific notation parsing (`-3.5e-2`, `1.2e-4`, `p < 1e-4`, `1.2 × 10^-4`, `1.2 \times 10^{-4}`), scientific rounding-unit tolerance, en-dash CI range separator handling (`[-0.15–0.42]`), leading-dot negative values (`-.42`), `-0.00`, and IEEE-754 half-to-even boundary tolerance in `audit_manuscript()`.
- Fixed crash in `audit_estimator()` when 100% of replications fail to converge, counted all-`NaN`/`Inf` estimator outputs as non-convergence, and handled zero-variance constant estimators gracefully.
- Hardened `audit_sbc()` to reject/count `NaN`/`Inf` draws cleanly, return a structured `"fail"` audit when fewer than 10 valid replications complete, and guard small-$B$ / high-bin ratios.
- Hardened `audit_sensitivity()` against `NaN`/`Inf` refits, all-`NA` columns, single-cluster datasets, and unbalanced cluster sizes in LOCO jackknife and cluster bootstrap.

# statci 0.1.0

- Initial CRAN-grade release of `statci`: 13-rule AST statistical linter for `.R`/`.qmd`/`.Rmd`, estimator calibration & SBC fuzzing harness, parallel RNG invariance checker, manuscript-to-code consistency auditor, adversarial sensitivity battery, standalone HTML/Markdown/JSON report renderer, and GitHub Actions workflow generator.
