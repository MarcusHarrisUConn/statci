# Package index

## AST Statistical Linter, PR Diff Linting & SARIF Export

Parse R scripts and Quarto/RMarkdown chunks into XML abstract syntax
trees, evaluate 20 built-in statistical lint rules or custom rules,
incrementally lint Git pull-request diffs, and export SARIF 2.1.0 JSON
for GitHub Code Scanning.

- [`stat_lint()`](https://marcusharrisphd.com/statci/reference/stat_lint.md)
  : Lint an R script or Quarto/RMarkdown document for statistical
  hazards

- [`stat_lint_text()`](https://marcusharrisphd.com/statci/reference/stat_lint_text.md)
  : Lint R or Quarto/RMarkdown source code supplied as a string

- [`stat_lint_dir()`](https://marcusharrisphd.com/statci/reference/stat_lint_dir.md)
  : Lint all R, Quarto, and RMarkdown files in one or more directories

- [`stat_lint_diff()`](https://marcusharrisphd.com/statci/reference/stat_lint_diff.md)
  : Incrementally lint only added or modified lines in a Git diff / pull
  request

- [`list_stat_rules()`](https://marcusharrisphd.com/statci/reference/list_stat_rules.md)
  : List available statistical lint rules

- [`new_stat_rule()`](https://marcusharrisphd.com/statci/reference/new_stat_rule.md)
  :

  Construct a custom `statci` lint rule

- [`register_stat_rule()`](https://marcusharrisphd.com/statci/reference/register_stat_rule.md)
  : Register a custom statistical lint rule

- [`unregister_stat_rule()`](https://marcusharrisphd.com/statci/reference/unregister_stat_rule.md)
  : Remove a rule from the active registry

- [`reset_stat_rules()`](https://marcusharrisphd.com/statci/reference/reset_stat_rules.md)
  :

  Reset the rule registry to the built-in `statci` rule set

- [`export_statci_sarif()`](https://marcusharrisphd.com/statci/reference/export_statci_sarif.md)
  :

  Export `statci` AST lint results to SARIF 2.1.0 JSON

## Estimator Calibration, SBC, Power Curves & Parallel RNG

Verify bias, RMSE, exact Clopper-Pearson interval coverage, Type-I
error, Simulation-Based Calibration (SBC), power monotonicity, and
core-count-invariant L’Ecuyer-CMRG parallel RNG streams.

- [`audit_estimator()`](https://marcusharrisphd.com/statci/reference/audit_estimator.md)
  : Audit a statistical estimator via Monte Carlo calibration and
  fuzzing
- [`audit_sbc()`](https://marcusharrisphd.com/statci/reference/audit_sbc.md)
  : Audit posterior calibration via Simulation-Based Calibration (SBC)
- [`plot(`*`<statci_sbc>`*`)`](https://marcusharrisphd.com/statci/reference/plot.statci_sbc.md)
  : Plot SBC rank histogram with expected uniformity band
- [`audit_power_curve()`](https://marcusharrisphd.com/statci/reference/audit_power_curve.md)
  : Audit an estimator's empirical power curve across an effect-size
  grid
- [`plot(`*`<statci_power_curve>`*`)`](https://marcusharrisphd.com/statci/reference/plot.statci_power_curve.md)
  : Plot empirical power curve with exact Clopper-Pearson confidence
  band
- [`check_parallel_rng()`](https://marcusharrisphd.com/statci/reference/check_parallel_rng.md)
  : Audit parallel RNG invariance across core counts
- [`statci_parallel_apply()`](https://marcusharrisphd.com/statci/reference/statci_parallel_apply.md)
  : Execute a stochastic map with core-count-invariant L'Ecuyer-CMRG RNG
  streams

## Manuscript-to-Code Consistency Auditor

Cross-check inline prose, Markdown/Quarto tables, LaTeX .tex tables, APA
statistics, and internal arithmetic against fitted R model objects.

- [`audit_manuscript()`](https://marcusharrisphd.com/statci/reference/audit_manuscript.md)
  : Audit a Quarto, RMarkdown, or LaTeX manuscript against saved R model
  objects
- [`audit_manuscript_text()`](https://marcusharrisphd.com/statci/reference/audit_manuscript_text.md)
  : Audit manuscript prose or tables supplied as an in-memory string

## Adversarial Sensitivity & Multiverse Specification Curves

Stress-test fitted modeling pipelines under outlier winsorization,
leave-one-cluster-out (LOCO) / leave-one-out influence, missingness
contrasts, nonparametric bootstrap resampling, and full Cartesian
multiverse specification curves.

- [`audit_sensitivity()`](https://marcusharrisphd.com/statci/reference/audit_sensitivity.md)
  : Run an adversarial sensitivity and robustness battery on a
  statistical pipeline
- [`plot(`*`<statci_sensitivity>`*`)`](https://marcusharrisphd.com/statci/reference/plot.statci_sensitivity.md)
  : Plot adversarial sensitivity shifts across scenarios
- [`audit_multiverse()`](https://marcusharrisphd.com/statci/reference/audit_multiverse.md)
  : Audit analytical robustness across a multiverse specification curve
- [`summary(`*`<statci_multiverse>`*`)`](https://marcusharrisphd.com/statci/reference/summary.statci_multiverse.md)
  : Summarize a multiverse specification-curve audit
- [`plot(`*`<statci_multiverse>`*`)`](https://marcusharrisphd.com/statci/reference/plot.statci_multiverse.md)
  : Plot a two-panel publication-ready specification curve and decision
  matrix

## Project Orchestration, Reports & GitHub Actions

Drive full project verification from .statci.yml, render standalone
HTML, Markdown, and JSON audit dashboards, enforce CI gates, and
generate hardened GitHub Actions workflows.

- [`read_statci_config()`](https://marcusharrisphd.com/statci/reference/read_statci_config.md)
  :

  Read and validate a `.statci.yml` project configuration file

- [`statci_audit_project()`](https://marcusharrisphd.com/statci/reference/statci_audit_project.md)
  :

  Run the full configured `statci` verification pipeline on a project

- [`statci_report()`](https://marcusharrisphd.com/statci/reference/statci_report.md)
  :

  Assemble a unified `statci` audit report

- [`render_statci_report()`](https://marcusharrisphd.com/statci/reference/render_statci_report.md)
  :

  Render a `statci_report` to standalone HTML, GitHub-Flavored Markdown,
  or JSON

- [`statci_check_gate()`](https://marcusharrisphd.com/statci/reference/statci_check_gate.md)
  :

  Enforce CI quality gates on a `statci_report`

- [`use_statci_github_action()`](https://marcusharrisphd.com/statci/reference/use_statci_github_action.md)
  :

  Generate a GitHub Actions workflow for automated `statci` verification

## Package Overview & Re-exports

- [`statci`](https://marcusharrisphd.com/statci/reference/statci-package.md)
  [`statci-package`](https://marcusharrisphd.com/statci/reference/statci-package.md)
  : statci: Statistical CI/CD, Calibration Fuzzing, and Manuscript
  Auditing
- [`reexports`](https://marcusharrisphd.com/statci/reference/reexports.md)
  [`as_tibble`](https://marcusharrisphd.com/statci/reference/reexports.md)
  : Objects exported from other packages
