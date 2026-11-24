# statci: Statistical CI/CD, Calibration Fuzzing, and Manuscript Auditing

Automated continuous integration and verification tools for statistical
R pipelines, methods packages, and Quarto/RMarkdown/LaTeX manuscripts.

## Verification Pillars

- **AST Statistical Linter & PR Diff Linting**:
  [`stat_lint()`](https://marcusharrisphd.com/statci/reference/stat_lint.md),
  [`stat_lint_text()`](https://marcusharrisphd.com/statci/reference/stat_lint_text.md),
  [`stat_lint_dir()`](https://marcusharrisphd.com/statci/reference/stat_lint_dir.md),
  [`stat_lint_diff()`](https://marcusharrisphd.com/statci/reference/stat_lint_diff.md),
  [`list_stat_rules()`](https://marcusharrisphd.com/statci/reference/list_stat_rules.md),
  [`new_stat_rule()`](https://marcusharrisphd.com/statci/reference/new_stat_rule.md),
  [`register_stat_rule()`](https://marcusharrisphd.com/statci/reference/register_stat_rule.md),
  [`export_statci_sarif()`](https://marcusharrisphd.com/statci/reference/export_statci_sarif.md).

- **Estimator Calibration, SBC, Power & Parallel RNG**:
  [`audit_estimator()`](https://marcusharrisphd.com/statci/reference/audit_estimator.md),
  [`audit_sbc()`](https://marcusharrisphd.com/statci/reference/audit_sbc.md),
  [`audit_power_curve()`](https://marcusharrisphd.com/statci/reference/audit_power_curve.md),
  [`check_parallel_rng()`](https://marcusharrisphd.com/statci/reference/check_parallel_rng.md),
  [`statci_parallel_apply()`](https://marcusharrisphd.com/statci/reference/statci_parallel_apply.md).

- **Manuscript-to-Code Consistency Auditor**:
  [`audit_manuscript()`](https://marcusharrisphd.com/statci/reference/audit_manuscript.md),
  [`audit_manuscript_text()`](https://marcusharrisphd.com/statci/reference/audit_manuscript_text.md).

- **Adversarial Sensitivity & Multiverse Robustness**:
  [`audit_sensitivity()`](https://marcusharrisphd.com/statci/reference/audit_sensitivity.md),
  [`audit_multiverse()`](https://marcusharrisphd.com/statci/reference/audit_multiverse.md).

- **Orchestration, Reports & GitHub Actions**:
  [`read_statci_config()`](https://marcusharrisphd.com/statci/reference/read_statci_config.md),
  [`statci_audit_project()`](https://marcusharrisphd.com/statci/reference/statci_audit_project.md),
  [`statci_report()`](https://marcusharrisphd.com/statci/reference/statci_report.md),
  [`render_statci_report()`](https://marcusharrisphd.com/statci/reference/render_statci_report.md),
  [`statci_check_gate()`](https://marcusharrisphd.com/statci/reference/statci_check_gate.md),
  [`use_statci_github_action()`](https://marcusharrisphd.com/statci/reference/use_statci_github_action.md).

## See also

Useful links:

- <https://marcusharrisphd.com/statci/>

- <https://github.com/MarcusHarrisUConn/statci>

- Report bugs at <https://github.com/MarcusHarrisUConn/statci/issues>

## Author

**Maintainer**: Marcus Harris <marcus.harris.uconn@gmail.com>
\[copyright holder\]

Authors:

- Marcus Harris <marcus.harris.uconn@gmail.com> \[copyright holder\]
