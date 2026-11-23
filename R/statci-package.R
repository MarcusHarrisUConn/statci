#' statci: Statistical CI/CD, Calibration Fuzzing, and Manuscript Auditing
#'
#' Automated continuous integration and verification tools for statistical R
#' pipelines, methods packages, and Quarto/RMarkdown/LaTeX manuscripts.
#'
#' @section Verification Pillars:
#' - **AST Statistical Linter**: [stat_lint()], [stat_lint_text()],
#'   [stat_lint_dir()], [list_stat_rules()], [new_stat_rule()],
#'   [register_stat_rule()], [export_statci_sarif()].
#' - **Estimator Calibration, SBC, Power & Parallel RNG**: [audit_estimator()],
#'   [audit_sbc()], [audit_power_curve()], [check_parallel_rng()],
#'   [statci_parallel_apply()].
#' - **Manuscript-to-Code Consistency Auditor**: [audit_manuscript()],
#'   [audit_manuscript_text()].
#' - **Adversarial Sensitivity Battery**: [audit_sensitivity()].
#' - **Orchestration, Reports & GitHub Actions**: [read_statci_config()],
#'   [statci_audit_project()], [statci_report()], [render_statci_report()],
#'   [statci_check_gate()], [use_statci_github_action()].
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang %||% .data
#' @importFrom tibble as_tibble tibble
## usethis namespace: end
NULL

#' @export
tibble::as_tibble
