# GitHub Actions CI/CD workflow generator (hardened against workflow/shell/R injection)

#' Generate a GitHub Actions workflow for automated `statci` verification
#'
#' Creates a hardened GitHub Actions workflow YAML file (default
#' `.github/workflows/statci.yaml`) that installs R and package dependencies,
#' runs `statci` statistical linting, optional manuscript auditing or custom
#' calibration/sensitivity scripts, writes a GitHub-Flavored Markdown summary
#' to `$GITHUB_STEP_SUMMARY`, uploads the standalone HTML audit dashboard as a
#' workflow artifact, and enforces the `statci` pass/warn/fail gate.
#'
#' All user-supplied parameters (`branches`, `r_version`, `lint_paths`,
#' `manuscript`, `models_path`, `audit_script`, `report_dir`, `file`) are
#' strictly validated against path traversal, newline/YAML injection, and
#' shell/R command injection, and passed into the runner strictly via workflow
#' `env:` variables rather than interpolating untrusted strings into `run:`
#' blocks.
#'
#' @param path Root project directory (default `"."`).
#' @param file Workflow path relative to `path` (default
#'   `".github/workflows/statci.yaml"`). Must resolve inside `path`.
#' @param branches Character vector of branch names triggering `push` and
#'   `pull_request` workflows (default `c("main", "master")`).
#' @param r_version R version specifier passed to `r-lib/actions/setup-r`
#'   (default `"release"`). Allowed values: `"release"`, `"devel"`, `"oldrel"`,
#'   `"oldrel-1"`..`"oldrel-4"`, or a numeric version such as `"4.4.2"`.
#' @param fail_on CI gate level passed to [statci_check_gate()]: `"fail"`
#'   (default) or `"warn"`.
#' @param lint_paths Character vector of project-relative directories to scan
#'   with [stat_lint_dir()] when `audit_script` is `NULL` (default `c("R")`).
#' @param manuscript Optional project-relative path to a `.qmd`/`.Rmd`
#'   manuscript to audit against `models_path`.
#' @param models_path Optional project-relative path to an `.rds` file or
#'   directory of `.rds` model objects used with `manuscript`.
#' @param audit_script Optional project-relative path to a custom R script
#'   (ending in `.R` or `.r`) that executes full estimator calibration,
#'   manuscript auditing, and sensitivity checks.
#' @param report_dir Project-relative directory where HTML/Markdown audit
#'   artifacts are written (default `"statci-reports"`).
#' @param upload_artifact Logical; include an `actions/upload-artifact` step
#'   for HTML and Markdown audit reports? Default `TRUE`.
#' @param upload_sarif Logical; if `TRUE`, export AST lint findings to SARIF
#'   2.1.0 via [export_statci_sarif()] and add a
#'   `github/codeql-action/upload-sarif` step (also adds
#'   `security-events: write` permission). Default `FALSE`.
#' @param overwrite Logical; overwrite existing workflow file? Default `FALSE`.
#'
#' @return Invisibly returns the normalized path to the generated YAML file.
#' @export
#'
#' @examples
#' tmp <- tempfile("statci_gh_")
#' dir.create(tmp)
#' wf <- use_statci_github_action(path = tmp, lint_paths = c("R", "analysis"))
#' file.exists(wf)
#' unlink(tmp, recursive = TRUE)
use_statci_github_action <- function(path = ".",
                                     file = ".github/workflows/statci.yaml",
                                     branches = c("main", "master"),
                                     r_version = "release",
                                     fail_on = c("fail", "warn"),
                                     lint_paths = c("R"),
                                     manuscript = NULL,
                                     models_path = NULL,
                                     audit_script = NULL,
                                     report_dir = "statci-reports",
                                     upload_artifact = TRUE,
                                     upload_sarif = FALSE,
                                     overwrite = FALSE) {
  fail_on <- match.arg(fail_on)
  validate_existing_dir(path, arg = "path")
  dest <- validate_contained_dest(path, file, arg = "file")
  branches <- validate_branch_names(branches)
  r_version <- validate_r_version(r_version)
  report_dir <- validate_safe_rel_path(report_dir, arg = "report_dir")

  if (!is.null(audit_script)) {
    audit_script <- validate_safe_rel_path(audit_script, arg = "audit_script", must_be_r_script = TRUE)
  } else {
    if (!is.character(lint_paths) || length(lint_paths) < 1L) {
      rlang::abort("`lint_paths` must be a non-empty character vector of relative directory paths.")
    }
    lint_paths <- vapply(lint_paths, validate_safe_rel_path, character(1L), arg = "lint_paths")
    if (!is.null(manuscript)) {
      manuscript <- validate_safe_rel_path(manuscript, arg = "manuscript")
    }
    if (!is.null(models_path)) {
      models_path <- validate_safe_rel_path(models_path, arg = "models_path")
    }
    if (xor(is.null(manuscript), is.null(models_path))) {
      rlang::abort("Provide both `manuscript` and `models_path`, or neither.")
    }
  }

  if (file.exists(dest) && !isTRUE(overwrite)) {
    rlang::abort(paste0("Workflow file already exists: `", dest, "`. Set `overwrite = TRUE` to replace it."))
  }

  dir_out <- dirname(dest)
  if (!dir.exists(dir_out)) {
    dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
  }

  branch_yaml <- paste0("[", paste(vapply(branches, safe_yaml_scalar, character(1L), arg = "branches"), collapse = ", "), "]")

  if (!is.null(audit_script)) {
    step_shell <- "bash"
    env_block <- c(
      "        env:",
      paste0("          STATCI_AUDIT_SCRIPT: ", safe_yaml_scalar(audit_script, "audit_script")),
      paste0("          STATCI_REPORT_DIR: ", safe_yaml_scalar(report_dir, "report_dir")),
      paste0("          STATCI_FAIL_ON: ", safe_yaml_scalar(fail_on, "fail_on"))
    )
    r_step_lines <- c(
      "          Rscript --vanilla \"$STATCI_AUDIT_SCRIPT\""
    )
  } else {
    step_shell <- "Rscript {0}"
    env_block <- c(
      "        env:",
      paste0("          STATCI_LINT_PATHS: ", safe_yaml_scalar(paste(lint_paths, collapse = ","), "lint_paths")),
      if (!is.null(manuscript)) paste0("          STATCI_MANUSCRIPT: ", safe_yaml_scalar(manuscript, "manuscript")),
      if (!is.null(models_path)) paste0("          STATCI_MODELS_PATH: ", safe_yaml_scalar(models_path, "models_path")),
      paste0("          STATCI_REPORT_DIR: ", safe_yaml_scalar(report_dir, "report_dir")),
      paste0("          STATCI_FAIL_ON: ", safe_yaml_scalar(fail_on, "fail_on"))
    )
    # Static R runner code with ZERO untrusted string interpolation
    r_step_lines <- c(
      "          library(statci)",
      "          report_dir <- Sys.getenv(\"STATCI_REPORT_DIR\", unset = \"statci-reports\")",
      "          fail_on <- Sys.getenv(\"STATCI_FAIL_ON\", unset = \"fail\")",
      "          raw_paths <- Sys.getenv(\"STATCI_LINT_PATHS\", unset = \"R\")",
      "          paths <- trimws(strsplit(raw_paths, \",\", fixed = TRUE)[[1L]])",
      "          paths <- paths[nzchar(paths) & dir.exists(paths)]",
      "          dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)",
      "          lint_res <- if (length(paths) > 0L) statci::stat_lint_dir(paths) else statci::stat_lint_text(\"\")",
      "          ms_path <- Sys.getenv(\"STATCI_MANUSCRIPT\", unset = \"\")",
      "          models_path <- Sys.getenv(\"STATCI_MODELS_PATH\", unset = \"\")",
      "          ms_res <- if (nzchar(ms_path) && nzchar(models_path)) {",
      "            statci::audit_manuscript(ms_path, models = models_path)",
      "          } else {",
      "            NULL",
      "          }",
      "          rep <- statci::statci_report(lint = lint_res, manuscript = ms_res)",
      "          statci::render_statci_report(rep, output_file = file.path(report_dir, \"statci-report.html\"), format = \"html\")",
      "          md <- statci::render_statci_report(rep, output_file = file.path(report_dir, \"statci-report.md\"), format = \"markdown\")",
      if (isTRUE(upload_sarif)) "          statci::export_statci_sarif(lint_res, output_file = file.path(report_dir, \"statci.sarif\"), base_uri = \".\")",
      "          summary_file <- Sys.getenv(\"GITHUB_STEP_SUMMARY\")",
      "          if (nzchar(summary_file)) cat(readLines(md, warn = FALSE), sep = \"\\n\", file = summary_file, append = TRUE)",
      "          print(rep)",
      "          statci::statci_check_gate(rep, fail_on = fail_on)"
    )
  }

  sarif_block <- if (isTRUE(upload_sarif)) {
    c(
      "",
      "      - name: Upload SARIF results to GitHub Code Scanning",
      "        if: always()",
      "        uses: github/codeql-action/upload-sarif@v3",
      "        with:",
      paste0("          sarif_file: ", safe_yaml_scalar(paste0(report_dir, "/statci.sarif"), "report_dir"))
    )
  } else {
    character(0)
  }

  perm_lines <- c(
    "permissions:",
    "  contents: read",
    if (isTRUE(upload_sarif)) "  security-events: write"
  )

  artifact_block <- if (isTRUE(upload_artifact)) {
    c(
      "",
      "      - name: Upload statci audit report artifacts",
      "        if: always()",
      "        uses: actions/upload-artifact@v4",
      "        with:",
      "          name: statci-audit-report",
      paste0("          path: ", safe_yaml_scalar(paste0(report_dir, "/"), "report_dir")),
      "          if-no-files-found: warn"
    )
  } else {
    character(0)
  }

  yaml_lines <- c(
    "# Generated by statci::use_statci_github_action()",
    "name: statci Statistical CI/CD",
    "",
    "on:",
    "  push:",
    paste0("    branches: ", branch_yaml),
    "  pull_request:",
    paste0("    branches: ", branch_yaml),
    "  workflow_dispatch:",
    "",
    perm_lines,
    "",
    "jobs:",
    "  statci-audit:",
    "    name: Statistical Verification & Audit",
    "    runs-on: ubuntu-latest",
    "    steps:",
    "      - name: Checkout repository",
    "        uses: actions/checkout@v4",
    "        with:",
    "          persist-credentials: false",
    "",
    "      - name: Setup R",
    "        uses: r-lib/actions/setup-r@v2",
    "        with:",
    paste0("          r-version: ", safe_yaml_scalar(r_version, "r_version")),
    "          use-public-rspm: true",
    "",
    "      - name: Install R dependencies",
    "        uses: r-lib/actions/setup-r-dependencies@v2",
    "        with:",
    "          extra-packages: |",
    "            any::statci",
    "",
    "      - name: Run statci verification suite",
    paste0("        shell: ", step_shell),
    env_block,
    "        run: |",
    r_step_lines,
    sarif_block,
    artifact_block
  )

  writeLines(yaml_lines, con = dest, useBytes = TRUE)
  invisible(normalizePath(dest, winslash = "/", mustWork = FALSE))
}
