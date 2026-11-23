# GitHub Actions CI/CD workflow generator

#' Generate a GitHub Actions workflow for automated `statci` verification
#'
#' Creates a ready-to-use GitHub Actions workflow YAML file (default
#' `.github/workflows/statci.yaml`) that installs R and package dependencies,
#' runs `statci` statistical linting, optional manuscript auditing or custom
#' calibration/sensitivity scripts, writes a GitHub-Flavored Markdown summary
#' to `$GITHUB_STEP_SUMMARY`, uploads the standalone HTML audit dashboard as a
#' workflow artifact, and enforces the `statci` pass/warn/fail gate.
#'
#' @param path Root project directory (default `"."`).
#' @param file Workflow path relative to `path` (default
#'   `".github/workflows/statci.yaml"`).
#' @param r_version R version specifier passed to `r-lib/actions/setup-r@v2`
#'   (default `"release"`).
#' @param fail_on CI gate level passed to [statci_check_gate()]: `"fail"`
#'   (default) or `"warn"`.
#' @param lint_paths Character vector of project directories to scan with
#'   [stat_lint_dir()] when `audit_script` is `NULL` (default `c("R")`).
#' @param manuscript Optional path to a `.qmd`/`.Rmd` manuscript to audit
#'   against `models_path`.
#' @param models_path Optional path to an `.rds` file or directory of `.rds`
#'   model objects used with `manuscript`.
#' @param audit_script Optional path to a custom project R script (e.g.,
#'   `"inst/statci/run-audit.R"`) that executes full estimator calibration,
#'   manuscript auditing, and sensitivity checks. When supplied, the workflow
#'   executes `audit_script` directly.
#' @param report_dir Relative directory where HTML/Markdown audit artifacts are
#'   written (default `"statci-reports"`).
#' @param upload_artifact Logical; include an `actions/upload-artifact@v4` step
#'   for HTML and Markdown audit reports? Default `TRUE`.
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
                                     r_version = "release",
                                     fail_on = c("fail", "warn"),
                                     lint_paths = c("R"),
                                     manuscript = NULL,
                                     models_path = NULL,
                                     audit_script = NULL,
                                     report_dir = "statci-reports",
                                     upload_artifact = TRUE,
                                     overwrite = FALSE) {
  fail_on <- match.arg(fail_on)
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    rlang::abort("`path` must be a valid directory path.")
  }

  dest <- file.path(path, file)
  if (file.exists(dest) && !isTRUE(overwrite)) {
    rlang::abort(paste0("Workflow file already exists: `", dest, "`. Set `overwrite = TRUE` to replace it."))
  }

  dir_out <- dirname(dest)
  if (!dir.exists(dir_out)) {
    dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
  }

  step_shell <- if (!is.null(audit_script)) "bash" else "Rscript {0}"
  r_step_lines <- if (!is.null(audit_script)) {
    c(
      paste0("          Rscript \"", audit_script, "\"")
    )
  } else {
    lint_vec <- paste0("c(", paste(paste0("\"", lint_paths, "\""), collapse = ", "), ")")
    ms_block <- if (!is.null(manuscript) && !is.null(models_path)) {
      paste0(
        "          ms_res <- statci::audit_manuscript(\"", manuscript, "\", models = \"", models_path, "\")"
      )
    } else {
      "          ms_res <- NULL"
    }
    c(
      "          library(statci)",
      paste0("          dir.create(\"", report_dir, "\", recursive = TRUE, showWarnings = FALSE)"),
      paste0("          paths <- ", lint_vec),
      "          paths <- paths[dir.exists(paths)]",
      "          lint_res <- if (length(paths) > 0L) statci::stat_lint_dir(paths) else statci::stat_lint_text(\"\")",
      ms_block,
      "          rep <- statci::statci_report(lint = lint_res, manuscript = ms_res)",
      paste0("          statci::render_statci_report(rep, output_file = \"", report_dir, "/statci-report.html\", format = \"html\")"),
      paste0("          md <- statci::render_statci_report(rep, output_file = \"", report_dir, "/statci-report.md\", format = \"markdown\")"),
      "          summary_file <- Sys.getenv(\"GITHUB_STEP_SUMMARY\")",
      "          if (nzchar(summary_file)) cat(readLines(md, warn = FALSE), sep = \"\\n\", file = summary_file, append = TRUE)",
      "          print(rep)",
      paste0("          statci::statci_check_gate(rep, fail_on = \"", fail_on, "\")")
    )
  }

  artifact_block <- if (isTRUE(upload_artifact)) {
    c(
      "",
      "      - name: Upload statci audit report artifacts",
      "        if: always()",
      "        uses: actions/upload-artifact@v4",
      "        with:",
      "          name: statci-audit-report",
      paste0("          path: ", report_dir, "/"),
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
    "    branches: [main, master]",
    "  pull_request:",
    "    branches: [main, master]",
    "  workflow_dispatch:",
    "",
    "permissions:",
    "  contents: read",
    "",
    "jobs:",
    "  statci-audit:",
    "    name: Statistical Verification & Audit",
    "    runs-on: ubuntu-latest",
    "    steps:",
    "      - name: Checkout repository",
    "        uses: actions/checkout@v4",
    "",
    "      - name: Setup R",
    "        uses: r-lib/actions/setup-r@v2",
    "        with:",
    paste0("          r-version: '", r_version, "'"),
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
    "        run: |",
    r_step_lines,
    artifact_block
  )

  writeLines(yaml_lines, con = dest, useBytes = TRUE)
  invisible(normalizePath(dest, mustWork = FALSE))
}
