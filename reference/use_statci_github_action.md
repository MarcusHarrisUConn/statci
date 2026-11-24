# Generate a GitHub Actions workflow for automated `statci` verification

Creates a hardened GitHub Actions workflow YAML file (default
`.github/workflows/statci.yaml`) that installs R and package
dependencies, runs `statci` statistical linting, optional manuscript
auditing or custom calibration/sensitivity scripts, writes a
GitHub-Flavored Markdown summary to `$GITHUB_STEP_SUMMARY`, uploads the
standalone HTML audit dashboard as a workflow artifact, and enforces the
`statci` pass/warn/fail gate.

## Usage

``` r
use_statci_github_action(
  path = ".",
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
  overwrite = FALSE
)
```

## Arguments

- path:

  Root project directory (default `"."`).

- file:

  Workflow path relative to `path` (default
  `".github/workflows/statci.yaml"`). Must resolve inside `path`.

- branches:

  Character vector of branch names triggering `push` and `pull_request`
  workflows (default `c("main", "master")`).

- r_version:

  R version specifier passed to `r-lib/actions/setup-r` (default
  `"release"`). Allowed values: `"release"`, `"devel"`, `"oldrel"`,
  `"oldrel-1"`..`"oldrel-4"`, or a numeric version such as `"4.4.2"`.

- fail_on:

  CI gate level passed to
  [`statci_check_gate()`](https://marcusharrisphd.com/statci/reference/statci_check_gate.md):
  `"fail"` (default) or `"warn"`.

- lint_paths:

  Character vector of project-relative directories to scan with
  [`stat_lint_dir()`](https://marcusharrisphd.com/statci/reference/stat_lint_dir.md)
  when `audit_script` is `NULL` (default `c("R")`).

- manuscript:

  Optional project-relative path to a `.qmd`/`.Rmd` manuscript to audit
  against `models_path`.

- models_path:

  Optional project-relative path to an `.rds` file or directory of
  `.rds` model objects used with `manuscript`.

- audit_script:

  Optional project-relative path to a custom R script (ending in `.R` or
  `.r`) that executes full estimator calibration, manuscript auditing,
  and sensitivity checks.

- report_dir:

  Project-relative directory where HTML/Markdown audit artifacts are
  written (default `"statci-reports"`).

- upload_artifact:

  Logical; include an `actions/upload-artifact` step for HTML and
  Markdown audit reports? Default `TRUE`.

- upload_sarif:

  Logical; if `TRUE`, export AST lint findings to SARIF 2.1.0 via
  [`export_statci_sarif()`](https://marcusharrisphd.com/statci/reference/export_statci_sarif.md)
  and add a `github/codeql-action/upload-sarif` step (also adds
  `security-events: write` permission). Default `FALSE`.

- overwrite:

  Logical; overwrite existing workflow file? Default `FALSE`.

## Value

Invisibly returns the normalized path to the generated YAML file.

## Details

All user-supplied parameters (`branches`, `r_version`, `lint_paths`,
`manuscript`, `models_path`, `audit_script`, `report_dir`, `file`) are
strictly validated against path traversal, newline/YAML injection, and
shell/R command injection, and passed into the runner strictly via
workflow `env:` variables rather than interpolating untrusted strings
into `run:` blocks.

## Examples

``` r
tmp <- tempfile("statci_gh_")
dir.create(tmp)
wf <- use_statci_github_action(path = tmp, lint_paths = c("R", "analysis"))
file.exists(wf)
#> [1] TRUE
unlink(tmp, recursive = TRUE)
```
