# Run the full configured `statci` verification pipeline on a project

Orchestrates statistical AST linting, optional manuscript-to-code
auditing, optional programmatic estimator/SBC/parallel
RNG/sensitivity/power-curve audits, report rendering (HTML, Markdown,
JSON, and/or SARIF 2.1.0), and optional CI gate enforcement in a single
call driven by `.statci.yml`.

## Usage

``` r
statci_audit_project(
  path = ".",
  config = NULL,
  estimator = NULL,
  sbc = NULL,
  parallel_rng = NULL,
  sensitivity = NULL,
  power_curve = NULL,
  multiverse = NULL,
  enforce_gate = FALSE
)
```

## Arguments

- path:

  Root project directory (default `"."`).

- config:

  One of:

  - A `"statci_config"` object from
    [`read_statci_config()`](https://marcusharrisphd.com/statci/reference/read_statci_config.md);

  - Path to a `.statci.yml` file; or

  - `NULL` (auto-discovers `file.path(path, ".statci.yml")` if present,
    otherwise uses sensible defaults).

- estimator:

  Optional `"statci_estimator_audit"` object to include.

- sbc:

  Optional `"statci_sbc"` object to include.

- parallel_rng:

  Optional `"statci_parallel_rng"` object to include.

- sensitivity:

  Optional `"statci_sensitivity"` object to include.

- power_curve:

  Optional `"statci_power_curve"` object to include.

- multiverse:

  Optional `"statci_multiverse"` object to include.

- enforce_gate:

  Logical; if `TRUE`, calls
  [`statci_check_gate()`](https://marcusharrisphd.com/statci/reference/statci_check_gate.md)
  using `config$gate$fail_on`. Default `FALSE`.

## Value

An S3 object of class `"statci_report"` (with `$config` and `$artifacts`
attached when reports are written to disk).

## Examples

``` r
tmp <- tempfile("statci_proj_")
dir.create(file.path(tmp, "R"), recursive = TRUE)
writeLines(c("set.seed(42)", "x <- rnorm(20)"), file.path(tmp, "R", "sim.R"))
rep <- statci_audit_project(tmp)
rep
#> [PASS] statci Statistical CI/CD Audit Report
#> [PASS] AST Statistical Linter: 1 file(s), 0 finding(s) (0 error, 0 warning, 0
#> info)
unlink(tmp, recursive = TRUE)
```
