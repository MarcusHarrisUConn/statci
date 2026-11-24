# Assemble a unified `statci` audit report

Combines results from any subset of `statci` verification suites—AST
linting
([`stat_lint()`](https://marcusharrisphd.com/statci/reference/stat_lint.md)
/
[`stat_lint_dir()`](https://marcusharrisphd.com/statci/reference/stat_lint_dir.md)),
estimator calibration
([`audit_estimator()`](https://marcusharrisphd.com/statci/reference/audit_estimator.md)),
Simulation-Based Calibration
([`audit_sbc()`](https://marcusharrisphd.com/statci/reference/audit_sbc.md)),
parallel RNG invariance
([`check_parallel_rng()`](https://marcusharrisphd.com/statci/reference/check_parallel_rng.md)),
manuscript auditing
([`audit_manuscript()`](https://marcusharrisphd.com/statci/reference/audit_manuscript.md)),
and adversarial sensitivity
([`audit_sensitivity()`](https://marcusharrisphd.com/statci/reference/audit_sensitivity.md))—
into a single pass/warn/fail audit report object.

## Usage

``` r
statci_report(
  lint = NULL,
  estimator = NULL,
  sbc = NULL,
  parallel_rng = NULL,
  manuscript = NULL,
  sensitivity = NULL,
  power_curve = NULL,
  multiverse = NULL,
  title = "statci Statistical CI/CD Audit Report",
  metadata = list()
)
```

## Arguments

- lint:

  Optional `"statci_lint"` object.

- estimator:

  Optional `"statci_estimator_audit"` object.

- sbc:

  Optional standalone `"statci_sbc"` object.

- parallel_rng:

  Optional standalone `"statci_parallel_rng"` object.

- manuscript:

  Optional `"statci_manuscript_audit"` object.

- sensitivity:

  Optional `"statci_sensitivity"` object.

- power_curve:

  Optional `"statci_power_curve"` object.

- multiverse:

  Optional `"statci_multiverse"` object.

- title:

  Report title string.

- metadata:

  Optional named list of custom metadata (e.g., git SHA, project name).

## Value

An S3 object of class `"statci_report"` containing `title`, `sections`
(summary tibble), `components`, `metadata`, and overall `status`
(`"pass"`, `"warn"`, or `"fail"`).

## Examples

``` r
lint_res <- stat_lint_text("set.seed(1)\nx <- rnorm(20)\n")
rep <- statci_report(lint = lint_res)
rep
#> [PASS] statci Statistical CI/CD Audit Report
#> [PASS] AST Statistical Linter: 1 file(s), 0 finding(s) (0 error, 0 warning, 0
#> info)
```
