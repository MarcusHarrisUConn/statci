# Enforce CI quality gates on a `statci_report`

Signals an error (causing a non-zero exit status in CI pipelines) when
the audit report fails to meet the requested gate level.

## Usage

``` r
statci_check_gate(report, fail_on = c("fail", "warn"))
```

## Arguments

- report:

  A `"statci_report"` (or any `statci_*` audit object with a `$status`
  field).

- fail_on:

  Gate threshold: `"fail"` (default; errors only on `"fail"`) or
  `"warn"` (errors on either `"warn"` or `"fail"`).

## Value

Invisibly returns `TRUE` when the gate passes; otherwise aborts with an
informative error message.

## Examples

``` r
rep <- statci_report(lint = stat_lint_text("set.seed(1)\nx <- rnorm(5)\n"))
statci_check_gate(rep)
```
