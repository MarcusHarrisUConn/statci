# Read and validate a `.statci.yml` project configuration file

Parses a YAML configuration file strictly with `eval.expr = FALSE`
(blocking arbitrary R code evaluation during YAML loading) and validates
all fields against the `statci` configuration schema, including
enabled/disabled lint rules, severity overrides, exclude patterns,
manuscript/model paths, report targets, and CI gate thresholds.

## Usage

``` r
read_statci_config(path = ".statci.yml")
```

## Arguments

- path:

  Path to a `.statci.yml` (or custom YAML) configuration file.

## Value

An S3 object of class `"statci_config"` containing normalized `lint`,
`manuscript`, `gate`, `report`, and `source` fields.

## Examples

``` r
tmp <- tempfile(fileext = ".yml")
writeLines(c(
  "version: 1",
  "lint:",
  "  paths: [R]",
  "  exclude_rules: [cor_without_use_arg]",
  "gate:",
  "  fail_on: fail"
), tmp)
cfg <- read_statci_config(tmp)
cfg
#> <statci_config> (source: file197fa38ee1c.yml)
#> lint paths: R
#> excluded rules: cor_without_use_arg
#> gate fail_on: fail
unlink(tmp)
```
