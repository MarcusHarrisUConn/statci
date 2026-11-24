## R CMD check results

0 errors | 0 warnings | 0 notes

Checked with:
- `rcmdcheck::rcmdcheck()` (including full PDF reference manual build via `pdflatex` and 5 executable vignettes)
- `rcmdcheck::rcmdcheck(args = c("--as-cran"), env = c("_R_CHECK_CRAN_INCOMING_REMOTE_" = "false", "_R_CHECK_LIMIT_CORES_" = "true"))`
- `spelling::spell_check_package()` (0 misspellings)
- `covr::package_coverage()` (93.1% line coverage across 579 passing test expectations)

## Test environments

- local macOS (aarch64-apple-darwin23), R 4.6.1

## Submission notes

- This is a new CRAN submission (`statci` 0.5.1).
- All examples, tests, and vignettes respect the 2-core CRAN limit
  (`_R_CHECK_LIMIT_CORES_=true`), operate completely offline, write temporary
  files only inside `tempdir()` with cleanup, preserve `.Random.seed` via
  `withr`, and preserve graphics `par()` state via `on.exit()`.
- Formal DOI references are included in `DESCRIPTION` for Simulation-Based
  Calibration (Talts et al., 2018) and exact binomial intervals
  (Clopper & Pearson, 1934).
