# Getting Started with statci: Statistical Linting, Manuscript Auditing & CI Reports

`statci` brings software-engineering-grade continuous integration to
statistical R pipelines and Quarto/RMarkdown manuscripts. This vignette
walks through:

1.  Auditing R scripts and Quarto documents with the AST-based
    statistical linter.
2.  Suppressing intentional exceptions and registering custom AST rules.
3.  Cross-checking numerical claims in manuscripts against fitted R
    model objects.
4.  Combining results into standalone HTML/Markdown audit reports and
    generating a GitHub Actions workflow.

``` r

library(statci)
```

## 1. Inspecting Built-In Statistical Lint Rules

`statci` ships **20 built-in AST rules** targeting reproducibility,
parallel RNG safety, duplicate worker seeds, train/test data leakage,
in-sample prediction evaluation, multilevel centering,
missingness/factor/coercion/merge-cardinality/matrix-drop hazards,
floating-point comparisons, unvalidated stepwise selection, and
multiple-testing inflation:

``` r

list_stat_rules()[, c("id", "severity", "category", "title")]
#> # A tibble: 20 × 4
#>    id                             severity category        title                
#>    <chr>                          <chr>    <chr>           <chr>                
#>  1 unseeded_randomness            error    reproducibility Unseeded stochastic …
#>  2 unsafe_parallel_rng            error    parallel_rng    Unsafe parallel rand…
#>  3 duplicate_parallel_seed        error    parallel_rng    Constant set.seed() …
#>  4 preprocessing_before_split     error    data_leakage    Feature preprocessin…
#>  5 in_sample_prediction_eval      warning  data_leakage    Predictive error eva…
#>  6 uncentered_mlm_interaction     warning  modeling        Uncentered predictor…
#>  7 silent_listwise_mismatch       warning  modeling        Potential silent lis…
#>  8 silent_factor_drop             warning  modeling        Silent factor coerci…
#>  9 silent_na_coercion             warning  modeling        Unchecked numeric co…
#> 10 unverified_merge_cardinality   warning  modeling        Unverified join/merg…
#> 11 missing_drop_false_matrix      warning  modeling        2D column subsetting…
#> 12 implicit_na_rm_masking         warning  modeling        Summary metric uses …
#> 13 equality_on_float              warning  numerics        Exact equality compa…
#> 14 unadjusted_multiple_testing    warning  inference       Multiple hypothesis …
#> 15 stepwise_selection_unvalidated warning  inference       Unvalidated in-sampl…
#> 16 missing_convergence_check      warning  modeling        Iterative model fitt…
#> 17 global_seed_override_in_fn     warning  reproducibility Global RNG seed muta…
#> 18 short_logical_literal          warning  hygiene         Short logical symbol…
#> 19 cor_without_use_arg            info     modeling        cor()/cov() called w…
#> 20 absolute_path_in_script        warning  reproducibility Hardcoded machine-sp…
```

## 2. Linting R Scripts and Quarto Documents

Unlike style linters,
[`stat_lint()`](https://marcusharrisphd.com/statci/reference/stat_lint.md)
parses full R syntax trees via `xmlparsedata` and `xml2`. Let’s inspect
a small analysis script with several silent methodological flaws:

``` r

flawed_script <- '
df <- read.csv("/Users/researcher/Desktop/study.csv")
df$x <- scale(df$x)
idx <- sample(nrow(df), 80)
train <- df[idx, ]

fit <- lme4::lmer(y ~ x * w + (1 + x | school), data = train)
pvals <- sapply(1:10, function(j) t.test(rnorm(20), rnorm(20), var.equal = F)$p.value)
'

lint_bad <- stat_lint_text(flawed_script)
lint_bad
#> [FAIL] statci lint: 1 file(s), 20 rule(s) | 2 error(s), 5 warning(s), 0 info
#> WARN  <text:r>:2:16 (absolute_path_in_script): Hardcoded non-portable path
#> "/Users/researcher/Desktop/study.csv" inside `read.csv()` breaks
#> CI/reproducibility; use project-relative paths (e.g., `file.path()` or
#> `here::here()`).
#> ERROR <text:r>:3:9 (preprocessing_before_split): Potential train/test data
#> leakage: `scale()` is applied before data splitting on the full dataset; fit
#> preprocessing parameters on training data only.
#> ERROR <text:r>:4:8 (unseeded_randomness): Stochastic call `sample()` invoked
#> without a preceding `set.seed()` or `withr::with_seed()` call.
#> WARN  <text:r>:7:8 (missing_convergence_check): `lmer()` fitted without checking
#> `lme4::isSingular()` or `@optinfo$conv`; boundary/singular fits or optimizer
#> non-convergence may go undetected.
#> WARN  <text:r>:7:8 (uncentered_mlm_interaction): Multilevel interaction in
#> `lmer()` uses uncentered predictor(s): `w`. Center Level-1/Level-2 predictors
#> (e.g., cluster-mean or grand-mean centering) to prevent contextual confounding
#> and aid interpretation.
#> WARN  <text:r>:8:10 (unadjusted_multiple_testing): Repeated hypothesis testing
#> inside `sapply()` without `p.adjust()` or multiple-testing correction in scope.
#> WARN  <text:r>:8:76 (short_logical_literal): Mutable symbol `F` used instead of
#> `FALSE`. `T`/`F` can be overwritten by variables and silently corrupt
#> statistical arguments.
```

Notice how `statci` pinpoints each distinct statistical hazard—hardcoded
personal paths, unseeded stochasticity, preprocessing before data
splitting, uncentered multilevel interactions, missing
convergence/singularity checks, unadjusted multiple testing, and mutable
`F` instead of `FALSE`—with exact line and column coordinates.

### Exact Line Mapping and Cross-Chunk Scope in Quarto (`.qmd`)

For `.qmd` and `.Rmd` files, `statci` projects all R chunks into a
blank-line-preserving AST so line numbers match the original document
exactly and state flows cleanly across chunks:

``` r

fence <- paste(rep("`", 3), collapse = "")
qmd_doc <- paste(
  "---",
  'title: "Treatment Analysis"',
  "---",
  "",
  "We begin by setting a global seed in our setup chunk.",
  "",
  paste0(fence, "{r qmd-setup}"),
  "set.seed(2026)",
  fence,
  "",
  "Next we simulate and run a clean regression, with one intentional float comparison:",
  "",
  paste0(fence, "{r}"),
  "#| label: check-mean",
  "x <- rnorm(100)",
  "flag <- mean(x) == 0.0",
  fence,
  sep = "\n"
)

qmd_lint <- stat_lint_text(qmd_doc, format = "qmd")
qmd_lint
#> [WARN] statci lint: 1 file(s), 20 rule(s) | 0 error(s), 1 warning(s), 0 info
#> WARN  <text:qmd>:16:9 [check-mean] (equality_on_float): Exact floating-point
#> comparison (`==`) on `mean(x)==0.0`. Use `isTRUE(all.equal(...))` or `abs(a -
#> b) < tol` instead.
```

Notice that `rnorm(100)` on line 15 is recognized as seeded by
`set.seed(2026)` in the earlier `qmd-setup` chunk, while
`mean(x) == 0.0` is flagged at exact line 16 in chunk `check-mean`.

### Inline Suppressions and Custom Rules

Intentional exceptions can be silenced with `# statci-ignore` or
`# statci-ignore: <rule_id>`, and teams can register project-specific
AST rules with
[`new_stat_rule()`](https://marcusharrisphd.com/statci/reference/new_stat_rule.md)
and
[`register_stat_rule()`](https://marcusharrisphd.com/statci/reference/register_stat_rule.md):

``` r

no_attach_rule <- new_stat_rule(
  id = "no_attach_call",
  title = "Avoid attach() in reproducible analyses",
  severity = "warning",
  category = "hygiene",
  description = "attach() alters the search path and risks silent variable masking.",
  recommendation = "Use explicit data$col references or with().",
  check = function(ctx) {
    out <- list()
    for (u in ctx$units) {
      nds <- xml2::xml_find_all(u$xml, ".//SYMBOL_FUNCTION_CALL[text() = 'attach']")
      for (nd in nds) {
        out[[length(out) + 1L]] <- list(
          line = as.integer(xml2::xml_attr(nd, "line1")) + u$line_offset,
          col = as.integer(xml2::xml_attr(nd, "col1")),
          message = "Avoid attach(); pass `data =` explicitly to modeling functions."
        )
      }
    }
    out
  }
)

stat_lint_text("attach(mtcars)\n", rules = list(no_attach_rule))
#> [WARN] statci lint: 1 file(s), 1 rule(s) | 0 error(s), 1 warning(s), 0 info
#> WARN  <text:r>:1:1 (no_attach_call): Avoid attach(); pass `data =` explicitly to
#> modeling functions.
```

## 3. Manuscript-to-Code Consistency Auditing

Copy-pasted numbers in manuscripts frequently drift out of sync when
datasets are cleaned or covariates change.
[`audit_manuscript()`](https://marcusharrisphd.com/statci/reference/audit_manuscript.md)
and
[`audit_manuscript_text()`](https://marcusharrisphd.com/statci/reference/audit_manuscript_text.md)
cross-check prose statistics against actual fitted R model objects
(`lm`, `glm`, `lmerMod`, `htest`, or tidy data frames) using
rounding-aware tolerance rules.

``` r

set.seed(42)
study_df <- data.frame(
  treatment = rbinom(150, 1, 0.5),
  baseline = rnorm(150)
)
study_df$outcome <- 2.0 + 0.65 * study_df$treatment - 0.30 * study_df$baseline +
  rnorm(150, sd = 0.8)

fit_primary <- lm(outcome ~ treatment + baseline, data = study_df)
sm <- summary(fit_primary)$coefficients
ci <- confint(fit_primary)

# Suppose an author accidentally copied a flipped sign for baseline and a stale N = 160
draft_qmd <- paste(
  "---",
  "title: Main Results",
  "---",
  "",
  "<!-- statci: model=primary term=treatment -->",
  sprintf(
    "Treatment significantly improved outcomes (*b* = %.2f, SE = %.2f, 95%% CI [%.2f, %.2f], *p* < .001).",
    sm["treatment", 1], sm["treatment", 2], ci["treatment", 1], ci["treatment", 2]
  ),
  "",
  "<!-- statci: model=primary term=baseline -->",
  sprintf(
    "Baseline score was positively associated with outcome (*b* = %.2f), across *N* = 160 participants.",
    abs(sm["baseline", 1]) # Oops! Sign flipped from negative to positive, and N is stale
  ),
  sep = "\n"
)

ms_audit <- audit_manuscript_text(draft_qmd, models = list(primary = fit_primary))
ms_audit
#> [FAIL] statci Manuscript Audit (<text>): 7 claim(s) | 5 pass, 0 warn, 2 fail |
#> internal: 2 (0 fail)
#> [FAIL] line 9 (primary [baseline] estimate): reported `= 0.33` vs actual
#> `-0.3259` (sign_flip)
#> [FAIL] line 9 (primary n): reported `= 160` vs actual `150.0000`
#> (stale_sample_size)
```

Every verified claim—including passing rounded matches, sign flips, and
stale sample sizes—is returned in `as_tibble(ms_audit)`:

``` r

as_tibble(ms_audit)[, c("line", "term", "metric", "reported_raw", "actual", "issue_type", "status")]
#> # A tibble: 7 × 7
#>    line term      metric   reported_raw      actual issue_type        status
#>   <int> <chr>     <chr>    <chr>              <dbl> <chr>             <chr> 
#> 1     6 treatment ci_lower 0.28           0.277     match             pass  
#> 2     6 treatment ci_upper 0.77           0.774     match             pass  
#> 3     6 treatment estimate 0.53           0.526     match             pass  
#> 4     6 treatment se       0.13           0.126     match             pass  
#> 5     6 treatment p_value  .001           0.0000495 match             pass  
#> 6     9 baseline  estimate 0.33          -0.326     sign_flip         fail  
#> 7     9 NA        n        160          150         stale_sample_size fail
```

## 4. Unified Reports, CI Gates & GitHub Actions

Combine verification results into a unified
[`statci_report()`](https://marcusharrisphd.com/statci/reference/statci_report.md),
render standalone HTML or GitHub-Flavored Markdown summaries, and
enforce automated pass/warn/fail gates:

``` r

clean_lint <- stat_lint_text("set.seed(2026)\nx <- rnorm(50)\n")
rep <- statci_report(
  lint = clean_lint,
  manuscript = ms_audit,
  title = "Study Replication Audit"
)
rep
#> [FAIL] Study Replication Audit
#> [PASS] AST Statistical Linter: 1 file(s), 0 finding(s) (0 error, 0 warning, 0
#> info)
#> [FAIL] Manuscript-to-Code Consistency: 7 claim(s) audited against 1 model(s); 1
#> sign flip(s)

# Render Markdown summary suitable for $GITHUB_STEP_SUMMARY
md_summary <- render_statci_report(rep, format = "markdown")
cat(substr(md_summary, 1, 600), "...\n")
#> # Study Replication Audit
#> 
#> **Overall Status:** **FAIL** | **Generated:** 2026-11-24 21:43:38 UTC | **statci:** v0.5.1
#> 
#> ## Executive Summary
#> 
#> | Section | Status | Checks | Passed | Warnings | Failures | Summary |
#> | --- | --- | --- | --- | --- | --- | --- |
#> | AST Statistical Linter | **PASS** | 20 | 20 | 0 | 0 | 1 file(s), 0 finding(s) (0 error, 0 warning, 0 info) |
#> | Manuscript-to-Code Consistency | **FAIL** | 7 | 5 | 0 | 2 | 7 claim(s) audited against 1 model(s); 1 sign flip(s) |
#> 
#> ## 1. AST Statistical Linter
#> 
#> All checked files passed with zero statistical lint findings.
#> 
#> ## 3. Manuscript-to-C ...

# Generate a ready-to-use GitHub Actions workflow in a temporary project directory
tmp_proj <- tempfile("statci_demo_")
dir.create(tmp_proj)
wf_path <- use_statci_github_action(
  path = tmp_proj,
  lint_paths = c("R", "analysis"),
  manuscript = "manuscript.qmd",
  models_path = "models/fits.rds"
)
cat(head(readLines(wf_path), 24), sep = "\n")
#> # Generated by statci::use_statci_github_action()
#> name: statci Statistical CI/CD
#> 
#> on:
#>   push:
#>     branches: ['main', 'master']
#>   pull_request:
#>     branches: ['main', 'master']
#>   workflow_dispatch:
#> 
#> permissions:
#>   contents: read
#> 
#> jobs:
#>   statci-audit:
#>     name: Statistical Verification & Audit
#>     runs-on: ubuntu-latest
#>     steps:
#>       - name: Checkout repository
#>         uses: actions/checkout@v4
#>         with:
#>           persist-credentials: false
#> 
#>       - name: Setup R
unlink(tmp_proj, recursive = TRUE)
```
