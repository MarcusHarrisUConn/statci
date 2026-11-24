# Manuscript-to-Code Consistency Auditing for Quarto, RMarkdown & LaTeX

Copy-pasted numbers in research manuscripts routinely drift out of sync
with fitted R models during peer review, sample filtering, or covariate
revisions. `statci` verifies prose statistics, Markdown/Quarto tables,
LaTeX `.tex` manuscripts, and internal statistical arithmetic directly
against saved model objects.

``` r

library(statci)
```

## 1. Auditing Prose and Markdown/Quarto Tables Together

[`audit_manuscript()`](https://marcusharrisphd.com/statci/reference/audit_manuscript.md)
parses both inline APA prose (`b`, `SE`, `t(df)`, `F(df1, df2)`,
`chi2(df)`, `95% CI`, `p`, `OR`, `r`, `R^2`, `N`, `AIC`, `BIC`) and
Markdown/Quarto pipe tables:

``` r

fit_mpg <- lm(mpg ~ wt, data = mtcars)

qmd_path <- system.file("examples", "sample-manuscript.qmd", package = "statci")
if (nzchar(qmd_path)) {
  res_qmd <- audit_manuscript(qmd_path, models = list(primary = fit_mpg))
  res_qmd
  as_tibble(res_qmd)[, c("line", "term", "metric", "reported_raw", "actual", "issue_type", "status")]
}
#> # A tibble: 15 × 7
#>     line term  metric    reported_raw    actual issue_type status
#>    <int> <chr> <chr>     <chr>            <dbl> <chr>      <chr> 
#>  1     9 wt    ci_lower  -6.49        -6.49e+ 0 match      pass  
#>  2     9 wt    ci_upper  -4.20        -4.20e+ 0 match      pass  
#>  3     9 wt    estimate  -5.34        -5.34e+ 0 match      pass  
#>  4     9 wt    se        0.56          5.59e- 1 match      pass  
#>  5     9 wt    p_value   .001          1.29e-10 match      pass  
#>  6     9 wt    statistic -9.56        -9.56e+ 0 match      pass  
#>  7    10 NA    f_stat    91.38         9.14e+ 1 match      pass  
#>  8    10 NA    n         32            3.2 e+ 1 match      pass  
#>  9    10 NA    r_squared 0.75          7.53e- 1 match      pass  
#> 10    15 wt    estimate  -5.34        -5.34e+ 0 match      pass  
#> 11    15 wt    se        0.56          5.59e- 1 match      pass  
#> 12    15 wt    statistic -9.56        -9.56e+ 0 match      pass  
#> 13    15 wt    ci_lower  -6.49        -6.49e+ 0 match      pass  
#> 14    15 wt    ci_upper  -4.20        -4.20e+ 0 match      pass  
#> 15    15 wt    p_value   .001          1.29e-10 match      pass
```

## 2. Catching Sign Flips, Rounding Discrepancies, Table Drift & Stale Sample Sizes

Consider a manuscript where an author updated the model after dropping
outliers, but forgot to update a sign in prose, entered a stale table
cell, and left the old sample size `N = 40`:

``` r

drifted_md <- paste(
  "<!-- statci: model=primary term=wt -->",
  "Vehicle weight positively predicted MPG (*b* = 5.34, SE = 0.56, *p* < .001, *N* = 40).",
  "",
  "<!-- statci: model=primary -->",
  "| Term | b | SE | t | 95% CI | p |",
  "|---|---|---|---|---|---|",
  "| wt | -5.33 | 0.56 | -9.56 | [-6.49, -4.20] | < .001 |",
  sep = "\n"
)

drift_res <- audit_manuscript_text(drifted_md, models = list(primary = fit_mpg))
drift_res
#> [FAIL] statci Manuscript Audit (<text>): 10 claim(s) | 7 pass, 1 warn, 2 fail |
#> internal: 3 (0 fail)
#> [FAIL] line 2 (primary [wt] estimate): reported `= 5.34` vs actual `-5.3445`
#> (sign_flip)
#> [FAIL] line 2 (primary n): reported `= 40` vs actual `32.0000`
#> (stale_sample_size)
#> [WARN] line 7 (primary [wt] estimate): reported `= -5.33` vs actual `-5.3445`
#> (rounding_discrepancy)
#> [WARN] line 7 (internal_ci_midpoint): CI midpoint (-5.345) disagrees with
#> reported b (-5.33).
as_tibble(drift_res)[, c("line", "metric", "reported_raw", "actual", "issue_type", "status")]
#> # A tibble: 10 × 6
#>     line metric    reported_raw    actual issue_type           status
#>    <int> <chr>     <chr>            <dbl> <chr>                <chr> 
#>  1     2 estimate  5.34         -5.34e+ 0 sign_flip            fail  
#>  2     2 se        0.56          5.59e- 1 match                pass  
#>  3     2 p_value   .001          1.29e-10 match                pass  
#>  4     2 n         40            3.2 e+ 1 stale_sample_size    fail  
#>  5     7 estimate  -5.33        -5.34e+ 0 rounding_discrepancy warn  
#>  6     7 se        0.56          5.59e- 1 match                pass  
#>  7     7 statistic -9.56        -9.56e+ 0 match                pass  
#>  8     7 ci_lower  -6.49        -6.49e+ 0 match                pass  
#>  9     7 ci_upper  -4.20        -4.20e+ 0 match                pass  
#> 10     7 p_value   .001          1.29e-10 match                pass
```

Notice how `statci` distinguishes: - **`sign_flip` (`fail`)**:
`b = 5.34` vs actual `-5.3445` - **`stale_sample_size` (`fail`)**:
`N = 40` vs actual `32` - **`rounding_discrepancy` (`warn`)**: table
cell `-5.33` is off by 0.01 in the final decimal place relative to
`-5.3445`.

## 3. Auditing APA F-Statistics, Chi-Squared Tests & Scientific Notation

[`audit_manuscript()`](https://marcusharrisphd.com/statci/reference/audit_manuscript.md)
handles `F(df1, df2) = ...`, `chi2(df) = ...` (including `\chi^2`), odds
ratios (`OR`), Unicode minus signs, leading-dot decimals (`-.42`,
`.003`), and scientific notation (`1.2e-4`, `1.2 \times 10^{-4}`):

``` r

ct_mat <- matrix(c(30, 10, 12, 28), nrow = 2)
chi_fit <- suppressWarnings(chisq.test(ct_mat, correct = FALSE))
chi_val <- formatC(as.numeric(chi_fit$statistic), digits = 2, format = "f")

apa_txt <- paste0(
  "<!-- statci: model=lm_fit -->\n",
  "Overall regression was significant, F(1, 30) = 91.38, R^2 = 0.75, N = 32.\n",
  "<!-- statci: model=chi_fit -->\n",
  "Group proportions differed strongly, chi2(1) = ", chi_val, ", p < 1e-4."
)

audit_manuscript_text(apa_txt, models = list(lm_fit = fit_mpg, chi_fit = chi_fit))
#> [PASS] statci Manuscript Audit (<text>): 5 claim(s) | 5 pass, 0 warn, 0 fail
```

## 4. Auditing LaTeX (`.tex`) Manuscripts and `tabular` Environments

For journal submissions written in LaTeX,
[`audit_manuscript()`](https://marcusharrisphd.com/statci/reference/audit_manuscript.md)
natively strips `%` comments and `verbatim` blocks, unwraps
`\textit`/`\mathrm`/`\beta`/`\chi^2`, recognizes `% statci:` comment
anchors, and audits `\begin{tabular}...\end{tabular}` rows:

``` r

tex_path <- system.file("examples", "sample-manuscript.tex", package = "statci")
if (nzchar(tex_path)) {
  res_tex <- audit_manuscript(tex_path, models = list(primary = fit_mpg))
  res_tex
}
#> [PASS] statci Manuscript Audit (sample-manuscript.tex): 12 claim(s) | 12 pass,
#> 0 warn, 0 fail | internal: 4 (0 fail)
```

## 5. Standalone Internal Arithmetic Cross-Checking (Even Without Saved Models)

Even when no `.rds` model object is attached (`models = NULL`),
`audit_manuscript_text(..., check_internal = TRUE)` uses **interval
arithmetic** over rounding half-units to verify that reported `t`/`z`
matches `b / SE` and that `95% CI` bounds are symmetric around `b` and
consistent with `SE`:

``` r

# Consistent clause passes
ok_arith <- audit_manuscript_text(
  "Effect of x: b = 0.42, SE = 0.11, t(98) = 3.82, 95% CI [0.20, 0.64].",
  models = NULL
)
ok_arith
#> [PASS] statci Manuscript Audit (<text>): 0 claim(s) | 0 pass, 0 warn, 0 fail |
#> internal: 3 (0 fail)
ok_arith$internal_checks[, c("check_type", "reported_expr", "status", "detail")]
#> # A tibble: 3 × 4
#>   check_type           reported_expr                status detail               
#>   <chr>                <chr>                        <chr>  <chr>                
#> 1 internal_t_ratio     b=0.42, SE=0.11, t=3.82      pass   t consistent with b …
#> 2 internal_ci_midpoint b=0.42, CI [0.20, 0.64]      pass   CI midpoint consiste…
#> 3 internal_ci_width    SE=0.11, 95% CI [0.20, 0.64] pass   CI half-width consis…

# Copy-paste typo (t = 1.15 instead of 3.82; CI shifted right) fails automatically
bad_arith <- audit_manuscript_text(
  "Effect of x: b = 0.42, SE = 0.11, t(98) = 1.15, 95% CI [0.40, 0.90].",
  models = NULL
)
bad_arith
#> [FAIL] statci Manuscript Audit (<text>): 0 claim(s) | 0 pass, 0 warn, 0 fail |
#> internal: 3 (3 fail)
#> [FAIL] line 1 (internal_t_ratio): Reported t = 1.15 inconsistent with b / SE
#> (3.82)
#> [FAIL] line 1 (internal_ci_midpoint): CI midpoint (0.650) disagrees with
#> reported b (0.42).
#> [FAIL] line 1 (internal_ci_width): CI half-width (0.250) inconsistent with SE
#> (0.11).
bad_arith$internal_checks[, c("check_type", "status", "detail")]
#> # A tibble: 3 × 3
#>   check_type           status detail                                            
#>   <chr>                <chr>  <chr>                                             
#> 1 internal_t_ratio     fail   Reported t = 1.15 inconsistent with b / SE (3.82) 
#> 2 internal_ci_midpoint fail   CI midpoint (0.650) disagrees with reported b (0.…
#> 3 internal_ci_width    fail   CI half-width (0.250) inconsistent with SE (0.11).
```
