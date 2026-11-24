# Tests for v0.4.0 capabilities (20 AST rules, stat_lint_diff, audit_multiverse)
# plus targeted coverage-gap and regression tests from covr audit

test_that("20 built-in AST rules include Rules 18-20 with true positives and 0% false positives", {
  reset_stat_rules()
  rules <- list_stat_rules()
  expect_equal(nrow(rules), 20L)
  expect_true(all(c(
    "unverified_merge_cardinality",
    "missing_drop_false_matrix",
    "implicit_na_rm_masking"
  ) %in% rules$id))

  # Rule 18: unverified_merge_cardinality
  bad_m1 <- stat_lint_text("m <- merge(df1, df2, by = 'id')\n", rules = "unverified_merge_cardinality")
  expect_equal(nrow(bad_m1$findings), 1L)
  bad_m2 <- stat_lint_text("m <- dplyr::left_join(df1, df2, by = 'id')\n", rules = "unverified_merge_cardinality")
  expect_equal(nrow(bad_m2$findings), 1L)
  ok_m1 <- stat_lint_text("m <- dplyr::left_join(df1, df2, by = 'id', relationship = 'many-to-one')\n", rules = "unverified_merge_cardinality")
  expect_equal(nrow(ok_m1$findings), 0L)
  ok_m2 <- stat_lint_text("m <- merge(df1, df2, by = 'id')\nstopifnot(nrow(m) == nrow(df1))\n", rules = "unverified_merge_cardinality")
  expect_equal(nrow(ok_m2$findings), 0L)

  # Rule 19: missing_drop_false_matrix
  bad_d1 <- stat_lint_text("f <- function(X, j) X[, j]\n", rules = "missing_drop_false_matrix")
  expect_equal(nrow(bad_d1$findings), 1L)
  bad_d2 <- stat_lint_text("f <- \\(M, k) M[, k]\n", rules = "missing_drop_false_matrix")
  expect_equal(nrow(bad_d2$findings), 1L)
  ok_d1 <- stat_lint_text("f <- function(X, j) X[, j, drop = FALSE]\n", rules = "missing_drop_false_matrix")
  expect_equal(nrow(ok_d1$findings), 0L)
  ok_d2 <- stat_lint_text("f <- function(df, idx) { sub <- df[idx, ]; sub[, 1:3]; sub[, c('a', 'b')]; sub[, 1] <- 0 }\n", rules = "missing_drop_false_matrix")
  expect_equal(nrow(ok_d2$findings), 0L)
  # Top-level (outside reusable function) should not trigger
  ok_d3 <- stat_lint_text("v <- X[, 1]\n", rules = "missing_drop_false_matrix")
  expect_equal(nrow(ok_d3$findings), 0L)

  # Rule 20: implicit_na_rm_masking
  bad_na1 <- stat_lint_text("m <- mean(x, na.rm = TRUE)\ns <- sd(x, na.rm = TRUE)\n", rules = "implicit_na_rm_masking")
  expect_equal(nrow(bad_na1$findings), 2L)
  ok_na1 <- stat_lint_text("n_miss <- sum(is.na(x))\nm <- mean(x, na.rm = TRUE)\n", rules = "implicit_na_rm_masking")
  expect_equal(nrow(ok_na1$findings), 0L)
  ok_na2 <- stat_lint_text("if (!anyNA(x)) m <- mean(x, na.rm = TRUE)\n", rules = "implicit_na_rm_masking")
  expect_equal(nrow(ok_na2$findings), 0L)
  ok_na3 <- stat_lint_text("m <- mean(x)\n", rules = "implicit_na_rm_masking")
  expect_equal(nrow(ok_na3$findings), 0L)

  # Verify clean-analysis.R passes ALL 20 rules with 0 findings and flawed-analysis.R catches new rules
  clean_ex <- system.file("examples", "clean-analysis.R", package = "statci")
  flawed_ex <- system.file("examples", "flawed-analysis.R", package = "statci")
  if (nzchar(clean_ex) && nzchar(flawed_ex)) {
    expect_equal(nrow(stat_lint(clean_ex)$findings), 0L)
    fl <- stat_lint(flawed_ex)
    expect_true(all(c(
      "unverified_merge_cardinality",
      "missing_drop_false_matrix",
      "implicit_na_rm_masking"
    ) %in% fl$findings$rule))
  }
})

test_that("stat_lint_diff reports only added/modified lines via diff_text and real git diff, with security guards", {
  tmp <- tempfile("statci_diff_test_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # 1. Offline unified diff_text (with context lines, deletions, and /dev/null)
  script <- file.path(tmp, "legacy.R")
  writeLines(c(
    "legacy_bad <- rnorm(10)",
    "set.seed(42)",
    "ok_new <- rnorm(10)",
    "bad_new <- mean(ok_new, na.rm = T)"
  ), script)

  diff_u <- paste(
    "diff --git a/deleted.R b/deleted.R",
    "--- a/deleted.R",
    "+++ /dev/null",
    "@@ -1,2 +0,0 @@",
    "-x <- rnorm(5)",
    "diff --git a/legacy.R b/legacy.R",
    "--- a/legacy.R",
    "+++ b/legacy.R",
    "@@ -1,2 +1,4 @@",
    " legacy_bad <- rnorm(10)",
    " set.seed(42)",
    "+ok_new <- rnorm(10)",
    "+bad_new <- mean(ok_new, na.rm = T)",
    sep = "\n"
  )

  dres <- stat_lint_diff(path = tmp, diff_text = diff_u)
  expect_s3_class(dres, "statci_lint")
  # Line 1 (legacy_bad) is suppressed; line 3 is seeded by untouched line 2; only line 4 flags!
  expect_true(all(dres$findings$line == 4L))
  expect_true("short_logical_literal" %in% dres$findings$rule)
  expect_gte(dres$diff_meta$n_suppressed_untouched, 1L)
  expect_equal(dres$diff_meta$changed_files, "legacy.R")

  # 2. Security: reject path traversal in crafted diff_text and option injection in base_ref
  evil_diff <- paste(
    "--- a/../../outside.R",
    "+++ b/../../outside.R",
    "@@ -0,0 +1,1 @@",
    "+x <- rnorm(1)",
    sep = "\n"
  )
  expect_error(stat_lint_diff(path = tmp, diff_text = evil_diff), "traversal")
  expect_error(stat_lint_diff(path = tmp, base_ref = "--exec=id"), "option injection")
  expect_error(stat_lint_diff(path = tmp, base_ref = "main;id"), "Invalid Git ref")
  expect_error(stat_lint_diff(path = tmp, base_ref = "HEAD..main"), "disallowed Git ref")
  expect_error(stat_lint_diff(path = tmp, diff_text = 123), "character vector")

  # 3. Real temporary git repository workflow (working tree, staged, and two-ref diff)
  git_bin <- Sys.which("git")
  skip_if(!nzchar(git_bin))
  repo <- file.path(tmp, "repo with space")
  dir.create(repo)
  run_git <- function(args) {
    system2(git_bin, c("-C", shQuote(repo), args), stdout = TRUE, stderr = TRUE)
  }
  run_git(c("init", "-q"))
  run_git(c("config", "user.email", "test@example.com"))
  run_git(c("config", "user.name", shQuote("Test User")))
  run_git(c("config", "commit.gpgsign", "false"))
  writeLines(c("legacy <- rnorm(5)", "set.seed(1)"), file.path(repo, "app.R"))
  run_git(c("add", "app.R"))
  run_git(c("commit", "-q", "-m", shQuote("initial legacy commit")))

  # Modify app.R: add a line with float == on line 3
  writeLines(c("legacy <- rnorm(5)", "set.seed(1)", "flag <- mean(rnorm(5)) == 0.5"), file.path(repo, "app.R"))
  gres <- stat_lint_diff(path = repo, base_ref = "HEAD")
  expect_equal(nrow(gres$findings), 1L)
  expect_equal(gres$findings$line, 3L)
  expect_equal(gres$findings$rule, "equality_on_float")

  # Staged diff
  run_git(c("add", "app.R"))
  sres <- stat_lint_diff(path = repo, staged = TRUE)
  expect_equal(nrow(sres$findings), 1L)
  expect_equal(sres$findings$line, 3L)

  # Two-ref diff (HEAD~1 .. HEAD)
  run_git(c("commit", "-q", "-m", shQuote("add check")))
  twres <- stat_lint_diff(path = repo, base_ref = "HEAD~1", head_ref = "HEAD")
  expect_equal(nrow(twres$findings), 1L)
  expect_equal(twres$findings$line, 3L)
})

test_that("audit_multiverse evaluates specification curves, detects fragility, plots, and integrates into reports", {
  set.seed(42)
  n <- 90L
  df <- data.frame(
    x = rnorm(n),
    c1 = rnorm(n),
    c2 = rnorm(n)
  )
  df$y <- 1.0 + 1.2 * df$x + 0.3 * df$c1 - 0.2 * df$c2 + rnorm(n, sd = 0.5)

  # 1. Robust 2x2x2 multiverse -> PASS
  mv_ok <- audit_multiverse(
    data = df,
    decisions = list(
      outliers = c("none", "winsor_5pct"),
      covariates = c("none", "c1", "c1_c2"),
      transform = c("raw", "centered")
    ),
    fit_fn = function(d, spec) {
      if (spec$outliers == "winsor_5pct") {
        qs <- stats::quantile(d$y, c(0.05, 0.95))
        d$y <- pmin(pmax(d$y, qs[1L]), qs[2L])
      }
      if (spec$transform == "centered") {
        d$x <- d$x - mean(d$x)
      }
      fml <- switch(
        spec$covariates,
        none = y ~ x,
        c1 = y ~ x + c1,
        c1_c2 = y ~ x + c1 + c2
      )
      stats::lm(fml, data = d)
    },
    param = "x"
  )

  expect_s3_class(mv_ok, "statci_multiverse")
  expect_equal(mv_ok$status, "pass")
  expect_equal(mv_ok$summary$n_specs, 12L)
  expect_equal(mv_ok$summary$n_valid, 12L)
  expect_equal(mv_ok$summary$sign_consistency, 1.0)
  expect_equal(mv_ok$summary$prop_significant, 1.0)
  expect_s3_class(as_tibble(mv_ok), "tbl_df")
  expect_invisible(print(mv_ok))
  sm_mv <- summary(mv_ok)
  expect_equal(sm_mv$overall$status, "pass")
  if (rlang::is_installed("ggplot2")) {
    expect_s3_class(plot(mv_ok), "ggplot")
  }

  # 2. Fragile multiverse where outlier rule flips sign -> FAIL and identifies top driver
  df_frag <- df
  df_frag$y <- -0.4 * df_frag$x + rnorm(n, sd = 0.3)
  df_frag$x[1L] <- 14
  df_frag$y[1L] <- 24

  mv_bad <- audit_multiverse(
    data = df_frag,
    decisions = list(
      outliers = c("keep_all", "winsor_5pct"),
      covariates = c("none", "c1")
    ),
    fit_fn = function(d, spec) {
      if (spec$outliers == "winsor_5pct") {
        qx <- stats::quantile(d$x, c(0.05, 0.95))
        qy <- stats::quantile(d$y, c(0.05, 0.95))
        d$x <- pmin(pmax(d$x, qx[1L]), qx[2L])
        d$y <- pmin(pmax(d$y, qy[1L]), qy[2L])
      }
      if (spec$covariates == "none") stats::lm(y ~ x, data = d) else stats::lm(y ~ x + c1, data = d)
    },
    param = "x"
  )
  expect_equal(mv_bad$status, "fail")
  expect_equal(mv_bad$summary$sign_consistency, 0.5)
  expect_equal(mv_bad$summary$most_influential_fork, "outliers")

  # 3. 100% failing specifications -> clean fail without crash; max_specs guard
  mv_err <- audit_multiverse(
    data = df,
    decisions = list(a = c("1", "2")),
    fit_fn = function(d, spec) stop("boom")
  )
  expect_equal(mv_err$status, "fail")
  expect_equal(mv_err$summary$n_valid, 0L)
  expect_error(plot(mv_err), "No converged specifications")
  expect_error(
    audit_multiverse(df, list(a = 1:20, b = 1:20), fit_fn = function(d, s) 1, max_specs = 10L),
    "exceeding `max_specs"
  )

  # 4. Integrates into statci_report (HTML, Markdown, JSON) and statci_audit_project
  rep <- statci_report(multiverse = mv_ok)
  expect_equal(rep$status, "pass")
  expect_match(render_statci_report(rep, format = "html"), "Multiverse Specification Curve", fixed = TRUE)
  expect_match(render_statci_report(rep, format = "markdown"), "Multiverse Specification Curve", fixed = TRUE)
  js <- jsonlite::fromJSON(render_statci_report(rep, format = "json"))
  expect_equal(nrow(js$multiverse), 12L)
})

test_that("covr regression and branch-coverage checks across utils, parser, manuscript, config, power, report, and sarif", {
  # 1. R/utils.R: default_extract_estimates & normalize_estimator_result edge branches
  expect_equal(names(statci:::default_extract_estimates(c(1, 2))), c("param_1", "param_2"))
  expect_equal(names(statci:::default_extract_estimates(list(estimate = c(3, 4)))), c("param_1", "param_2"))
  expect_equal(statci:::default_extract_estimates(stats::t.test(1:10))[[1L]], 5.5)
  expect_error(statci:::default_extract_estimates("bad_obj"), "Cannot auto-extract")
  expect_error(statci:::normalize_estimator_result("bad_obj"), "Unsupported return type")
  norm_explicit <- statci:::normalize_estimator_result(list(
    estimate = c(1.2), term = "beta1", ci_lower = 0.5, ci_upper = 1.9, p_value = 0.01
  ))
  expect_equal(names(norm_explicit$estimate), "beta1")
  expect_equal(norm_explicit$p_value[["beta1"]], 0.01)
  if (rlang::is_installed("lme4")) {
    fm <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
    expect_true("Days" %in% names(statci:::default_extract_estimates(fm)))
  }

  # 2. R/lint-parser.R & R/lint-rules.R: .R parse error col extraction, right-assign, mutate centering, print.statci_rule
  pe <- stat_lint_text("x <- 1\ny <- 2 + * 3\n")
  expect_equal(pe$status, "fail")
  expect_equal(pe$findings$rule, "parse_error")
  expect_equal(pe$findings$line, 2L)
  expect_gt(pe$findings$col, 1L)

  # Right assignment `->`
  ra <- stat_lint_text("factor(c('1', '2')) -> f\nas.numeric(f) -> v\n", rules = "silent_factor_drop")
  expect_equal(nrow(ra$findings), 1L)

  # dplyr::mutate centering & factor recognized by uncentered_mlm_interaction
  mut_ok <- stat_lint_text(
    "df2 <- dplyr::mutate(df, x = x - mean(x), g = factor(g))\nfit <- lmer(y ~ x * g + (1 | id), data = df2)\n",
    rules = "uncentered_mlm_interaction"
  )
  expect_equal(nrow(mut_ok$findings), 0L)
  expect_invisible(print(new_stat_rule("r_p", "Title", description = "Desc", check = function(ctx) list())))
  expect_error(stat_lint_text("x <- 1", rules = "nonexistent_rule"), "Unknown lint rule")
  expect_error(stat_lint_text("x <- 1", rules = 123), "`rules` must be")

  # 3. R/audit-manuscript.R: single-line \begin{verbatim}...\end{verbatim} bugfix, missing_model/term, infer_group_term, AIC/BIC, inequalities
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  tex_verb <- paste(
    "\\begin{verbatim} b = 999.99 \\end{verbatim}",
    "% statci: model=fit term=wt",
    paste0("Slope: b = ", round(coef(fit)[["wt"]], 2), ", N = 32."),
    sep = "\n"
  )
  res_v <- audit_manuscript_text(tex_verb, models = list(fit = fit), format = "tex")
  expect_equal(res_v$status, "pass")
  expect_equal(nrow(res_v$checks), 2L)

  # Missing model & missing term diagnostics
  mm <- audit_manuscript_text("<!-- statci: model=nope term=wt -->\nb = 1.0.", models = list(fit = fit))
  expect_equal(mm$checks$issue_type, "missing_model")
  mt <- audit_manuscript_text("<!-- statci: model=fit term=nope -->\nb = 1.0.", models = list(fit = fit))
  expect_equal(mt$checks$issue_type, "missing_term")

  # Untagged multi-term inference by prose mention & by metric scoring + default_model + AIC/BIC + p >= / <= / >
  b_hp <- formatC(coef(fit)[["hp"]], digits = 3, format = "f")
  aic_v <- formatC(AIC(fit), digits = 1, format = "f")
  bic_v <- formatC(BIC(fit), digits = 1, format = "f")
  multi_txt <- paste(
    paste0("Horsepower hp had a small effect (b = ", b_hp, ", p <= 0.05)."),
    paste0("Another term had slope b = ", round(coef(fit)[["wt"]], 2), ", p < 0.01."),
    paste0("Fit criteria: AIC = ", aic_v, ", BIC = ", bic_v, "."),
    sep = "\n"
  )
  res_m <- audit_manuscript_text(multi_txt, models = list(m1 = fit, m2 = fit), default_model = "m1")
  expect_equal(res_m$status, "pass")
  expect_true(all(c("aic", "bic") %in% res_m$checks$metric))
  expect_error(audit_manuscript_text("b = 1", models = list(fit = fit), default_model = "nope"), "default_model")
  expect_error(audit_manuscript_text("b = 1", models = list(bad = "not_a_model")), "Unsupported model class")

  # 4. R/report.R: HTML & Markdown render BOTH estimator$type1 and manuscript$internal_checks
  est_t1 <- audit_estimator(
    dgp = function() { x <- rnorm(50); data.frame(x = x, y = rnorm(50)) },
    estimator = function(d) lm(y ~ x, data = d),
    true_params = c(x = 0),
    n_sim = 40L,
    seed = 42L
  )
  ms_ic <- audit_manuscript_text("b = 0.42, SE = 0.11, t(98) = 1.10, 95% CI [0.20, 0.64].", models = NULL)
  rep_ic <- statci_report(estimator = est_t1, manuscript = ms_ic)
  h_ic <- render_statci_report(rep_ic, format = "html")
  m_ic <- render_statci_report(rep_ic, format = "markdown")
  expect_match(h_ic, "Internal Arithmetic Checks", fixed = TRUE)
  expect_match(m_ic, "Internal Arithmetic Checks", fixed = TRUE)
  expect_match(h_ic, "Type-I Error Calibration", fixed = TRUE)
  expect_match(m_ic, "Type-I Error Calibration", fixed = TRUE)
  expect_invisible(print(statci_report()))
  expect_error(statci_check_gate(list()), "must be a `statci_report`")

  # 5. R/audit-power.R: lm, logical, list(p_value), list(reject), and failing test_fn
  pc_lm <- audit_power_curve(
    dgp = function(d) { x <- rnorm(40); data.frame(x = x, y = d * x + rnorm(40)) },
    test_fn = function(df) lm(y ~ x, data = df),
    effect_grid = c(0, 0.6),
    param = "x",
    n_sim = 25L,
    seed = 3L
  )
  expect_equal(pc_lm$status, "pass")
  pc_lgl <- audit_power_curve(
    dgp = function(d) d,
    test_fn = function(d) d > 0.2,
    effect_grid = c(0, 0.5),
    n_sim = 20L,
    check_null_type1 = FALSE,
    seed = 3L
  )
  expect_equal(pc_lgl$curve$power, c(0, 1))
  pc_lst <- audit_power_curve(
    dgp = function(d) d,
    test_fn = function(d) list(reject = d > 0.2),
    effect_grid = c(0, 0.5),
    n_sim = 20L,
    check_null_type1 = FALSE,
    seed = 3L
  )
  expect_equal(pc_lst$curve$power, c(0, 1))
  pc_err <- audit_power_curve(
    dgp = function(d) d,
    test_fn = function(d) stop("fail"),
    effect_grid = c(0, 0.5),
    n_sim = 12L
  )
  expect_equal(pc_err$status, "fail")

  # 6. R/config.R: error paths + internal-only manuscript project + missing lint dir fallback
  tmp <- tempfile("statci_cfg_cov_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("b = 0.42, SE = 0.11, t(98) = 3.82, 95% CI [0.20, 0.64].", file.path(tmp, "note.qmd"))
  cfg_f <- file.path(tmp, "custom.yml")
  writeLines(c(
    "version: 1",
    "lint:",
    "  paths: [missing_dir]",
    "  rules: [unseeded_randomness]",
    "  skip_eval_false: true",
    "manuscript:",
    "  path: note.qmd",
    "  tol_extra: 0.001",
    "  warn_rounding: true",
    "  check_internal: true",
    "gate:",
    "  fail_on: warn"
  ), cfg_f)
  cfg_obj <- read_statci_config(cfg_f)
  rep_p1 <- statci_audit_project(tmp, config = cfg_obj)
  rep_p2 <- statci_audit_project(tmp, config = cfg_f)
  expect_equal(rep_p1$status, "pass")
  expect_equal(rep_p2$status, "pass")

  bad_cfgs <- list(
    "version: 2",
    "lint:\n  unknown: 1",
    "lint:\n  rules: [nope_rule]",
    "lint:\n  exclude_rules: [nope_rule]",
    "lint:\n  skip_eval_false: 'yes'",
    "manuscript:\n  tol_extra: -1",
    "gate:\n  fail_on: critical",
    "report:\n  formats: [docx]"
  )
  for (idx in seq_along(bad_cfgs)) {
    bf <- file.path(tmp, paste0("b", idx, ".yml"))
    writeLines(bad_cfgs[[idx]], bf)
    expect_error(read_statci_config(bf))
  }

  # 7. R/sarif.R: error on report without lint + unregistered custom rule + Quarto chunk prefix
  expect_error(export_statci_sarif(statci_report()), "does not contain a `lint` component")
  expect_error(export_statci_sarif("not_lint"), "must be a `statci_lint`")
  qmd_s <- stat_lint_text("```{r ch1}\nx <- rnorm(5)\n```\n", format = "qmd", rules = "unseeded_randomness")
  sar_q <- jsonlite::fromJSON(export_statci_sarif(qmd_s), simplifyVector = FALSE)
  expect_match(sar_q$runs[[1L]]$results[[1L]]$message$text, "\\[chunk: ch1\\]")
})
