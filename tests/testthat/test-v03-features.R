# Tests for v0.3.0 features: .statci.yml, orchestrator, 3 new AST rules,
# tables/LaTeX/APA/internal arithmetic manuscript checks, power curve, and SARIF 2.1.0

test_that("17 built-in AST rules include Rules 15-17 with true positives and zero false positives", {
  reset_stat_rules()
  rules <- list_stat_rules()
  expect_gte(nrow(rules), 20L)
  expect_true(all(c(
    "silent_na_coercion",
    "stepwise_selection_unvalidated",
    "in_sample_prediction_eval"
  ) %in% rules$id))

  # Rule 15: silent_na_coercion
  bad_na1 <- stat_lint_text('x <- as.numeric(c("1", "2", "bad"))\n', rules = "silent_na_coercion")
  expect_equal(nrow(bad_na1$findings), 1L)
  bad_na2 <- stat_lint_text('raw <- trimws(readLines("nums.txt"))\nx <- as.numeric(raw)\n', rules = "silent_na_coercion")
  expect_equal(nrow(bad_na2$findings), 1L)
  ok_na1 <- stat_lint_text('raw <- trimws(readLines("nums.txt"))\nx <- as.numeric(raw)\nstopifnot(!anyNA(x))\n', rules = "silent_na_coercion")
  expect_equal(nrow(ok_na1$findings), 0L)
  # Must NOT conflict with Rule 6 recommendation as.numeric(as.character(factor_var))
  ok_na2 <- stat_lint_text('f <- factor(c("10", "20"))\nx <- as.numeric(as.character(f))\n', rules = "silent_na_coercion")
  expect_equal(nrow(ok_na2$findings), 0L)
  ok_na3 <- stat_lint_text("x <- as.numeric(df$score)\n", rules = "silent_na_coercion")
  expect_equal(nrow(ok_na3$findings), 0L)

  # Rule 16: stepwise_selection_unvalidated
  bad_step <- stat_lint_text("fit <- step(lm(y ~ ., data = df))\n", rules = "stepwise_selection_unvalidated")
  expect_equal(nrow(bad_step$findings), 1L)
  bad_aic <- stat_lint_text("fit <- MASS::stepAIC(full_mod, trace = FALSE)\n", rules = "stepwise_selection_unvalidated")
  expect_equal(nrow(bad_aic$findings), 1L)
  ok_step1 <- stat_lint_text("set.seed(1)\nsp <- initial_split(df)\nfit <- step(lm(y ~ ., data = training(sp)))\n", rules = "stepwise_selection_unvalidated")
  expect_equal(nrow(ok_step1$findings), 0L)
  ok_step2 <- stat_lint_text("fit <- lm(y ~ x1 + x2, data = df)\n", rules = "stepwise_selection_unvalidated")
  expect_equal(nrow(ok_step2$findings), 0L)

  # Rule 17: in_sample_prediction_eval
  bad_in1 <- stat_lint_text(
    "fit <- lm(y ~ x, data = df)\npred <- predict(fit, newdata = df)\nerr <- rmse(df$y, pred)\n",
    rules = "in_sample_prediction_eval"
  )
  expect_equal(nrow(bad_in1$findings), 1L)
  bad_in2 <- stat_lint_text(
    "fit <- lm(y ~ x, data = train)\npred <- predict(fit)\nmse <- mean((train$y - pred)^2)\n",
    rules = "in_sample_prediction_eval"
  )
  expect_equal(nrow(bad_in2$findings), 1L)
  ok_out <- stat_lint_text(
    "fit <- lm(y ~ x, data = train)\npred <- predict(fit, newdata = test)\nerr <- mean((test$y - pred)^2)\n",
    rules = "in_sample_prediction_eval"
  )
  expect_equal(nrow(ok_out$findings), 0L)
  ok_plot <- stat_lint_text(
    "fit <- lm(y ~ x, data = df)\nfitted_vals <- predict(fit)\n",
    rules = "in_sample_prediction_eval"
  )
  expect_equal(nrow(ok_plot$findings), 0L)
})

test_that("bundled inst/examples clean-analysis.R passes all 17 rules and flawed-analysis.R fails", {
  clean_ex <- system.file("examples", "clean-analysis.R", package = "statci")
  flawed_ex <- system.file("examples", "flawed-analysis.R", package = "statci")
  skip_if(!nzchar(clean_ex) || !nzchar(flawed_ex))

  res_clean <- stat_lint(clean_ex)
  expect_equal(nrow(res_clean$findings), 0L)
  expect_equal(res_clean$status, "pass")

  res_flawed <- stat_lint(flawed_ex)
  expect_equal(res_flawed$status, "fail")
  expect_gte(nrow(res_flawed$findings), 6L)
  expect_true(all(c(
    "absolute_path_in_script",
    "preprocessing_before_split",
    "uncentered_mlm_interaction",
    "missing_convergence_check",
    "in_sample_prediction_eval",
    "unadjusted_multiple_testing",
    "short_logical_literal"
  ) %in% res_flawed$findings$rule))
})

test_that("read_statci_config validates schema strictly and blocks eval.expr YAML attacks", {
  tmp <- tempfile("statci_cfg_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # Valid config
  cfg_path <- file.path(tmp, ".statci.yml")
  writeLines(c(
    "version: 1",
    "lint:",
    "  paths: [R]",
    "  exclude_rules: [cor_without_use_arg]",
    "  severity_overrides:",
    "    short_logical_literal: error",
    "gate:",
    "  fail_on: fail",
    "report:",
    "  title: Custom CI Audit",
    "  output_dir: out",
    "  formats: [html, markdown, json, sarif]"
  ), cfg_path)
  cfg <- read_statci_config(cfg_path)
  expect_s3_class(cfg, "statci_config")
  expect_equal(cfg$lint$severity_overrides[["short_logical_literal"]], "error")
  expect_invisible(print(cfg))

  # Reject unknown top-level key
  bad1 <- file.path(tmp, "bad1.yml")
  writeLines("unknown_section:\n  foo: 1", bad1)
  expect_error(read_statci_config(bad1), "Unknown top-level key")

  # Reject invalid severity override
  bad2 <- file.path(tmp, "bad2.yml")
  writeLines("lint:\n  severity_overrides:\n    short_logical_literal: critical", bad2)
  expect_error(read_statci_config(bad2), "Invalid severity")

  # Reject path traversal in lint.paths
  bad3 <- file.path(tmp, "bad3.yml")
  writeLines("lint:\n  paths: ['../outside']", bad3)
  expect_error(read_statci_config(bad3), "traversal")

  # Verify eval.expr = FALSE: !expr does NOT execute arbitrary R code
  marker <- file.path(tmp, "pwned.txt")
  bad_expr <- file.path(tmp, "expr.yml")
  writeLines(paste0("report:\n  title: !expr writeLines('pwn', '", marker, "')"), bad_expr)
  tryCatch(read_statci_config(bad_expr), error = function(e) NULL)
  expect_false(file.exists(marker))
})

test_that("statci_audit_project orchestrates linting, severity overrides, manuscript, reports, and SARIF", {
  tmp <- tempfile("statci_proj_test_")
  dir.create(file.path(tmp, "R"), recursive = TRUE)
  dir.create(file.path(tmp, "models"), recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  writeLines(c("set.seed(1)", "x <- rnorm(20)", "m <- mean(x)"), file.path(tmp, "R", "ok.R"))
  fit <- lm(mpg ~ wt, data = mtcars)
  saveRDS(list(primary = fit), file.path(tmp, "models", "fits.rds"))
  writeLines(c(
    "<!-- statci: model=primary term=wt -->",
    "Effect: b = -5.34, SE = 0.56, t(30) = -9.56, 95% CI [-6.49, -4.20], p < .001, N = 32."
  ), file.path(tmp, "paper.qmd"))
  writeLines(c(
    "version: 1",
    "lint:",
    "  paths: [R]",
    "manuscript:",
    "  path: paper.qmd",
    "  models: models/fits.rds",
    "gate:",
    "  fail_on: fail",
    "report:",
    "  output_dir: reports",
    "  formats: [html, markdown, json, sarif]"
  ), file.path(tmp, ".statci.yml"))

  rep <- statci_audit_project(tmp, enforce_gate = TRUE)
  expect_s3_class(rep, "statci_report")
  expect_equal(rep$status, "pass")
  expect_equal(length(rep$artifacts), 4L)
  expect_true(all(file.exists(rep$artifacts)))

  # Verify severity override elevates warning -> error and fails gate
  writeLines(c("set.seed(1)", "m <- mean(rnorm(10), na.rm = T)"), file.path(tmp, "R", "ok.R"))
  writeLines(c(
    "version: 1",
    "lint:",
    "  paths: [R]",
    "  severity_overrides:",
    "    short_logical_literal: error",
    "gate:",
    "  fail_on: fail"
  ), file.path(tmp, ".statci.yml"))
  expect_error(statci_audit_project(tmp, enforce_gate = TRUE), "statci CI gate failed")
})

test_that("audit_manuscript parses Markdown tables, LaTeX .tex files, APA F/chi2, and internal arithmetic", {
  fit <- lm(mpg ~ wt, data = mtcars)

  # 1. Markdown table (pass + drift detection)
  md_ok <- paste(
    "<!-- statci: model=primary -->",
    "| Term | b | SE | t | 95% CI | p |",
    "|---|---|---|---|---|---|",
    "| wt | -5.34 | 0.56 | -9.56 | [-6.49, -4.20] | < .001 |",
    sep = "\n"
  )
  res_tbl_ok <- audit_manuscript_text(md_ok, models = list(primary = fit))
  expect_equal(res_tbl_ok$status, "pass")
  expect_gte(nrow(res_tbl_ok$checks), 6L)

  md_bad <- paste(
    "<!-- statci: model=primary -->",
    "| Term | b | SE | p |",
    "|---|---|---|---|",
    "| wt | +5.34 | 0.56 | < .001 |",
    sep = "\n"
  )
  res_tbl_bad <- audit_manuscript_text(md_bad, models = list(primary = fit))
  expect_equal(res_tbl_bad$status, "fail")
  expect_true("sign_flip" %in% res_tbl_bad$checks$issue_type)

  # 2. APA F(df1, df2) and chi2(df)
  ct_mat <- matrix(c(30, 10, 12, 28), nrow = 2)
  chi_fit <- suppressWarnings(chisq.test(ct_mat, correct = FALSE))
  chi_str <- formatC(as.numeric(chi_fit$statistic), digits = 2, format = "f")
  apa_res <- audit_manuscript_text(
    paste0(
      "<!-- statci: model=lm1 -->\nModel fit: F(1, 30) = 91.38, R^2 = 0.75, N = 32.\n",
      "<!-- statci: model=chi1 -->\nContingency test: chi2(1) = ", chi_str, ", p < .001."
    ),
    models = list(lm1 = fit, chi1 = chi_fit)
  )
  expect_equal(apa_res$status, "pass")
  expect_true(all(c("f_stat", "chisq") %in% apa_res$checks$metric))

  # 3. LaTeX .tex manuscript & tabular environment
  tex_ex <- system.file("examples", "sample-manuscript.tex", package = "statci")
  if (nzchar(tex_ex)) {
    res_tex <- audit_manuscript(tex_ex, models = list(primary = fit))
    expect_equal(res_tex$format, "tex")
    expect_equal(res_tex$status, "pass")
    expect_gte(nrow(res_tex$checks), 10L)
  }

  # 4. Standalone internal arithmetic checks (models = NULL)
  ic_ok <- audit_manuscript_text(
    "Effect: b = 0.42, SE = 0.11, t(98) = 3.82, 95% CI [0.20, 0.64].",
    models = NULL
  )
  expect_equal(ic_ok$status, "pass")
  expect_gte(nrow(ic_ok$internal_checks), 3L)

  ic_bad <- audit_manuscript_text(
    "Effect: b = 0.42, SE = 0.11, t(98) = 1.10, 95% CI [0.50, 0.90].",
    models = NULL
  )
  expect_equal(ic_bad$status, "fail")
  expect_true("internal_t_ratio" %in% ic_bad$internal_checks$check_type[ic_bad$internal_checks$status == "fail"])
  expect_true("internal_ci_midpoint" %in% ic_bad$internal_checks$check_type[ic_bad$internal_checks$status == "fail"])

  # Inverted CI bounds caught
  ic_inv <- audit_manuscript_text("Effect: b = 0.42, 95% CI [0.64, 0.20].", models = NULL)
  expect_equal(ic_inv$status, "fail")
  expect_true("internal_ci_order" %in% ic_inv$internal_checks$check_type)
})

test_that("audit_power_curve passes calibrated curves and catches non-monotonicity and underpowering", {
  pc_ok <- audit_power_curve(
    dgp = function(d) rnorm(40, mean = d, sd = 1),
    test_fn = function(y) t.test(y, mu = 0),
    effect_grid = c(0, 0.25, 0.50, 0.75),
    n_sim = 80L,
    target_effect = 0.50,
    target_power = 0.80,
    seed = 42L
  )
  expect_s3_class(pc_ok, "statci_power_curve")
  expect_equal(pc_ok$status, "pass")
  expect_equal(pc_ok$monotonicity$status, "pass")
  expect_equal(pc_ok$null_check$status, "pass")
  expect_true(is.finite(pc_ok$mde) && pc_ok$mde > 0.2 && pc_ok$mde < 0.7)
  expect_s3_class(as_tibble(pc_ok), "tbl_df")
  expect_invisible(print(pc_ok))
  if (rlang::is_installed("ggplot2")) {
    expect_s3_class(plot(pc_ok), "ggplot")
  }

  # Non-monotonic broken test (rejects at 0.3, never rejects at 0.8) -> FAIL
  pc_bad_mono <- audit_power_curve(
    dgp = function(d) d,
    test_fn = function(d) if (d == 0.3) 0.001 else if (d == 0.8) 0.90 else stats::runif(1),
    effect_grid = c(0, 0.3, 0.8),
    n_sim = 40L,
    seed = 7L
  )
  expect_equal(pc_bad_mono$monotonicity$status, "fail")
  expect_equal(pc_bad_mono$status, "fail")

  # Underpowered at target_effect -> FAIL
  pc_under <- audit_power_curve(
    dgp = function(d) rnorm(10, mean = d, sd = 2),
    test_fn = function(y) t.test(y, mu = 0),
    effect_grid = c(0, 0.2),
    n_sim = 80L,
    target_effect = 0.2,
    target_power = 0.80,
    seed = 7L
  )
  expect_equal(pc_under$target_check$status, "fail")
  expect_equal(pc_under$status, "fail")

  # Integrates into statci_report HTML/Markdown/JSON
  rep <- statci_report(power_curve = pc_ok)
  expect_equal(rep$status, "pass")
  expect_match(render_statci_report(rep, format = "html"), "Power-Curve Audit", fixed = TRUE)
  expect_match(render_statci_report(rep, format = "markdown"), "Power-Curve Audit", fixed = TRUE)
})

test_that("export_statci_sarif emits valid SARIF 2.1.0 JSON and integrates with GHA", {
  tmp <- tempfile("statci_sarif_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  r_file <- file.path(tmp, "bad.R")
  writeLines(c("x <- rnorm(10)", "tt <- t.test(x, var.equal = F)"), r_file)
  lres <- stat_lint(r_file)

  sarif_path <- file.path(tmp, "statci.sarif")
  export_statci_sarif(lres, output_file = sarif_path, base_uri = tmp)
  expect_true(file.exists(sarif_path))

  parsed <- jsonlite::fromJSON(sarif_path, simplifyVector = FALSE)
  expect_equal(parsed$version, "2.1.0")
  expect_match(parsed[["$schema"]], "sarif-2.1.0.json")
  run1 <- parsed$runs[[1L]]
  expect_equal(run1$tool$driver$name, "statci")
  expect_gte(length(run1$tool$driver$rules), 20L)
  expect_equal(length(run1$results), 2L)
  expect_equal(run1$results[[1L]]$locations[[1L]]$physicalLocation$artifactLocation$uri, "bad.R")
  expect_equal(run1$results[[1L]]$locations[[1L]]$physicalLocation$region$startLine, 1L)

  # Also works directly from a statci_report
  rep <- statci_report(lint = lres)
  s_rep <- export_statci_sarif(rep)
  expect_equal(jsonlite::fromJSON(s_rep)$version, "2.1.0")

  # use_statci_github_action(upload_sarif = TRUE)
  wf <- use_statci_github_action(path = tmp, upload_sarif = TRUE)
  wf_txt <- paste(readLines(wf, warn = FALSE), collapse = "\n")
  expect_match(wf_txt, "security-events: write", fixed = TRUE)
  expect_match(wf_txt, "export_statci_sarif", fixed = TRUE)
  expect_match(wf_txt, "github/codeql-action/upload-sarif@v3", fixed = TRUE)
})
