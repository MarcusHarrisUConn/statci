test_that("statci_report aggregates all pillars, renders HTML/Markdown/JSON, and enforces CI gate", {
  # Build compact results for every pillar
  lint_res <- stat_lint_text("set.seed(1)\nx <- rnorm(20)\ny <- cor(x, x)\n") # info only -> pass
  est_res <- audit_estimator(
    dgp = function() {
      x <- rnorm(60)
      data.frame(x = x, y = 1.0 * x + rnorm(60))
    },
    estimator = function(d) lm(y ~ x, data = d),
    true_params = c(x = 1.0),
    n_sim = 50L,
    seed = 42L
  )
  sbc_res <- audit_sbc(
    prior = function() c(mu = rnorm(1, 0, 2)),
    dgp = function(th) rnorm(15, mean = th[["mu"]], sd = 1),
    posterior_draws = function(y) {
      pv <- 1 / (1 / 4 + length(y))
      rnorm(49, mean = pv * sum(y), sd = sqrt(pv))
    },
    n_sim = 50L,
    n_bins = 5L,
    seed = 42L
  )
  par_res <- check_parallel_rng(
    fun = function(i) rnorm(3),
    X = 1:4,
    cores = c(1L, 2L),
    seed = 42L
  )
  fit <- lm(mpg ~ wt, data = mtcars)
  ms_res <- audit_manuscript_text(
    paste0("<!-- statci: model=fit term=wt -->\nb = ", round(coef(fit)[["wt"]], 2), ", N = 32."),
    models = list(fit = fit)
  )
  set.seed(3)
  df_s <- data.frame(x = rnorm(60))
  df_s$y <- 2 * df_s$x + rnorm(60, sd = 0.4)
  sens_res <- audit_sensitivity(
    df_s,
    fit_fn = function(d) lm(y ~ x, data = d),
    n_boot = 25L,
    max_loo_units = 15L,
    seed = 3L
  )

  rep <- statci_report(
    lint = lint_res,
    estimator = est_res,
    sbc = sbc_res,
    parallel_rng = par_res,
    manuscript = ms_res,
    sensitivity = sens_res,
    title = "Full Pipeline Audit <Test>"
  )

  expect_s3_class(rep, "statci_report")
  expect_equal(nrow(rep$sections), 6L)
  expect_s3_class(as_tibble(rep), "tbl_df")
  expect_invisible(print(rep))

  # Render HTML, Markdown, JSON
  tmp <- tempfile("statci_rep_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  html_path <- file.path(tmp, "audit.html")
  md_path <- file.path(tmp, "audit.md")
  render_statci_report(rep, output_file = html_path, format = "html")
  render_statci_report(rep, output_file = md_path, format = "markdown")

  expect_true(file.exists(html_path))
  expect_true(file.exists(md_path))
  html_txt <- paste(readLines(html_path, warn = FALSE), collapse = "\n")
  expect_match(html_txt, "<!DOCTYPE html>")
  expect_match(html_txt, "Full Pipeline Audit &lt;Test&gt;") # HTML escaped!
  expect_match(html_txt, "AST Statistical Linter")
  expect_match(html_txt, "Adversarial Sensitivity Battery")

  md_txt <- paste(readLines(md_path, warn = FALSE), collapse = "\n")
  expect_match(md_txt, "# Full Pipeline Audit <Test>")
  expect_match(md_txt, "## Executive Summary")

  if (rlang::is_installed("jsonlite")) {
    js <- render_statci_report(rep, format = "json")
    parsed <- jsonlite::fromJSON(js)
    expect_equal(parsed$title, "Full Pipeline Audit <Test>")
    expect_equal(nrow(parsed$sections), 6L)
  }

  # Gate checks
  failing_rep <- statci_report(lint = stat_lint_text("x <- rnorm(10)\n"))
  expect_equal(failing_rep$status, "fail")
  expect_error(statci_check_gate(failing_rep, fail_on = "fail"), "statci CI gate failed")

  warn_rep <- statci_report(lint = stat_lint_text("set.seed(1)\nx <- mean(rnorm(5), na.rm = T)\n"))
  expect_equal(warn_rep$status, "warn")
  expect_true(statci_check_gate(warn_rep, fail_on = "fail"))
  expect_error(statci_check_gate(warn_rep, fail_on = "warn"), "statci CI gate failed")
})

test_that("use_statci_github_action writes valid GitHub Actions YAML and guards overwrite", {
  tmp <- tempfile("statci_gha_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  wf <- use_statci_github_action(
    path = tmp,
    lint_paths = c("R", "analysis"),
    manuscript = "paper.qmd",
    models_path = "models/fits.rds",
    fail_on = "fail"
  )
  expect_true(file.exists(wf))
  txt <- paste(readLines(wf, warn = FALSE), collapse = "\n")
  expect_match(txt, "actions/checkout@v4")
  expect_match(txt, "r-lib/actions/setup-r@v2")
  expect_match(txt, "actions/upload-artifact@v4")
  expect_match(txt, "statci::stat_lint_dir")
  expect_match(txt, "statci::audit_manuscript")
  expect_match(txt, "GITHUB_STEP_SUMMARY")
  expect_false(grepl(":::", txt, fixed = TRUE))

  if (rlang::is_installed("yaml")) {
    parsed <- yaml::yaml.load_file(wf)
    expect_equal(parsed$name, "statci Statistical CI/CD")
    expect_true("statci-audit" %in% names(parsed$jobs))
  }

  # Overwrite guard
  expect_error(use_statci_github_action(path = tmp), "already exists")

  # Custom audit_script mode with overwrite = TRUE
  wf2 <- use_statci_github_action(
    path = tmp,
    audit_script = "scripts/run-statci.R",
    overwrite = TRUE
  )
  txt2 <- paste(readLines(wf2, warn = FALSE), collapse = "\n")
  expect_match(txt2, "shell: bash")
  expect_match(txt2, "Rscript \"scripts/run-statci.R\"")
})
