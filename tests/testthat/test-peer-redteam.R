# Permanent regression tests for all 20 independent peer red-team findings
# (Phase 2 Remediation, v0.5.0: H1-H6, M1-M10, L1-L4)

# ==============================================================================
# H1: Symlinked ancestor directory write escape in validate_contained_dest()
# ==============================================================================
test_that("H1: symlinked ancestor write escapes are blocked in GHA and project audit", {
  skip_on_os("windows")

  proj <- tempfile("statci_h1_proj_")
  outside <- tempfile("statci_h1_out_")
  dir.create(file.path(proj, "R"), recursive = TRUE)
  dir.create(outside, recursive = TRUE)
  withr::defer(unlink(c(proj, outside), recursive = TRUE, force = TRUE))

  ok_link <- isTRUE(suppressWarnings(file.symlink(outside, file.path(proj, "out_link"))))
  skip_if_not(ok_link, "symlink creation unsupported")

  # 1. use_statci_github_action must refuse writing through symlinked parent dir
  expect_error(
    use_statci_github_action(path = proj, file = "out_link/pwned.yaml"),
    "escapes|symlink|outside"
  )
  expect_false(file.exists(file.path(outside, "pwned.yaml")))

  # 2. Nested non-existent subdir through symlinked ancestor must also be rejected
  expect_error(
    use_statci_github_action(path = proj, file = "out_link/nested/deep/pwned.yaml"),
    "escapes|symlink|outside"
  )
  expect_false(file.exists(file.path(outside, "nested", "deep", "pwned.yaml")))

  # 3. statci_audit_project report.output_dir through symlinked ancestor must fail
  writeLines("x <- rnorm(10)", file.path(proj, "R", "a.R"))
  writeLines(
    c(
      "version: 1",
      "lint:",
      "  paths: [R]",
      "report:",
      "  output_dir: out_link/subdir",
      "  formats: [html, sarif]"
    ),
    file.path(proj, ".statci.yml")
  )
  expect_error(statci_audit_project(proj), "escapes|symlink|outside")
  expect_false(file.exists(file.path(outside, "subdir", "statci-report.html")))
  expect_false(file.exists(file.path(outside, "subdir", "statci.sarif")))

  # 4. Dangling symlink component must also be rejected
  dangling_target <- file.path(outside, "does_not_exist")
  suppressWarnings(file.symlink(dangling_target, file.path(proj, "dangling_link")))
  expect_error(
    use_statci_github_action(path = proj, file = "dangling_link/ci.yaml"),
    "symbolic link|symlink|escapes|outside"
  )
})

# ==============================================================================
# H2: Symlinked lint.paths external directory read & code-snippet leakage
# ==============================================================================
test_that("H2: symlinked lint.paths escaping project root are rejected", {
  skip_on_os("windows")

  proj <- tempfile("statci_h2_proj_")
  ext <- tempfile("statci_h2_ext_")
  dir.create(proj, recursive = TRUE)
  dir.create(ext, recursive = TRUE)
  withr::defer(unlink(c(proj, ext), recursive = TRUE, force = TRUE))

  writeLines("secret_key <- 'TOP_SECRET'; x <- rnorm(5)", file.path(ext, "leak.R"))
  ok_link <- isTRUE(suppressWarnings(file.symlink(ext, file.path(proj, "R_ext_link"))))
  skip_if_not(ok_link, "symlink creation unsupported")

  writeLines(
    c("version: 1", "lint:", "  paths: [R_ext_link]"),
    file.path(proj, ".statci.yml")
  )

  expect_error(statci_audit_project(proj), "escapes|symlink|outside")
})

# ==============================================================================
# H3: Complete silent CI bypass in stat_lint_diff() via added ++ / deleted --
# ==============================================================================
test_that("H3: stat_lint_diff() does not terminate active hunk on added +++ or deleted ---", {
  d <- tempfile("statci_h3_")
  dir.create(d, recursive = TRUE)
  withr::defer(unlink(d, recursive = TRUE, force = TRUE))

  # 1. Added line starting with "++ " emits "+++ ..." inside hunk
  writeLines(
    c(
      'hdr <- "',
      "++ innocent_text",
      '"',
      "x <- rnorm(100)",
      "y <- mean(x, na.rm = T)"
    ),
    file.path(d, "bypass_add.R")
  )
  diff_add <- paste(
    "diff --git a/bypass_add.R b/bypass_add.R",
    "--- /dev/null",
    "+++ b/bypass_add.R",
    "@@ -0,0 +1,5 @@",
    '+hdr <- "',
    "+++ innocent_text",
    '+"',
    "+x <- rnorm(100)",
    "+y <- mean(x, na.rm = T)",
    sep = "\n"
  )
  res_add <- stat_lint_diff(path = d, diff_text = diff_add)
  expect_gt(nrow(res_add$findings), 0L)
  expect_true("unseeded_randomness" %in% res_add$findings$rule)
  expect_true("short_logical_literal" %in% res_add$findings$rule)
  expect_identical(res_add$status, "fail")

  # 2. Deleted line starting with "-- " emits "--- ..." inside hunk
  writeLines(
    c(
      'hdr <- "',
      "kept line",
      '"',
      "z <- runif(50)"
    ),
    file.path(d, "bypass_del.R")
  )
  diff_del <- paste(
    "diff --git a/bypass_del.R b/bypass_del.R",
    "--- a/bypass_del.R",
    "+++ b/bypass_del.R",
    "@@ -1,3 +1,4 @@",
    ' hdr <- "',
    "--- removed_comment",
    "+kept line",
    ' "',
    "+z <- runif(50)",
    sep = "\n"
  )
  res_del <- stat_lint_diff(path = d, diff_text = diff_del)
  expect_true("unseeded_randomness" %in% res_del$findings$rule)
  expect_identical(res_del$status, "fail")
})

# ==============================================================================
# H4: Raw HTML / <script> XSS injection in Markdown multiverse reports
# ==============================================================================
test_that("H4: build_markdown_report() neutralizes backticks and HTML tags in multiverse param", {
  p_evil <- "`<script>alert(1)</script>`"
  mv <- audit_multiverse(
    data = data.frame(x = 1:20, y = 1:20),
    decisions = list(d = c("a", "b")),
    fit_fn = function(d, spec) {
      list(
        estimate = stats::setNames(1.0, p_evil),
        se = stats::setNames(0.1, p_evil)
      )
    },
    param = p_evil
  )

  md <- render_statci_report(statci_report(multiverse = mv), format = "markdown")
  expect_false(grepl("<script>alert(1)</script>", md, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;", md, fixed = TRUE))
})

# ==============================================================================
# H5: in_sample_prediction_eval false positive & pipe evasions
# ==============================================================================
test_that("H5: in_sample_prediction_eval allows positional test_df and catches piped leakage", {
  # Clean out-of-sample evaluation with positional 2nd arg -> 0 FP
  fp <- stat_lint_text(
    "fit <- lm(y ~ x, data = train_df)\np <- predict(fit, test_df)\nrmse(test_df$y, p)\n",
    rules = "in_sample_prediction_eval"
  )
  expect_equal(nrow(fp$findings), 0L)

  # Positional 2nd arg equal to training data IS still in-sample leakage
  tp_pos <- stat_lint_text(
    "fit <- lm(y ~ x, data = train_df)\np <- predict(fit, train_df)\nrmse(train_df$y, p)\n",
    rules = "in_sample_prediction_eval"
  )
  expect_equal(nrow(tp_pos$findings), 1L)

  # Pipe evasions must all be caught
  ev1 <- stat_lint_text(
    "fit <- lm(y ~ x, data = df)\np <- fit |> predict()\nrmse(df$y, p)\n",
    rules = "in_sample_prediction_eval"
  )
  ev2 <- stat_lint_text(
    "fit <- lm(y ~ x, data = df)\np <- fit |> fitted()\nrmse(df$y, p)\n",
    rules = "in_sample_prediction_eval"
  )
  ev3 <- stat_lint_text(
    "fit <- df |> lm(y ~ x, data = _)\np <- predict(fit)\nrmse(df$y, p)\n",
    rules = "in_sample_prediction_eval"
  )
  expect_equal(nrow(ev1$findings), 1L)
  expect_equal(nrow(ev2$findings), 1L)
  expect_equal(nrow(ev3$findings), 1L)
})

# ==============================================================================
# H6: audit_multiverse() reserved fork names & NA fork choices
# ==============================================================================
test_that("H6: audit_multiverse() rejects reserved fork names and preserves NA levels", {
  withr::with_seed(1L, {
    df <- data.frame(x = stats::rnorm(60), c1 = stats::rnorm(60), y = stats::rnorm(60))
  })

  # 1. Reserved column names rejected cleanly
  expect_error(
    audit_multiverse(
      df,
      decisions = list(se = c("classical", "robust"), cov = c("none", "c1")),
      fit_fn = function(d, spec) stats::lm(y ~ x, data = d),
      param = "x"
    ),
    "reserved"
  )
  expect_error(
    audit_multiverse(
      df,
      decisions = list(error = c("a", "b")),
      fit_fn = function(d, spec) stats::lm(y ~ x, data = d),
      param = "x"
    ),
    "reserved"
  )
  expect_error(
    audit_multiverse(
      df,
      decisions = list(estimate = c("a", "b")),
      fit_fn = function(d, spec) stats::lm(y ~ x, data = d),
      param = "x"
    ),
    "reserved"
  )

  # 2. NA decision level is preserved in fork_sensitivity & plot()
  mv2 <- audit_multiverse(
    df,
    decisions = list(trim = c(NA_real_, 0.05, 0.10)),
    fit_fn = function(d, spec) {
      val <- if (is.na(spec$trim)) 10 else 1
      list(estimate = c(x = val), se = c(x = 0.1))
    },
    param = "x"
  )
  expect_equal(mv2$fork_sensitivity$n_levels, 3L)
  expect_equal(mv2$fork_sensitivity$median_range, 9, tolerance = 1e-8)
  expect_equal(mv2$fork_sensitivity$eta_squared, 1.0, tolerance = 1e-8)
  expect_identical(mv2$summary$most_influential_fork, "trim")

  plt <- plot(mv2)
  expect_s3_class(plt, "ggplot")
})

# ==============================================================================
# M1: stat_lint_diff() spaces, parentheses, UTF-8, quoted diff_text, plain diff -u
# ==============================================================================
test_that("M1: stat_lint_diff() handles spaces, parens, UTF-8, quoted diff, and plain diff -u", {
  d <- tempfile("statci_m1_")
  dir.create(file.path(d, "b"), recursive = TRUE)
  withr::defer(unlink(d, recursive = TRUE, force = TRUE))

  # 1. diff_text with spaces, parens, and octal-quoted UTF-8
  writeLines("x <- rnorm(10)", file.path(d, "my analysis.R"))
  writeLines("x <- rnorm(10)", file.path(d, "model(v2).R"))
  writeLines("x <- rnorm(10)", file.path(d, "caf\u00e9.R"))
  writeLines("x <- rnorm(10)", file.path(d, "b", "sim.R"))

  diff_special <- paste(
    'diff --git "a/my analysis.R" "b/my analysis.R"',
    "--- /dev/null",
    '+++ "b/my analysis.R"',
    "@@ -0,0 +1,1 @@",
    "+x <- rnorm(10)",
    "diff --git a/model(v2).R b/model(v2).R",
    "--- /dev/null",
    "+++ b/model(v2).R",
    "@@ -0,0 +1,1 @@",
    "+x <- rnorm(10)",
    'diff --git "a/caf\\303\\251.R" "b/caf\\303\\251.R"',
    "--- /dev/null",
    '+++ "b/caf\\303\\251.R"',
    "@@ -0,0 +1,1 @@",
    "+x <- rnorm(10)",
    sep = "\n"
  )
  res_sp <- stat_lint_diff(path = d, diff_text = diff_special, rules = "unseeded_randomness")
  expect_equal(nrow(res_sp$findings), 3L)

  # 2. Plain `diff -u` where real directory is named `b/` (no `diff --git` or `a/` prefix)
  diff_plain <- paste(
    "--- b/sim.R.orig",
    "+++ b/sim.R",
    "@@ -0,0 +1,1 @@",
    "+x <- rnorm(10)",
    sep = "\n"
  )
  res_plain <- stat_lint_diff(path = d, diff_text = diff_plain, rules = "unseeded_randomness")
  expect_equal(nrow(res_plain$findings), 1L)

  # 3. Real git repo with spaces and UTF-8 staged files
  skip_if(!nzchar(Sys.which("git")), "git not available")
  gdir <- tempfile("statci_m1_git_")
  dir.create(gdir, recursive = TRUE)
  withr::defer(unlink(gdir, recursive = TRUE, force = TRUE))

  withr::with_dir(gdir, {
    system2("git", c("init", "-q"))
    system2("git", c("config", "user.email", "a@b.com"))
    system2("git", c("config", "user.name", "Tester"))
    writeLines("x <- 1", "base.R")
    system2("git", c("add", "base.R"))
    system2("git", c("commit", "-q", "-m", "init"))

    writeLines("x <- rnorm(10)", "my analysis.R")
    writeLines("y <- runif(10)", "caf\u00e9.R")
    system2("git", c("add", shQuote("my analysis.R"), shQuote("caf\u00e9.R")))

    res_git <- stat_lint_diff(path = gdir, staged = TRUE, rules = "unseeded_randomness")
    expect_equal(nrow(res_git$findings), 2L)
    expect_equal(length(res_git$diff_meta$changed_files), 2L)
  })
})

# ==============================================================================
# M2: read_statci_config() validation of .inf, .na, and YAML sequences
# ==============================================================================
test_that("M2: read_statci_config() rejects .inf, .na booleans, and YAML sequences", {
  f <- tempfile(fileext = ".yml")
  withr::defer(unlink(f))

  writeLines(c("version: 1", "manuscript:", "  tol_extra: .inf"), f)
  expect_error(read_statci_config(f), "tol_extra|finite")

  writeLines(c("version: 1", "manuscript:", "  tol_extra: .na"), f)
  expect_error(read_statci_config(f), "tol_extra|finite")

  writeLines(c("version: 1", "manuscript:", "  warn_rounding: .na"), f)
  expect_error(read_statci_config(f), "warn_rounding")

  writeLines(c("version: 1", "manuscript:", "  check_internal: .na"), f)
  expect_error(read_statci_config(f), "check_internal")

  writeLines(c("- a: 1", "- b: 2"), f)
  expect_error(read_statci_config(f), "mapping")

  writeLines(c("version: 1", "lint:", "  - a", "  - b"), f)
  expect_error(read_statci_config(f), "mapping")
})

# ==============================================================================
# M3: export_statci_sarif() NA startLine/startColumn, RFC-3986 encoding, host path strip
# ==============================================================================
test_that("M3: export_statci_sarif() emits integer >= 1, RFC-3986 URIs, and no host paths", {
  # 1. NA line/col -> integer 1L (never string "NA")
  r_na <- new_stat_rule(
    "na_line_test",
    "NA Line",
    check = function(ctx) list(list(line = NA_integer_, col = NA_integer_, message = "x"))
  )
  j1 <- export_statci_sarif(stat_lint_text("x <- 1", rules = list(r_na)))
  parsed1 <- jsonlite::fromJSON(j1, simplifyVector = FALSE)
  reg <- parsed1$runs[[1L]]$results[[1L]]$locations[[1L]]$physicalLocation$region
  expect_identical(reg$startLine, 1L)
  expect_identical(reg$startColumn, 1L)

  # 2. Percent-encoding of #, ?, %, space
  d <- tempfile("statci_m3_")
  dir.create(d, recursive = TRUE)
  withr::defer(unlink(d, recursive = TRUE, force = TRUE))

  f <- file.path(d, "file #1?a=b%.R")
  writeLines("x <- rnorm(10)", f)
  res_lint <- stat_lint(f, rules = "unseeded_randomness")

  j2 <- export_statci_sarif(res_lint, base_uri = d)
  parsed2 <- jsonlite::fromJSON(j2, simplifyVector = FALSE)
  uri2 <- parsed2$runs[[1L]]$results[[1L]]$locations[[1L]]$physicalLocation$artifactLocation$uri
  expect_identical(uri2, "file%20%231%3Fa%3Db%25.R")

  # 3. base_uri = NULL must not leak absolute host paths
  j3 <- export_statci_sarif(res_lint, base_uri = NULL)
  parsed3 <- jsonlite::fromJSON(j3, simplifyVector = FALSE)
  r3 <- parsed3$runs[[1L]]$results[[1L]]
  uri3 <- r3$locations[[1L]]$physicalLocation$artifactLocation$uri
  fp3 <- r3$partialFingerprints$primaryLocationLineHash
  expect_false(startsWith(uri3, "/"))
  expect_false(grepl("statci_m3_", uri3, fixed = TRUE))
  expect_false(grepl("statci_m3_", fp3, fixed = TRUE))
  expect_identical(uri3, "file%20%231%3Fa%3Db%25.R")
})

# ==============================================================================
# M4: missing_drop_false_matrix scalar lookups (0 FP) & single-row/dynamic slices (TP)
# ==============================================================================
test_that("M4: missing_drop_false_matrix ignores scalar lookups and flags X[1, ] & X[, 1:p]", {
  # Zero false positives on scalar element lookups
  expect_equal(nrow(stat_lint_text("f <- function(m, i) m[i, 1]\n", rules = "missing_drop_false_matrix")$findings), 0L)
  expect_equal(nrow(stat_lint_text("f <- function(fit) coef(summary(fit))[2, 4]\n", rules = "missing_drop_false_matrix")$findings), 0L)
  expect_equal(nrow(stat_lint_text("f <- function(df) df[1, 'col']\n", rules = "missing_drop_false_matrix")$findings), 0L)

  # Clean constant multi-column slice & drop = FALSE -> 0 FP
  expect_equal(nrow(stat_lint_text("f <- function(X) X[, 1:3]\n", rules = "missing_drop_false_matrix")$findings), 0L)
  expect_equal(nrow(stat_lint_text("f <- function(X) X[1, , drop = FALSE]\n", rules = "missing_drop_false_matrix")$findings), 0L)

  # True positives: single-row slice X[1, ], single-col slice X[, j], dynamic X[, 1:p]
  expect_equal(nrow(stat_lint_text("f <- function(X) X[1, ]\n", rules = "missing_drop_false_matrix")$findings), 1L)
  expect_equal(nrow(stat_lint_text("f <- function(X, j) X[, j]\n", rules = "missing_drop_false_matrix")$findings), 1L)
  expect_equal(nrow(stat_lint_text("f <- function(X, p) X[, 1:p]\n", rules = "missing_drop_false_matrix")$findings), 1L)
})

# ==============================================================================
# M5: Pipe evasions in silent_na_coercion, silent_factor_drop, preprocessing_before_split
# ==============================================================================
test_that("M5: pipe evasions are caught across NA coercion, factor drop, and preprocessing", {
  expect_equal(
    nrow(stat_lint_text("x <- c('1', 'a') |> as.numeric()\n", rules = "silent_na_coercion")$findings),
    1L
  )
  expect_equal(
    nrow(stat_lint_text("s <- c('1', 'a')\nx <- s %>% as.numeric()\n", rules = "silent_na_coercion")$findings),
    1L
  )
  expect_equal(
    nrow(stat_lint_text("f <- factor(c('10', '20'))\nx <- f |> as.numeric()\n", rules = "silent_factor_drop")$findings),
    1L
  )
  expect_equal(
    nrow(stat_lint_text("df2 <- df |> mutate(x = scale(x))\nsp <- initial_split(df2)\n", rules = "preprocessing_before_split")$findings),
    1L
  )
})

# ==============================================================================
# M6: Six additional AST rule evasions
# ==============================================================================
test_that("M6: all six AST rule evasions are caught", {
  # 1. unseeded_randomness: RNGkind alone + higher-order lapply/do.call
  expect_equal(
    nrow(stat_lint_text("RNGkind(\"L'Ecuyer-CMRG\")\nx <- rnorm(100)\n", rules = "unseeded_randomness")$findings),
    1L
  )
  expect_equal(
    nrow(stat_lint_text("xs <- lapply(1:5, rnorm)\n", rules = "unseeded_randomness")$findings),
    1L
  )
  expect_equal(
    nrow(stat_lint_text("x <- do.call(rnorm, list(100))\n", rules = "unseeded_randomness")$findings),
    1L
  )

  # 2. equality_on_float: string literal inside index no longer disables rule
  expect_equal(
    nrow(stat_lint_text("ok <- mean(df[['x']]) == 0.5\n", rules = "equality_on_float")$findings),
    1L
  )
  # Direct top-level string comparison remains 0 FP
  expect_equal(
    nrow(stat_lint_text("ok <- x == '0.5'\n", rules = "equality_on_float")$findings),
    0L
  )

  # 3. short_logical_literal: lexical function parameter T does not disable top-level T
  expect_equal(
    nrow(stat_lint_text("m <- mean(x, na.rm = T)\nhelper <- function(T) T + 1\n", rules = "short_logical_literal")$findings),
    1L
  )
  # Inside the helper itself, parameter T is still allowed (0 FP)
  expect_equal(
    nrow(stat_lint_text("helper <- function(T) T + 1\n", rules = "short_logical_literal")$findings),
    0L
  )

  # 4. unverified_merge_cardinality: unassigned piped join + unrelated nrow(other)
  expect_equal(
    nrow(stat_lint_text("a |> left_join(b, by = 'id') |> lm(y ~ x, data = _)\nn <- nrow(other)\n", rules = "unverified_merge_cardinality")$findings),
    1L
  )

  # 5. stepwise_selection_unvalidated: substring `fold` in fit_fold_change + unrelated sample()
  expect_equal(
    nrow(stat_lint_text("fit_fold_change <- lm(y ~ ., data = df)\nm <- step(fit_fold_change)\n", rules = "stepwise_selection_unvalidated")$findings),
    1L
  )
  expect_equal(
    nrow(stat_lint_text("idx <- sample(nrow(other_df), 5)\nfit <- lm(y ~ ., data = df)\nm <- step(fit)\n", rules = "stepwise_selection_unvalidated")$findings),
    1L
  )

  # 6. implicit_na_rm_masking: positional mean(x, 0, TRUE), quantile(), trailing unrelated is.na(other)
  expect_equal(
    nrow(stat_lint_text("m <- mean(x, 0, TRUE)\n", rules = "implicit_na_rm_masking")$findings),
    1L
  )
  expect_equal(
    nrow(stat_lint_text("q <- quantile(x, 0.5, na.rm = TRUE)\n", rules = "implicit_na_rm_masking")$findings),
    1L
  )
  expect_equal(
    nrow(stat_lint_text("m <- mean(x, na.rm = TRUE)\nchk <- any(is.na(other_vec))\n", rules = "implicit_na_rm_masking")$findings),
    1L
  )
})

# ==============================================================================
# M7: audit_manuscript() 90%/99% CIs, empty .rds, regex model names, NA tol, B = 1000
# ==============================================================================
test_that("M7: audit_manuscript() handles 90%/99% CIs, empty rds, regex names, NA tol, B=1000", {
  # 1. Valid 90% and 99% CIs pass internal arithmetic
  r90 <- audit_manuscript_text("Effect: b = 0.50, SE = 0.10, t(100) = 5.00, 90% CI [0.33, 0.67].")
  r99 <- audit_manuscript_text("Effect: b = 0.50, SE = 0.10, t(100) = 5.00, 99% CI [0.24, 0.76].")
  expect_identical(r90$status, "pass")
  expect_identical(r99$status, "pass")

  # 2. Empty list .rds handled cleanly without crash
  f_rds <- tempfile(fileext = ".rds")
  withr::defer(unlink(f_rds))
  saveRDS(list(), f_rds)
  r_empty <- audit_manuscript_text("Effect: b = 0.50, SE = 0.10, t(100) = 5.00.", models = f_rds)
  expect_s3_class(r_empty, "statci_manuscript_audit")
  expect_identical(r_empty$status, "pass")

  # 3. Model names containing regex metacharacters do not crash
  fit1 <- stats::lm(mpg ~ wt, data = mtcars)
  fit2 <- stats::lm(mpg ~ hp, data = mtcars)
  r_rx <- audit_manuscript_text(
    "Model m[1: wt had b = -5.34.",
    models = list("m[1" = fit1, "m(2" = fit2)
  )
  expect_s3_class(r_rx, "statci_manuscript_audit")
  expect_identical(r_rx$status, "pass")

  # 4. Non-finite tol_extra rejected cleanly
  expect_error(
    audit_manuscript_text("b = 0.50, SE = 0.10, t = 5.00.", tol_extra = NA_real_),
    "tol_extra"
  )
  expect_error(
    audit_manuscript_text("b = 0.50, SE = 0.10, t = 5.00.", tol_extra = Inf),
    "tol_extra"
  )

  # 5. Bootstrap prose "B = 1000 bootstrap samples" is not flagged as regression estimate
  r_boot <- audit_manuscript_text(
    "We ran B = 1000 bootstrap samples for wt.",
    models = list(fit = fit1)
  )
  expect_equal(nrow(r_boot$checks), 0L)
  expect_identical(r_boot$status, "pass")
})

# ==============================================================================
# M8: audit_power_curve() reject = NA, invalid p-values, NA args, dup grid, negative MDE
# ==============================================================================
test_that("M8: audit_power_curve() handles NA reject, bad p-values, NA args, dup & negative grids", {
  # 1. list(reject = NA) counts as failed replication (fail_rate = 1, n_valid = 0)
  pc1 <- audit_power_curve(
    dgp = function(d) d,
    test_fn = function(y) list(reject = NA),
    effect_grid = c(0, 1),
    n_sim = 10L
  )
  expect_true(all(pc1$curve$n_valid == 0L))
  expect_true(all(pc1$curve$fail_rate == 1.0))

  # 2. Out-of-[0,1] / non-finite p-values count as invalid (n_valid = 0)
  pc2 <- audit_power_curve(
    dgp = function(d) d,
    test_fn = function(y) if (y == 0) -0.5 else Inf,
    effect_grid = c(0, 1),
    n_sim = 10L
  )
  expect_true(all(pc2$curve$n_valid == 0L))
  expect_true(all(pc2$curve$fail_rate == 1.0))

  # 3. NA/invalid scalar arguments rejected cleanly
  expect_error(
    audit_power_curve(function(d) stats::rnorm(10, d), function(y) stats::t.test(y), c(0, 0.5), n_sim = 10L, target_effect = NA_real_),
    "target_effect"
  )
  expect_error(
    audit_power_curve(function(d) stats::rnorm(10, d), function(y) stats::t.test(y), c(0, 0.5), n_sim = 10L, alpha = NA_real_),
    "alpha"
  )
  expect_error(
    audit_power_curve(function(d) stats::rnorm(10, d), function(y) stats::t.test(y), c(0.5, 0.5), n_sim = 10L),
    "distinct|effect_grid"
  )

  # 4. Negative-only effect grid computes finite MDE magnitude on |effect|
  pc_neg <- audit_power_curve(
    dgp = function(d) stats::rnorm(40, mean = d, sd = 1),
    test_fn = function(y) stats::t.test(y),
    effect_grid = c(0, -0.3, -0.6, -1.0),
    n_sim = 40L,
    seed = 42L
  )
  expect_true(is.finite(pc_neg$mde))
  expect_gt(pc_neg$mde, 0.2)
  expect_lt(pc_neg$mde, 1.0)
})

# ==============================================================================
# M9: audit_sensitivity() collinear lm, NA clusters in LOCO, numeric cluster bootstrap
# ==============================================================================
test_that("M9: audit_sensitivity() handles collinear lm, NA clusters, and numeric clusters", {
  withr::with_seed(9L, {
    # 1. Collinear lm with NA coefficient does not crash
    df_col <- data.frame(x1 = 1:20, x2 = 1:20, y = 1:20 + stats::rnorm(20))
    s_col <- audit_sensitivity(df_col, function(d) stats::lm(y ~ x1 + x2, data = d), n_boot = 10L, seed = 1L)
    expect_s3_class(s_col, "statci_sensitivity")

    # 2. Numeric cluster_col preserves numeric type in cluster bootstrap
    df_cl <- data.frame(cl = rep(1:6, each = 5), x = stats::rnorm(30))
    df_cl$y <- 0.8 * df_cl$x + stats::rnorm(30, sd = 0.2)
    s_cl <- audit_sensitivity(
      df_cl,
      function(d) {
        stopifnot(is.numeric(d$cl))
        stats::lm(y ~ x, data = d)
      },
      cluster_col = "cl",
      n_boot = 10L,
      seed = 2L
    )
    expect_equal(s_cl$bootstrap$fail_rate, 0)

    # 3. NA cluster IDs do not turn rows into all-NA during LOCO
    df_cl_na <- df_cl
    df_cl_na$cl[1:2] <- NA_integer_
    s_na <- audit_sensitivity(
      df_cl_na,
      function(d) {
        stopifnot(!any(is.na(d$y)))
        stats::lm(y ~ x, data = d)
      },
      cluster_col = "cl",
      winsor_probs = NULL,
      missingness_rates = NULL,
      n_boot = 0L,
      seed = 3L
    )
    expect_s3_class(s_na, "statci_sensitivity")
    expect_equal(nrow(s_na$loo_details), 6L)
    expect_true(all(is.finite(s_na$loo_details$estimate)))
    expect_true(all(is.finite(s_na$results$perturbed)))
  })
})

# ==============================================================================
# M10: audit_estimator() NA true_params & zero-SE/NA-CI fail; audit_sbc() varying L abort
# ==============================================================================
test_that("M10: audit_estimator() fails degenerate SE/CI and audit_sbc() rejects varying L", {
  # 1. Non-finite true_params rejected cleanly
  expect_error(
    audit_estimator(
      dgp = function() stats::rnorm(20, 1),
      estimator = function(y) c(mu = mean(y)),
      true_params = c(mu = NA_real_),
      n_sim = 10L
    ),
    "true_params"
  )

  # 2. Zero SE & all-NA CIs must FAIL (not silently pass)
  est <- audit_estimator(
    dgp = function() stats::rnorm(20, mean = 1),
    estimator = function(y) {
      list(
        estimate = c(mu = mean(y)),
        se = c(mu = 0),
        ci_lower = c(mu = NA_real_),
        ci_upper = c(mu = NA_real_)
      )
    },
    true_params = c(mu = 1),
    n_sim = 20L,
    seed = 7L
  )
  expect_identical(est$recovery$se_ratio_status, "fail")
  expect_identical(est$recovery$coverage_status, "fail")
  expect_identical(est$status, "fail")

  # 3. Varying posterior draw count L in audit_sbc() aborts cleanly
  k <- 0L
  expect_error(
    audit_sbc(
      prior = function() c(mu = stats::rnorm(1)),
      dgp = function(th) stats::rnorm(10, th[["mu"]], 1),
      posterior_draws = function(y) {
        k <<- k + 1L
        stats::rnorm(if (k == 1L) 10L else 50L, mean(y), 0.3)
      },
      n_sim = 10L,
      n_bins = 5L,
      seed = 11L
    ),
    "inconsistent|draw count"
  )
})

# ==============================================================================
# L1: Quarto/Rmd doc fences, tilde fences, blank line before #| eval: false
# ==============================================================================
test_that("L1: Quarto/Rmd chunk extractor handles outer fences, tilde fences, and blank lines", {
  # 1. Illustrative ```{r} inside ````markdown fence -> 0 FP
  doc_fence <- "````markdown\n```{r}\nx <- rnorm(10)\n```\n````\n"
  expect_equal(
    nrow(stat_lint_text(doc_fence, format = "qmd", rules = "unseeded_randomness")$findings),
    0L
  )

  # 2. Tilde-fenced ~~~{r} chunk IS linted
  tilde_chunk <- "~~~{r}\nx <- rnorm(10)\n~~~\n"
  expect_equal(
    nrow(stat_lint_text(tilde_chunk, format = "qmd", rules = "unseeded_randomness")$findings),
    1L
  )

  # 3. Blank line before #| eval: false is skipped when skip_eval_false = TRUE
  blank_opt <- "```{r}\n\n#| eval: false\nx <- rnorm(10)\n```\n"
  expect_equal(
    nrow(stat_lint_text(blank_opt, format = "qmd", rules = "unseeded_randomness", skip_eval_false = TRUE)$findings),
    0L
  )
})

# ==============================================================================
# L2: audit_multiverse() pre-expand.grid max_specs guard & finite alpha check
# ==============================================================================
test_that("L2: audit_multiverse() rejects huge grids before expand.grid and rejects alpha = NA", {
  # 4^15 = 1,073,741,824 combinations would OOM expand.grid(); must abort immediately
  huge <- stats::setNames(rep(list(c("a", "b", "c", "d")), 15L), paste0("f", 1:15))
  expect_error(
    audit_multiverse(mtcars, huge, function(d, s) stats::lm(mpg ~ wt, data = d), max_specs = 10L),
    "max_specs"
  )

  expect_error(
    audit_multiverse(mtcars, list(a = 1:2), function(d, s) stats::lm(mpg ~ wt, data = d), alpha = NA_real_),
    "alpha"
  )
})

# ==============================================================================
# L3: audit_manuscript() GFM tables without outer | & impossible p / R^2 bounds
# ==============================================================================
test_that("L3: audit_manuscript() parses pipe-free outer GFM tables and flags impossible p / R^2", {
  fit <- stats::lm(mpg ~ wt, data = mtcars)

  # 1. GFM table omitting outer | borders is parsed and catches mismatch
  r_tbl <- audit_manuscript_text(
    "<!-- statci: model=fit -->\nTerm | b | SE\n---|---:|---:\nwt | 99.99 | 0.56",
    models = list(fit = fit)
  )
  expect_gt(nrow(r_tbl$checks), 0L)
  expect_identical(r_tbl$status, "fail")

  # 2. Impossible p = 1.50 and R^2 = 1.40 fail internal arithmetic bounds
  r_bounds <- audit_manuscript_text(
    "Effect: b = 0.50, SE = 0.10, t(100) = 5.00, p = 1.50, R^2 = 1.40."
  )
  expect_identical(r_bounds$status, "fail")
  expect_true("internal_p_bounds" %in% r_bounds$internal_checks$check_type)
  expect_true("internal_r2_bounds" %in% r_bounds$internal_checks$check_type)
})

# ==============================================================================
# L4: audit_multiverse() most_influential_fork checks eta_squared OR median_range
# ==============================================================================
test_that("L4: audit_multiverse() reports most_influential_fork when eta_squared > 0 with 0 median_range", {
  mv <- audit_multiverse(
    mtcars,
    decisions = list(f1 = c("A", "B"), f2 = c("1", "2", "3")),
    fit_fn = function(d, s) {
      v <- if (s$f1 == "A") c(0, 2, 2)[[as.integer(s$f2)]] else c(4, 2, 2)[[as.integer(s$f2)]]
      list(estimate = c(b = v), se = c(b = 0.1))
    }
  )
  expect_gt(mv$fork_sensitivity$eta_squared[1L], 0.1)
  expect_identical(mv$summary$most_influential_fork, "f1")
})
