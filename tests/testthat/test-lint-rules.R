test_that("list_stat_rules returns 13 built-in rules and supports filtering", {
  reset_stat_rules()
  rules <- list_stat_rules()
  expect_s3_class(rules, "tbl_df")
  expect_gte(nrow(rules), 13L)
  expect_true(all(c("id", "severity", "category", "title", "description", "recommendation") %in% names(rules)))
  expect_true(all(list_stat_rules(severity = "error")$severity == "error"))
  expect_true(all(list_stat_rules(category = "modeling")$category == "modeling"))
})

test_that("Rule 1: unseeded_randomness catches unseeded calls and passes seeded code", {
  bad <- stat_lint_text("x <- rnorm(100)\ny <- sample(1:10, 5)\n", rules = "unseeded_randomness")
  expect_equal(bad$status, "fail")
  expect_equal(nrow(bad$findings), 2L)
  expect_equal(bad$findings$line, c(1L, 2L))

  # Calling set.seed AFTER rnorm still flags rnorm
  late_seed <- stat_lint_text("x <- rnorm(10)\nset.seed(42)\ny <- runif(10)\n", rules = "unseeded_randomness")
  expect_equal(nrow(late_seed$findings), 1L)
  expect_equal(late_seed$findings$line, 1L)

  # Clean seeded script
  ok1 <- stat_lint_text("set.seed(42)\nx <- rnorm(100)\ny <- sample(1:10, 5)\n", rules = "unseeded_randomness")
  expect_equal(nrow(ok1$findings), 0L)
  expect_equal(ok1$status, "pass")

  # Clean withr::with_seed wrapper
  ok2 <- stat_lint_text("x <- withr::with_seed(42, rnorm(100))\n", rules = "unseeded_randomness")
  expect_equal(nrow(ok2$findings), 0L)

  # Inside a reusable function definition should not trigger unseeded_randomness
  ok3 <- stat_lint_text("sim <- function(n) rnorm(n)\n", rules = "unseeded_randomness")
  expect_equal(nrow(ok3$findings), 0L)
})

test_that("Rule 2: unsafe_parallel_rng catches unsafe mclapply/parLapply/foreach/future and passes safe code", {
  bad_mc <- stat_lint_text("set.seed(1)\nres <- parallel::mclapply(1:4, function(i) rnorm(5), mc.cores = 2)\n", rules = "unsafe_parallel_rng")
  expect_equal(nrow(bad_mc$findings), 1L)

  bad_mc_false <- stat_lint_text("RNGkind(\"L'Ecuyer-CMRG\")\nres <- mclapply(1:4, rnorm, mc.cores = 2, mc.set.seed = FALSE)\n", rules = "unsafe_parallel_rng")
  expect_equal(nrow(bad_mc_false$findings), 1L)

  ok_mc <- stat_lint_text("RNGkind(\"L'Ecuyer-CMRG\")\nset.seed(1)\nres <- parallel::mclapply(1:4, rnorm, mc.cores = 2)\n", rules = "unsafe_parallel_rng")
  expect_equal(nrow(ok_mc$findings), 0L)

  bad_par <- stat_lint_text("cl <- makeCluster(2)\nres <- parLapply(cl, 1:4, rnorm)\n", rules = "unsafe_parallel_rng")
  expect_equal(nrow(bad_par$findings), 1L)

  ok_par <- stat_lint_text("cl <- makeCluster(2)\nclusterSetRNGStream(cl, 123)\nres <- parLapply(cl, 1:4, rnorm)\n", rules = "unsafe_parallel_rng")
  expect_equal(nrow(ok_par$findings), 0L)

  bad_fe <- stat_lint_text("res <- foreach(i = 1:4) %dopar% rnorm(1)\n", rules = "unsafe_parallel_rng")
  expect_equal(nrow(bad_fe$findings), 1L)

  ok_fe1 <- stat_lint_text("res <- foreach(i = 1:4) %dorng% rnorm(1)\n", rules = "unsafe_parallel_rng")
  expect_equal(nrow(ok_fe1$findings), 0L)

  ok_fe2 <- stat_lint_text("doRNG::registerDoRNG(123)\nres <- foreach(i = 1:4) %dopar% rnorm(1)\n", rules = "unsafe_parallel_rng")
  expect_equal(nrow(ok_fe2$findings), 0L)

  bad_fut <- stat_lint_text("res <- future_lapply(1:4, rnorm)\n", rules = "unsafe_parallel_rng")
  expect_equal(nrow(bad_fut$findings), 1L)

  ok_fut <- stat_lint_text("res <- future_lapply(1:4, rnorm, future.seed = TRUE)\n", rules = "unsafe_parallel_rng")
  expect_equal(nrow(ok_fut$findings), 0L)
})

test_that("Rule 3: preprocessing_before_split catches leakage and passes post-split scaling", {
  bad <- stat_lint_text(
    "set.seed(1)\ndf$x <- scale(df$x)\nidx <- sample(nrow(df), 50)\ntrain <- df[idx, ]\n",
    rules = "preprocessing_before_split"
  )
  expect_equal(nrow(bad$findings), 1L)
  expect_equal(bad$findings$line, 2L)

  bad_pca <- stat_lint_text(
    "set.seed(1)\nX_s <- prcomp(df)\nsp <- initial_split(df)\n",
    rules = "preprocessing_before_split"
  )
  expect_equal(nrow(bad_pca$findings), 1L)

  ok <- stat_lint_text(
    "set.seed(1)\nidx <- sample(nrow(df), 50)\ntrain <- df[idx, ]\ntest <- df[-idx, ]\ntrain$x <- scale(train$x)\n",
    rules = "preprocessing_before_split"
  )
  expect_equal(nrow(ok$findings), 0L)
})

test_that("Rule 4: uncentered_mlm_interaction catches raw interactions and passes centered predictors", {
  bad <- stat_lint_text(
    "fit <- lme4::lmer(y ~ x * w + (1 + x | school), data = df)\n",
    rules = "uncentered_mlm_interaction"
  )
  expect_equal(nrow(bad$findings), 1L)
  expect_match(bad$findings$message, "`x`")
  expect_match(bad$findings$message, "`w`")

  # Naming convention (_c, _gmc)
  ok1 <- stat_lint_text(
    "fit <- lmer(y ~ x_c * w_gmc + (1 + x_c | school), data = df)\n",
    rules = "uncentered_mlm_interaction"
  )
  expect_equal(nrow(ok1$findings), 0L)

  # Inline scale()
  ok2 <- stat_lint_text(
    "fit <- lmer(y ~ scale(x) * scale(w) + (1 | school), data = df)\n",
    rules = "uncentered_mlm_interaction"
  )
  expect_equal(nrow(ok2$findings), 0L)

  # Prior centering assignment
  ok3 <- stat_lint_text(
    "df$x <- df$x - ave(df$x, df$school)\ndf$w <- df$w - mean(df$w)\nfit <- lmer(y ~ x * w + (1 + x | school), data = df)\n",
    rules = "uncentered_mlm_interaction"
  )
  expect_equal(nrow(ok3$findings), 0L)

  # Plain lm (not multilevel) should not trigger
  ok4 <- stat_lint_text("fit <- lm(y ~ x * w, data = df)\n", rules = "uncentered_mlm_interaction")
  expect_equal(nrow(ok4$findings), 0L)
})

test_that("Rule 5: silent_listwise_mismatch catches unharmonized nested comparisons and passes complete-case data", {
  bad <- stat_lint_text(
    "m1 <- lm(y ~ x1, data = df)\nm2 <- lm(y ~ x1 + x2, data = df)\nanova(m1, m2)\nAIC(m1, m2)\n",
    rules = "silent_listwise_mismatch"
  )
  expect_equal(nrow(bad$findings), 2L)

  ok1 <- stat_lint_text(
    "df_cc <- na.omit(df)\nm1 <- lm(y ~ x1, data = df_cc)\nm2 <- lm(y ~ x1 + x2, data = df_cc)\nanova(m1, m2)\n",
    rules = "silent_listwise_mismatch"
  )
  expect_equal(nrow(ok1$findings), 0L)

  ok2 <- stat_lint_text(
    "m1 <- lm(y ~ x1, data = df, na.action = na.fail)\nm2 <- lm(y ~ x1 + x2, data = df, na.action = na.fail)\nanova(m1, m2)\n",
    rules = "silent_listwise_mismatch"
  )
  expect_equal(nrow(ok2$findings), 0L)
})

test_that("Rule 6: silent_factor_drop catches factor numeric coercion and un-dropped subset modeling", {
  bad1 <- stat_lint_text(
    "f <- factor(c('10', '20', '30'))\nx <- as.numeric(f)\n",
    rules = "silent_factor_drop"
  )
  expect_equal(nrow(bad1$findings), 1L)

  ok1 <- stat_lint_text(
    "f <- factor(c('10', '20', '30'))\nx <- as.numeric(as.character(f))\n",
    rules = "silent_factor_drop"
  )
  expect_equal(nrow(ok1$findings), 0L)

  bad2 <- stat_lint_text(
    "sub <- subset(df, group != 'control')\nfit <- lm(y ~ group, data = sub)\n",
    rules = "silent_factor_drop"
  )
  expect_equal(nrow(bad2$findings), 1L)

  ok2 <- stat_lint_text(
    "sub <- droplevels(subset(df, group != 'control'))\nfit <- lm(y ~ group, data = sub)\n",
    rules = "silent_factor_drop"
  )
  expect_equal(nrow(ok2$findings), 0L)
})

test_that("Rule 7: equality_on_float catches float == and ignores integer/string checks", {
  bad <- stat_lint_text(
    "if (0.1 + 0.2 == 0.3) print('eq')\nif (mean(x) != 0) print('nz')\n",
    rules = "equality_on_float"
  )
  expect_equal(nrow(bad$findings), 2L)

  ok <- stat_lint_text(
    "if (isTRUE(all.equal(0.1 + 0.2, 0.3))) print('eq')\nif (nrow(df) == 0L) print('empty')\nif (a == 'foo') print('str')\n",
    rules = "equality_on_float"
  )
  expect_equal(nrow(ok$findings), 0L)
})

test_that("Rule 8: unadjusted_multiple_testing catches unadjusted test loops and passes adjusted ones", {
  bad <- stat_lint_text(
    "pvals <- numeric(10)\nfor (i in 1:10) {\n  pvals[i] <- t.test(x[, i], y[, i])$p.value\n}\n",
    rules = "unadjusted_multiple_testing"
  )
  expect_equal(nrow(bad$findings), 1L)

  ok <- stat_lint_text(
    "pvals <- sapply(1:10, function(i) t.test(x[, i], y[, i])$p.value)\nadj <- p.adjust(pvals, method = 'BH')\n",
    rules = "unadjusted_multiple_testing"
  )
  expect_equal(nrow(ok$findings), 0L)
})

test_that("Rule 9: missing_convergence_check catches unchecked lmer/optim and passes checked fits", {
  bad1 <- stat_lint_text("fit <- lmer(y ~ x + (1 | g), data = df)\n", rules = "missing_convergence_check")
  expect_equal(nrow(bad1$findings), 1L)

  ok1 <- stat_lint_text("fit <- lmer(y ~ x + (1 | g), data = df)\nif (isSingular(fit)) warning('singular')\n", rules = "missing_convergence_check")
  expect_equal(nrow(ok1$findings), 0L)

  bad2 <- stat_lint_text("opt <- optim(c(0, 0), fn)\n", rules = "missing_convergence_check")
  expect_equal(nrow(bad2$findings), 1L)

  ok2 <- stat_lint_text("opt <- optim(c(0, 0), fn)\nstopifnot(opt$convergence == 0)\n", rules = "missing_convergence_check")
  expect_equal(nrow(ok2$findings), 0L)
})

test_that("Rule 10: global_seed_override_in_fn catches bare set.seed inside functions", {
  bad <- stat_lint_text("f <- function(n) {\n  set.seed(1)\n  rnorm(n)\n}\n", rules = "global_seed_override_in_fn")
  expect_equal(nrow(bad$findings), 1L)
  expect_equal(bad$findings$line, 2L)

  ok1 <- stat_lint_text("f <- function(n) {\n  withr::local_seed(1)\n  rnorm(n)\n}\n", rules = "global_seed_override_in_fn")
  expect_equal(nrow(ok1$findings), 0L)

  ok2 <- stat_lint_text(
    "f <- function(n) {\n  old <- .Random.seed\n  on.exit(assign('.Random.seed', old, envir = globalenv()))\n  set.seed(1)\n  rnorm(n)\n}\n",
    rules = "global_seed_override_in_fn"
  )
  expect_equal(nrow(ok2$findings), 0L)
})

test_that("Rule 11: short_logical_literal catches T/F but ignores user variables T <- 100 and df$T", {
  bad <- stat_lint_text("m <- mean(x, na.rm = T)\nt.test(x, y, var.equal = F)\n", rules = "short_logical_literal")
  expect_equal(nrow(bad$findings), 2L)

  ok <- stat_lint_text("T <- 100\nseq_len(T)\ndf$T\nmean(x, na.rm = TRUE)\n", rules = "short_logical_literal")
  expect_equal(nrow(ok$findings), 0L)
})

test_that("Rule 12: cor_without_use_arg catches missing use= and passes explicit use=", {
  bad <- stat_lint_text("r <- cor(x, y)\ncv <- cov(mat)\n", rules = "cor_without_use_arg")
  expect_equal(nrow(bad$findings), 2L)
  expect_equal(bad$status, "pass") # info severity does not fail/warn

  ok <- stat_lint_text("r <- cor(x, y, use = 'complete.obs')\ncv <- cov(mat, use = 'pairwise.complete.obs')\n", rules = "cor_without_use_arg")
  expect_equal(nrow(ok$findings), 0L)
})

test_that("Rule 13: absolute_path_in_script catches personal absolute paths and passes relative paths", {
  bad <- stat_lint_text(
    "df <- read.csv('/Users/alice/Desktop/data.csv')\nsetwd('C:/Projects/study')\n",
    rules = "absolute_path_in_script"
  )
  expect_equal(nrow(bad$findings), 2L)

  ok <- stat_lint_text(
    "df <- read.csv(file.path('data', 'clean.csv'))\n",
    rules = "absolute_path_in_script"
  )
  expect_equal(nrow(ok$findings), 0L)
})

test_that("Quarto/Rmd chunk extraction preserves exact line numbers, chunk labels, and cross-chunk state", {
  qmd <- paste(
    "---",
    "title: Test Analysis",
    "---",
    "",
    "Some intro text.",
    "",
    "```{r setup}",
    "set.seed(42)",
    "```",
    "",
    "Now run simulation and bad float check:",
    "",
    "```{r}",
    "#| label: analysis",
    "x <- rnorm(50)",
    "flag <- mean(x) == 0.5",
    "```",
    sep = "\n"
  )
  res <- stat_lint_text(qmd, format = "qmd")
  # Line 15 rnorm(50) is protected by line 8 set.seed(42) in chunk `setup`!
  expect_false("unseeded_randomness" %in% res$findings$rule)
  # Line 16 equality_on_float IS caught with exact original line number 16 and chunk label `analysis`
  expect_equal(nrow(res$findings), 1L)
  expect_equal(res$findings$rule, "equality_on_float")
  expect_equal(res$findings$line, 16L)
  expect_equal(res$findings$chunk, "analysis")
})

test_that("Inline suppressions, custom rules, parse_error fallback, and stat_lint_dir work", {
  # Inline suppressions
  code <- paste(
    "x <- rnorm(10) # statci-ignore: unseeded_randomness",
    "# statci-ignore-next-line",
    "y <- mean(x) == 0.5",
    sep = "\n"
  )
  res <- stat_lint_text(code)
  expect_equal(nrow(res$findings), 0L)

  # Custom rule registration
  custom <- new_stat_rule(
    id = "no_attach_test",
    title = "No attach()",
    severity = "warning",
    check = function(ctx) {
      out <- list()
      for (u in ctx$units) {
        nds <- xml2::xml_find_all(u$xml, ".//SYMBOL_FUNCTION_CALL[text() = 'attach']")
        for (nd in nds) {
          out[[length(out) + 1L]] <- list(
            line = as.integer(xml2::xml_attr(nd, "line1")) + u$line_offset,
            col = as.integer(xml2::xml_attr(nd, "col1")),
            message = "Avoid attach()"
          )
        }
      }
      out
    }
  )
  register_stat_rule(custom)
  on.exit(reset_stat_rules(), add = TRUE)
  res_c <- stat_lint_text("attach(mtcars)\n", rules = "no_attach_test")
  expect_equal(nrow(res_c$findings), 1L)
  expect_equal(res_c$findings$rule, "no_attach_test")

  # Multi-chunk qmd with one syntax-error chunk still lints valid chunk
  qmd_err <- paste(
    "```{r broken}",
    "x <- (1 +",
    "```",
    "",
    "```{r valid}",
    "y <- rnorm(5)",
    "```",
    sep = "\n"
  )
  res_e <- stat_lint_text(qmd_err, format = "qmd", rules = "unseeded_randomness")
  expect_true("parse_error" %in% res_e$findings$rule)
  expect_true("unseeded_randomness" %in% res_e$findings$rule)
  expect_equal(res_e$findings$line[res_e$findings$rule == "unseeded_randomness"], 6L)

  # stat_lint_dir on temp dir
  tmp <- tempfile("statci_lint_dir_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines(c("set.seed(1)", "x <- rnorm(10)"), file.path(tmp, "clean.R"))
  writeLines(c("x <- rnorm(10)"), file.path(tmp, "dirty.R"))
  dres <- stat_lint_dir(tmp, rules = "unseeded_randomness")
  expect_equal(dres$summary$n_files, 2L)
  expect_equal(nrow(dres$findings), 1L)
  expect_s3_class(as_tibble(dres), "tbl_df")
  expect_invisible(print(dres))

  # skip_eval_false in .qmd on disk
  qmd_file <- file.path(tmp, "doc.qmd")
  writeLines(c(
    "```{r demo, eval=FALSE}",
    "x <- rnorm(10)",
    "```",
    "```{r real}",
    "set.seed(1)",
    "y <- rnorm(10)",
    "```"
  ), qmd_file)
  res_skip <- stat_lint(qmd_file, skip_eval_false = TRUE, rules = "unseeded_randomness")
  expect_equal(nrow(res_skip$findings), 0L)
  res_noskip <- stat_lint(qmd_file, skip_eval_false = FALSE, rules = "unseeded_randomness")
  expect_equal(nrow(res_noskip$findings), 1L)
  expect_equal(res_noskip$findings$line, 2L)

  # exclude_rules and unregister_stat_rule
  res_ex <- stat_lint_text("x <- rnorm(10)\n", exclude_rules = "unseeded_randomness")
  expect_equal(nrow(res_ex$findings), 0L)
  expect_true(unregister_stat_rule("no_attach_test"))
  expect_false(unregister_stat_rule("no_attach_test"))
})
