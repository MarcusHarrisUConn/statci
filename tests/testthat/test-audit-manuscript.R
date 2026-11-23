test_that("audit_manuscript_text passes rounded matches and catches mismatches, sign flips, and stale N", {
  set.seed(123)
  df <- data.frame(x1 = rnorm(120), x2 = rnorm(120))
  df$y <- 1 + 0.55 * df$x1 - 0.35 * df$x2 + rnorm(120, sd = 0.6)
  fit <- lm(y ~ x1 + x2, data = df)
  sm <- summary(fit)$coefficients
  ci <- confint(fit)

  b1 <- round(sm["x1", 1], 2)
  se1 <- round(sm["x1", 2], 2)
  ci1_l <- round(ci["x1", 1], 2)
  ci1_u <- round(ci["x1", 2], 2)
  r2 <- round(summary(fit)$r.squared, 2)

  good_txt <- paste(
    "---",
    "title: Study",
    "---",
    "```{r}",
    "# Code block with fake numbers must be ignored: b = 999.99, N = 5",
    "```",
    paste0("<!-- statci: model=fit term=x1 -->"),
    paste0("Predictor x1 had a strong positive effect (b = ", b1, ", SE = ", se1,
           ", 95% CI [", ci1_l, ", ", ci1_u, "], p < .001)."),
    paste0("Overall model fit was solid (N = 120, R^2 = ", r2, ")."),
    sep = "\n"
  )

  res_good <- audit_manuscript_text(good_txt, models = list(fit = fit))
  expect_s3_class(res_good, "statci_manuscript_audit")
  expect_equal(res_good$status, "pass")
  expect_equal(res_good$summary$n_fail, 0L)
  expect_gte(res_good$summary$n_claims, 6L)

  # Stale manuscript: sign flip on x2, wrong N, violated p < .001, and rounding error
  b2_actual <- sm["x2", 1]
  b2_flipped <- round(abs(b2_actual), 2) # positive instead of negative!
  bad_txt <- paste(
    "<!-- statci: model=fit term=x2 -->",
    paste0("Predictor x2 showed a positive slope (b = ", b2_flipped, ", SE = 0.99)."),
    "The final sample included N = 150 participants.",
    sep = "\n"
  )

  res_bad <- audit_manuscript_text(bad_txt, models = list(fit = fit))
  expect_equal(res_bad$status, "fail")
  expect_true("sign_flip" %in% res_bad$checks$issue_type)
  expect_true("value_mismatch" %in% res_bad$checks$issue_type)
  expect_true("stale_sample_size" %in% res_bad$checks$issue_type)
  expect_equal(res_bad$summary$n_sign_flips, 1L)
  expect_invisible(print(res_bad))
  expect_s3_class(as_tibble(res_bad), "tbl_df")
})

test_that("audit_manuscript_text supports Quarto inline spans, rounding warnings, htest, and tidy data frames", {
  # Tidy data frame with known exact numbers
  tidy_df <- data.frame(
    term = c("trt", "age"),
    estimate = c(0.424, -0.153),
    std.error = c(0.112, 0.041),
    p.value = c(0.0002, 0.042),
    conf.low = c(0.204, -0.233),
    conf.high = c(0.644, -0.073),
    n = c(200, 200),
    stringsAsFactors = FALSE
  )

  # 0.424 reported as 0.43 -> off by 0.006 (> 0.005 half-unit, <= 0.015 one-half unit) -> WARN rounding_discrepancy
  # p < .001 for age (actual 0.042) -> FAIL inequality_violation
  span_txt <- paste(
    '[Treatment effect: b = 0.43, SE = 0.11, p < .001]{.statci model="tbl" term="trt"}',
    '[Age effect: b = -0.15, p < .001]{.statci model="tbl" term="age"}',
    sep = "\n"
  )

  res <- audit_manuscript_text(span_txt, models = list(tbl = tidy_df))
  expect_true("rounding_discrepancy" %in% res$checks$issue_type)
  expect_equal(res$checks$status[res$checks$issue_type == "rounding_discrepancy"], "warn")
  expect_true("inequality_violation" %in% res$checks$issue_type)
  expect_equal(res$status, "fail")

  # htest (cor.test)
  set.seed(7)
  x <- rnorm(50)
  y <- 0.6 * x + rnorm(50, sd = 0.5)
  ct <- cor.test(x, y)
  r_rep <- round(unname(ct$estimate), 2)
  ht_res <- audit_manuscript_text(
    paste0("Correlation was strong (r = ", r_rep, ", p < .001, N = 50)."),
    models = ct
  )
  expect_equal(ht_res$status, "pass")
})

test_that("audit_manuscript works with .qmd files and saved .rds model directories", {
  fit <- lm(mpg ~ wt, data = mtcars)
  tmp <- tempfile("statci_ms_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  rds_dir <- file.path(tmp, "models")
  dir.create(rds_dir)
  saveRDS(fit, file.path(rds_dir, "car_model.rds"))

  qmd_path <- file.path(tmp, "paper.qmd")
  writeLines(
    c(
      "<!-- statci: model=car_model term=wt -->",
      paste0("Heavier cars had lower mileage (*b* = ", round(coef(fit)[["wt"]], 2), ", *N* = 32).")
    ),
    qmd_path
  )

  res <- audit_manuscript(qmd_path, models = rds_dir)
  expect_equal(res$status, "pass")
  expect_equal(res$models_checked, "car_model")
})

test_that("audit_manuscript handles lmerMod objects when lme4 is available", {
  skip_if_not_installed("lme4")
  fm <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  b_days <- round(lme4::fixef(fm)[["Days"]], 2)
  res <- audit_manuscript_text(
    paste0("<!-- statci: model=fm term=Days -->\nEach day increased RT (b = ", b_days, ", N = 180)."),
    models = list(fm = fm)
  )
  expect_equal(res$status, "pass")
})

test_that("audit_manuscript handles Unicode minus, LaTeX math, glm odds ratios, and untagged prose matching", {
  set.seed(15)
  x <- rnorm(200)
  y <- rbinom(200, 1, plogis(-0.5 + 0.7 * x))
  gfit <- glm(y ~ x, family = binomial())
  b_x <- round(coef(gfit)[["x"]], 2)
  or_x <- round(exp(coef(gfit)[["x"]]), 2)

  # Untagged prose with LaTeX math and odds ratio
  txt <- paste0(
    "In logistic regression, x increased event odds ($\\beta = ", b_x,
    "$, OR = ", or_x, ", N = 200)."
  )
  res <- audit_manuscript_text(txt, models = gfit)
  expect_equal(res$status, "pass")
  expect_true(all(c("estimate", "or", "n") %in% res$checks$metric))

  # Unicode minus sign (\u2212) matching negative coefficient
  lm_fit <- lm(mpg ~ wt, data = mtcars)
  wt_abs <- formatC(abs(coef(lm_fit)[["wt"]]), digits = 2, format = "f")
  uni_txt <- paste0("Weight reduced fuel economy (b = \u2212", wt_abs, ", N = 32).")
  res_u <- audit_manuscript_text(uni_txt, models = lm_fit)
  expect_equal(res_u$status, "pass")
})
