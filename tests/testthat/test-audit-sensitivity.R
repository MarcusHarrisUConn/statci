test_that("audit_sensitivity passes a robust linear model across all four stress families", {
  set.seed(42)
  n <- 120L
  df <- data.frame(
    x = rnorm(n),
    z = rnorm(n)
  )
  df$y <- 1.0 + 2.0 * df$x - 1.0 * df$z + rnorm(n, sd = 0.4)

  res <- audit_sensitivity(
    data = df,
    fit_fn = function(d) lm(y ~ x + z, data = d),
    n_boot = 40L,
    max_loo_units = 25L,
    seed = 42L
  )

  expect_s3_class(res, "statci_sensitivity")
  expect_equal(res$status, "pass")
  expect_true(all(c("winsorization", "influence", "missingness", "bootstrap") %in% res$results$category))
  expect_equal(nrow(res$bootstrap), 2L)
  expect_true(all(res$bootstrap$sign_consistency >= 0.95))
  expect_s3_class(as_tibble(res), "tbl_df")
  expect_invisible(print(res))
})

test_that("audit_sensitivity flags an outlier-driven fragile model with sign flip", {
  set.seed(11)
  n <- 40L
  x <- rnorm(n, mean = 0, sd = 0.5)
  # True slope is negative (-0.6), but 1 extreme outlier makes OLS positive!
  y <- -0.6 * x + rnorm(n, sd = 0.25)
  x[1] <- 12
  y[1] <- 18
  df <- data.frame(x = x, y = y)

  res <- audit_sensitivity(
    data = df,
    fit_fn = function(d) lm(y ~ x, data = d),
    winsor_probs = c(0.05),
    missingness_rates = NULL,
    n_boot = 30L,
    max_loo_units = 40L,
    seed = 99L
  )

  expect_equal(res$status, "fail")
  expect_true(any(res$results$sign_flip, na.rm = TRUE))
  loo_row <- res$results[res$results$category == "influence", ]
  expect_equal(loo_row$status, "fail")
  expect_true(grepl("Most influential unit: 1", loo_row$detail))
})

test_that("audit_sensitivity supports leave-one-cluster-out (LOCO) and cluster bootstrap", {
  set.seed(5)
  n_cl <- 12L
  df <- do.call(rbind, lapply(seq_len(n_cl), function(g) {
    x <- rnorm(15)
    y <- 1 + 1.5 * x + rnorm(15, sd = 0.5)
    data.frame(school = paste0("S", g), x = x, y = y, stringsAsFactors = FALSE)
  }))
  # Introduce one extreme cluster S12
  df$y[df$school == "S12"] <- -15 * df$x[df$school == "S12"]

  res <- audit_sensitivity(
    data = df,
    fit_fn = function(d) lm(y ~ x, data = d),
    cluster_col = "school",
    winsor_probs = c(0.05),
    missingness_rates = c(0.05),
    n_boot = 30L,
    seed = 10L
  )

  expect_equal(res$status, "fail")
  loco <- res$results[res$results$scenario == "leave_one_cluster_out", ]
  expect_equal(nrow(loco), 1L)
  expect_true(grepl("S12", loco$detail))
})

test_that("audit_sensitivity evaluates observed missingness contrasts and plotting", {
  set.seed(21)
  df <- data.frame(x = rnorm(80), y = rnorm(80))
  df$y <- 1.5 * df$x + rnorm(80, sd = 0.5)
  df$x[1:5] <- NA

  res <- audit_sensitivity(
    data = df,
    fit_fn = function(d) lm(y ~ x, data = d),
    winsor_probs = c(0.02),
    missingness_rates = c(0.05),
    imputation_methods = c("complete_case", "median", "mean"),
    n_boot = 25L,
    max_loo_units = 15L,
    seed = 21L
  )

  expect_true(any(grepl("^observed_", res$results$scenario)))
  if (rlang::is_installed("ggplot2")) {
    p <- plot(res)
    expect_s3_class(p, "ggplot")
  }
})
