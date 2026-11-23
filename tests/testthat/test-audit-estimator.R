test_that("audit_estimator passes an unbiased well-calibrated OLS estimator", {
  res <- audit_estimator(
    dgp = function() {
      x <- rnorm(100)
      z <- rnorm(100)
      y <- 1.0 + 0.8 * x + 0.0 * z + rnorm(100, sd = 1)
      data.frame(x = x, z = z, y = y)
    },
    estimator = function(df) lm(y ~ x + z, data = df),
    true_params = c(x = 0.8, z = 0.0),
    n_sim = 150L,
    seed = 2026L
  )

  expect_s3_class(res, "statci_estimator_audit")
  expect_equal(res$status, "pass")
  expect_equal(res$convergence$status, "pass")
  expect_equal(nrow(res$recovery), 2L)
  expect_true(all(res$recovery$bias_status == "pass"))
  expect_true(all(res$recovery$coverage_status == "pass"))
  expect_true(all(res$recovery$se_ratio_status == "pass"))
  # z has truth = 0, so Type-I error calibration runs automatically
  expect_false(is.null(res$type1))
  expect_equal(res$type1$parameter, "z")
  expect_equal(res$type1$status, "pass")
  expect_s3_class(as_tibble(res), "tbl_df")
  expect_invisible(print(res))
})

test_that("audit_estimator catches an intentionally biased and undercovering estimator", {
  bad_res <- audit_estimator(
    dgp = function() rnorm(60, mean = 2.0, sd = 1.0),
    estimator = function(y) {
      # Biased by +0.5 and SE underestimated by factor of 3
      est <- mean(y) + 0.5
      se <- (sd(y) / sqrt(length(y))) / 3
      list(estimate = c(mu = est), se = c(mu = se))
    },
    true_params = c(mu = 2.0),
    n_sim = 120L,
    seed = 101L
  )

  expect_equal(bad_res$status, "fail")
  expect_equal(bad_res$recovery$bias_status, "fail")
  expect_equal(bad_res$recovery$coverage_status, "fail")
  expect_equal(bad_res$recovery$se_ratio_status, "fail")
  expect_lt(bad_res$recovery$coverage, 0.50)
})

test_that("audit_estimator catches Type-I error inflation under null_dgp", {
  inflated <- audit_estimator(
    dgp = function() rnorm(40, mean = 0.5, sd = 1),
    null_dgp = function() rnorm(40, mean = 0, sd = 1),
    estimator = function(y) {
      # Anti-conservative SE (half of true SE -> ~25-30% Type-I error)
      est <- mean(y)
      se <- (sd(y) / sqrt(length(y))) * 0.45
      list(estimate = c(mu = est), se = c(mu = se))
    },
    true_params = c(mu = 0.5),
    n_sim = 150L,
    seed = 77L
  )

  expect_equal(inflated$status, "fail")
  expect_false(is.null(inflated$type1))
  expect_equal(inflated$type1$status, "fail")
  expect_gt(inflated$type1$empirical_alpha, 0.15)
})

test_that("audit_estimator audits non-convergence and singular fit rates", {
  flaky <- audit_estimator(
    dgp = function() rnorm(20),
    estimator = function(y) {
      u <- runif(1)
      if (u < 0.25) stop("optimizer exploded")
      list(
        estimate = c(mu = mean(y)),
        se = c(mu = sd(y) / sqrt(length(y))),
        converged = u >= 0.35,
        singular = u < 0.50
      )
    },
    true_params = c(mu = 0),
    n_sim = 60L,
    seed = 55L
  )

  expect_equal(flaky$convergence$status, "fail")
  expect_gt(flaky$convergence$fail_rate, 0.10)
  expect_gt(flaky$convergence$singular_rate, 0.10)
  expect_equal(flaky$status, "fail")
})

test_that("audit_sbc passes a calibrated conjugate posterior and fails a miscalibrated posterior", {
  # Exact Normal-Normal conjugate posterior
  good_sbc <- audit_sbc(
    prior = function() c(mu = rnorm(1, 0, 2)),
    dgp = function(theta) rnorm(15, mean = theta[["mu"]], sd = 1),
    posterior_draws = function(y) {
      post_var <- 1 / (1 / 4 + length(y))
      post_mean <- post_var * sum(y)
      rnorm(99, mean = post_mean, sd = sqrt(post_var))
    },
    n_sim = 120L,
    n_bins = 10L,
    seed = 42L
  )
  expect_s3_class(good_sbc, "statci_sbc")
  expect_equal(good_sbc$status, "pass")
  expect_gt(good_sbc$summary$chisq_p, 0.01)
  expect_gt(good_sbc$summary$ks_p, 0.01)
  expect_s3_class(as_tibble(good_sbc), "tbl_df")
  expect_invisible(print(good_sbc))

  # Intentionally biased & underdispersed posterior -> must FAIL
  bad_sbc <- audit_sbc(
    prior = function() c(mu = rnorm(1, 0, 2)),
    dgp = function(theta) rnorm(15, mean = theta[["mu"]], sd = 1),
    posterior_draws = function(y) {
      post_var <- 1 / (1 / 4 + length(y))
      post_mean <- post_var * sum(y) + 0.6 # systematic shift + narrow spread
      rnorm(99, mean = post_mean, sd = 0.25 * sqrt(post_var))
    },
    n_sim = 120L,
    n_bins = 10L,
    seed = 42L
  )
  expect_equal(bad_sbc$status, "fail")
  expect_true(bad_sbc$summary$chisq_p < 0.01 || bad_sbc$summary$ks_p < 0.01)
})

test_that("check_parallel_rng passes L'Ecuyer per-task streams and fails non-invariant parallel code", {
  # Deterministic per-task L'Ecuyer streams via statci_parallel_apply
  pass_rng <- check_parallel_rng(
    fun = function(i) rnorm(5),
    X = seq_len(6L),
    cores = c(1L, 2L),
    seed = 123L
  )
  expect_s3_class(pass_rng, "statci_parallel_rng")
  expect_equal(pass_rng$status, "pass")
  expect_true(pass_rng$identical_all)
  expect_equal(pass_rng$max_abs_diff, 0)
  expect_invisible(print(pass_rng))

  # Broken runner whose RNG depends on core count
  broken_rng <- check_parallel_rng(
    runner = function(cores, seed) {
      if (cores == 1L) {
        withr::with_seed(seed, lapply(1:6, function(i) rnorm(3)))
      } else {
        cl <- parallel::makePSOCKcluster(cores)
        on.exit(parallel::stopCluster(cl), add = TRUE)
        parallel::clusterSetRNGStream(cl, iseed = seed)
        parallel::parLapply(cl, 1:6, function(i) rnorm(3))
      }
    },
    cores = c(1L, 2L),
    seed = 123L
  )
  expect_equal(broken_rng$status, "fail")
  expect_false(broken_rng$identical_all)
  expect_gt(broken_rng$max_abs_diff, 0)
})

test_that("audit_estimator integrates SBC, parallel RNG, glm, htest, and lmerMod singularity detection", {
  # Integrated SBC + parallel RNG
  combo <- audit_estimator(
    dgp = function() rnorm(40, mean = 1.0, sd = 1.0),
    estimator = function(y) stats::t.test(y, mu = 0),
    true_params = c(1.0),
    n_sim = 120L,
    sbc_prior = function() c(mu = rnorm(1, 0, 2)),
    sbc_dgp = function(th) rnorm(15, mean = th[["mu"]], sd = 1),
    sbc_draws = function(y) {
      pv <- 1 / (1 / 4 + length(y))
      rnorm(49, mean = pv * sum(y), sd = sqrt(pv))
    },
    sbc_n_sim = 60L,
    sbc_bins = 5L,
    check_parallel = TRUE,
    parallel_n_sim = 4L,
    seed = 2026L
  )
  expect_equal(combo$status, "pass")
  expect_false(is.null(combo$sbc))
  expect_false(is.null(combo$parallel_rng))
  expect_equal(combo$parallel_rng$status, "pass")
  if (rlang::is_installed("ggplot2")) {
    expect_s3_class(plot(combo$sbc), "ggplot")
  }

  # Logistic GLM calibration (n = 300 keeps finite-sample MLE bias negligible)
  glm_res <- audit_estimator(
    dgp = function() {
      x <- rnorm(300)
      p <- stats::plogis(-0.2 + 0.8 * x)
      data.frame(x = x, y = stats::rbinom(300, 1, p))
    },
    estimator = function(df) stats::glm(y ~ x, data = df, family = stats::binomial()),
    true_params = c(x = 0.8),
    n_sim = 100L,
    seed = 2026L
  )
  expect_equal(glm_res$status, "pass")

  # Real lme4::lmer with zero random-effect variance triggers singular fit auditing
  skip_if_not_installed("lme4")
  sing_res <- audit_estimator(
    dgp = function() {
      g <- rep(1:5, each = 6)
      x <- rnorm(30)
      data.frame(g = factor(g), x = x, y = 1 + 0.5 * x + rnorm(30, sd = 1))
    },
    estimator = function(df) lme4::lmer(y ~ x + (1 | g), data = df),
    true_params = c(x = 0.5),
    n_sim = 20L,
    max_singular_rate = 0.05,
    seed = 7L
  )
  expect_gt(sing_res$convergence$singular_rate, 0.05)
  expect_equal(sing_res$convergence$status, "fail")
})
