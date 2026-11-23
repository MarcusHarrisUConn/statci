# Estimator Calibration & Fuzzing Harness

#' Audit a statistical estimator via Monte Carlo calibration and fuzzing
#'
#' Evaluates parameter recovery (bias, RMSE, empirical vs. model SE ratio, and
#' confidence-interval coverage with exact Clopper-Pearson bounds accounting
#' for Monte Carlo error), Type-I error calibration under a null DGP,
#' non-convergence and singular-fit rates, and optionally Simulation-Based
#' Calibration ([audit_sbc()]) and parallel RNG invariance
#' ([check_parallel_rng()]).
#'
#' @param dgp Data-generating function. Either `function()` (0 arguments) or
#'   `function(true_params)` receiving `true_params`.
#' @param estimator Fitting function `function(data)` returning either:
#'   - A named list with `estimate` (required) and optional `se`, `ci_lower`,
#'     `ci_upper`, `p_value`, `converged`, `singular`;
#'   - A fitted `lm`, `glm`, `lmerMod`, `glmerMod`, or `htest` object; or
#'   - A numeric vector of point estimates.
#' @param true_params Named numeric vector (or scalar) of ground-truth
#'   parameter values matching names returned by `estimator`. If unnamed, names
#'   are matched positionally to the estimator output.
#' @param n_sim Integer number of Monte Carlo replications (default `200L`).
#' @param alpha Nominal significance level for confidence intervals
#'   (`1 - alpha`) and Type-I error checks (default `0.05`).
#' @param null_dgp Optional null data-generating function for dedicated Type-I
#'   error calibration. If `NULL`, any parameters in `true_params` equal to `0`
#'   are automatically audited for Type-I error.
#' @param null_params Optional character vector of parameter names to evaluate
#'   under `null_dgp` (defaults to all parameters returned by `estimator`).
#' @param n_sim_null Integer number of replications for `null_dgp` (defaults to
#'   `n_sim`).
#' @param sbc_prior Optional prior sampler for Simulation-Based Calibration
#'   (forwarded to [audit_sbc()]).
#' @param sbc_dgp Optional likelihood simulator `function(theta)` for SBC
#'   (defaults to `dgp`).
#' @param sbc_draws Optional posterior draw function `function(data)` for SBC.
#' @param sbc_n_sim Integer SBC replications (default `100L`).
#' @param sbc_bins Integer SBC histogram bins (default `10L`).
#' @param check_parallel Logical; if `TRUE`, also runs [check_parallel_rng()]
#'   to verify parallel RNG invariance. Default `FALSE`.
#' @param parallel_runner Optional custom `function(cores, seed)` for
#'   [check_parallel_rng()]. If `NULL` and `check_parallel = TRUE`, automatically
#'   tests `dgp` + `estimator` through [statci_parallel_apply()].
#' @param parallel_cores Integer vector of core counts to compare (default
#'   `c(1L, 2L)`).
#' @param parallel_n_sim Number of tasks used in parallel invariance check
#'   (default `8L`).
#' @param seed Integer RNG seed (default `42L`).
#' @param mc_alpha Exact binomial / Monte Carlo failure significance threshold
#'   (default `0.01`).
#' @param bias_tol_mc Monte Carlo z-score threshold (`|bias| / MCSE_bias`) for
#'   failing bias recovery (default `3.0`).
#' @param se_ratio_bounds Numeric length-2 target range for
#'   `mean(model_SE) / empirical_SE` (default `c(0.85, 1.15)`).
#' @param max_fail_rate Maximum tolerable error/non-convergence rate before
#'   `"fail"` (default `0.05`).
#' @param warn_fail_rate Error/non-convergence rate threshold for `"warn"`
#'   (default `0.01`).
#' @param max_singular_rate Maximum tolerable singular-fit rate before `"fail"`
#'   (default `0.10`).
#'
#' @return An S3 object of class `"statci_estimator_audit"` containing
#'   `recovery`, `type1`, `convergence`, `sbc`, `parallel_rng`, `settings`, and
#'   overall `status` (`"pass"`, `"warn"`, or `"fail"`).
#' @export
#'
#' @examples
#' res <- audit_estimator(
#'   dgp = function() {
#'     x <- rnorm(80)
#'     data.frame(x = x, y = 0.5 * x + rnorm(80))
#'   },
#'   estimator = function(df) lm(y ~ x, data = df),
#'   true_params = c(x = 0.5),
#'   n_sim = 60L,
#'   seed = 42L
#' )
#' res
audit_estimator <- function(dgp,
                            estimator,
                            true_params = NULL,
                            n_sim = 200L,
                            alpha = 0.05,
                            null_dgp = NULL,
                            null_params = NULL,
                            n_sim_null = n_sim,
                            sbc_prior = NULL,
                            sbc_dgp = NULL,
                            sbc_draws = NULL,
                            sbc_n_sim = 100L,
                            sbc_bins = 10L,
                            check_parallel = FALSE,
                            parallel_runner = NULL,
                            parallel_cores = c(1L, 2L),
                            parallel_n_sim = 8L,
                            seed = 42L,
                            mc_alpha = 0.01,
                            bias_tol_mc = 3.0,
                            se_ratio_bounds = c(0.85, 1.15),
                            max_fail_rate = 0.05,
                            warn_fail_rate = 0.01,
                            max_singular_rate = 0.10) {
  if (!is.function(dgp)) rlang::abort("`dgp` must be a function.")
  if (!is.function(estimator)) rlang::abort("`estimator` must be a function.")
  n_sim <- as.integer(n_sim)[1L]
  if (is.na(n_sim) || n_sim < 10L) {
    rlang::abort("`n_sim` must be an integer >= 10.")
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || alpha <= 0 || alpha >= 1) {
    rlang::abort("`alpha` must be strictly between 0 and 1.")
  }

  withr::local_seed(
    as.integer(seed)[1L],
    .rng_kind = "Mersenne-Twister",
    .rng_normal_kind = "Inversion",
    .rng_sample_kind = "Rejection"
  )

  # Run main simulation replications
  sim <- run_estimator_replications(
    dgp = dgp,
    estimator = estimator,
    params_arg = true_params,
    n_sim = n_sim,
    alpha = alpha
  )

  # Evaluate convergence & singularity
  fail_rate <- (sim$n_error + sim$n_nonconverged) / n_sim
  singular_rate <- sim$n_singular / n_sim
  conv_status <- if (fail_rate > max_fail_rate || singular_rate > max_singular_rate) {
    "fail"
  } else if (fail_rate > warn_fail_rate || singular_rate > max_singular_rate / 2) {
    "warn"
  } else {
    "pass"
  }
  convergence <- tibble::tibble(
    n_total = n_sim,
    n_valid = sim$n_valid,
    n_error = sim$n_error,
    n_nonconverged = sim$n_nonconverged,
    n_singular = sim$n_singular,
    fail_rate = fail_rate,
    singular_rate = singular_rate,
    status = conv_status
  )

  # Evaluate parameter recovery
  recovery <- evaluate_recovery(
    sim = sim,
    true_params = true_params,
    alpha = alpha,
    mc_alpha = mc_alpha,
    bias_tol_mc = bias_tol_mc,
    se_ratio_bounds = se_ratio_bounds
  )

  # Evaluate Type-I error calibration
  type1 <- NULL
  if (!is.null(null_dgp)) {
    null_sim <- run_estimator_replications(
      dgp = null_dgp,
      estimator = estimator,
      params_arg = NULL,
      n_sim = as.integer(n_sim_null)[1L],
      alpha = alpha
    )
    type1 <- evaluate_type1(
      sim = null_sim,
      target_params = null_params,
      alpha = alpha,
      mc_alpha = mc_alpha
    )
  } else if (!is.null(true_params) && sim$n_valid > 0L) {
    tp_matched <- match_true_params(true_params, colnames(sim$estimates))
    if (length(tp_matched) > 0L) {
      zero_params <- names(tp_matched)[abs(tp_matched) < 1e-12]
      if (length(zero_params) > 0L) {
        type1 <- evaluate_type1(
          sim = sim,
          target_params = zero_params,
          alpha = alpha,
          mc_alpha = mc_alpha
        )
      }
    }
  }

  # Optional SBC check
  sbc_res <- NULL
  if (!is.null(sbc_prior) && !is.null(sbc_draws)) {
    sbc_res <- audit_sbc(
      prior = sbc_prior,
      dgp = sbc_dgp %||% dgp,
      posterior_draws = sbc_draws,
      n_sim = sbc_n_sim,
      n_bins = sbc_bins,
      seed = as.integer(seed)[1L] + 1000L,
      mc_alpha = mc_alpha
    )
  }

  # Optional parallel RNG invariance check
  par_res <- NULL
  if (isTRUE(check_parallel) || !is.null(parallel_runner)) {
    if (!is.null(parallel_runner)) {
      par_res <- check_parallel_rng(
        runner = parallel_runner,
        cores = parallel_cores,
        seed = as.integer(seed)[1L]
      )
    } else {
      dgp_takes_arg <- length(formals(dgp)) > 0L && !is.null(true_params)
      par_res <- check_parallel_rng(
        runner = function(n_cores, s) {
          raw_list <- statci_parallel_apply(
            X = seq_len(as.integer(parallel_n_sim)[1L]),
            FUN = function(i, dgp_fn, est_fn, tp, takes_arg) {
              dat <- if (takes_arg) dgp_fn(tp) else dgp_fn()
              est_fn(dat)
            },
            dgp_fn = dgp,
            est_fn = estimator,
            tp = true_params,
            takes_arg = dgp_takes_arg,
            cores = n_cores,
            seed = s
          )
          lapply(raw_list, function(r) normalize_estimator_result(r, alpha = alpha)$estimate)
        },
        cores = parallel_cores,
        seed = as.integer(seed)[1L]
      )
    }
  }

  all_statuses <- c(
    convergence$status,
    if (!is.null(recovery) && nrow(recovery) > 0L) recovery$status,
    if (!is.null(type1) && nrow(type1) > 0L) type1$status,
    if (!is.null(sbc_res)) sbc_res$status,
    if (!is.null(par_res)) par_res$status
  )
  overall <- combine_status(all_statuses)

  structure(
    list(
      recovery = recovery,
      type1 = type1,
      convergence = convergence,
      sbc = sbc_res,
      parallel_rng = par_res,
      settings = list(
        n_sim = n_sim,
        alpha = alpha,
        seed = as.integer(seed)[1L],
        mc_alpha = mc_alpha,
        bias_tol_mc = bias_tol_mc,
        se_ratio_bounds = se_ratio_bounds
      ),
      status = overall
    ),
    class = "statci_estimator_audit"
  )
}

run_estimator_replications <- function(dgp, estimator, params_arg, n_sim, alpha) {
  dgp_takes_arg <- length(formals(dgp)) > 0L && !is.null(params_arg)

  est_list <- vector("list", n_sim)
  se_list <- vector("list", n_sim)
  cil_list <- vector("list", n_sim)
  ciu_list <- vector("list", n_sim)
  pval_list <- vector("list", n_sim)

  n_error <- 0L
  n_nonconverged <- 0L
  n_singular <- 0L
  pnames <- NULL

  for (m in seq_len(n_sim)) {
    warn_msgs <- character(0)
    rec <- tryCatch(
      withCallingHandlers(
        {
          dat <- if (dgp_takes_arg) dgp(params_arg) else dgp()
          raw <- estimator(dat)
          normalize_estimator_result(raw, alpha = alpha)
        },
        warning = function(w) {
          warn_msgs <<- c(warn_msgs, conditionMessage(w))
          invokeRestart("muffleWarning")
        },
        message = function(msg) {
          txt <- conditionMessage(msg)
          if (grepl("singular|converg", txt, ignore.case = TRUE)) {
            warn_msgs <<- c(warn_msgs, txt)
            invokeRestart("muffleMessage")
          }
        }
      ),
      error = function(e) NULL
    )

    if (is.null(rec)) {
      n_error <- n_error + 1L
      next
    }

    if (any(grepl("singular", warn_msgs, ignore.case = TRUE))) {
      rec$singular <- TRUE
    }
    if (any(grepl("failed to converge|did not converge|non-convergence", warn_msgs, ignore.case = TRUE))) {
      rec$converged <- FALSE
    }

    if (isTRUE(rec$singular)) {
      n_singular <- n_singular + 1L
    }
    if (length(rec$estimate) == 0L || all(!is.finite(rec$estimate))) {
      rec$converged <- FALSE
    }
    if (!isTRUE(rec$converged)) {
      n_nonconverged <- n_nonconverged + 1L
      next
    }

    if (is.null(pnames)) {
      pnames <- names(rec$estimate)
    }
    est_list[[m]] <- rec$estimate[pnames]
    se_list[[m]] <- rec$se[pnames]
    cil_list[[m]] <- rec$ci_lower[pnames]
    ciu_list[[m]] <- rec$ci_upper[pnames]
    pval_list[[m]] <- rec$p_value[pnames]
  }

  valid <- which(!vapply(est_list, is.null, logical(1L)))
  if (length(valid) == 0L) {
    empty_m <- matrix(numeric(0), nrow = 0L, ncol = 0L)
    return(list(
      estimates = empty_m, se = empty_m, ci_lower = empty_m, ci_upper = empty_m,
      p_value = empty_m, n_valid = 0L, n_error = n_error,
      n_nonconverged = n_nonconverged, n_singular = n_singular
    ))
  }

  bind_mat <- function(lst) {
    mat <- do.call(rbind, lst[valid])
    colnames(mat) <- pnames
    mat
  }

  list(
    estimates = bind_mat(est_list),
    se = bind_mat(se_list),
    ci_lower = bind_mat(cil_list),
    ci_upper = bind_mat(ciu_list),
    p_value = bind_mat(pval_list),
    n_valid = length(valid),
    n_error = n_error,
    n_nonconverged = n_nonconverged,
    n_singular = n_singular
  )
}

match_true_params <- function(true_params, col_names) {
  if (is.null(true_params) || length(col_names) == 0L) return(NULL)
  if (!is.null(names(true_params)) && any(nzchar(names(true_params)))) {
    common <- intersect(names(true_params), col_names)
    if (length(common) == 0L) {
      rlang::abort(paste0(
        "None of `true_params` names (", paste(names(true_params), collapse = ", "),
        ") match estimator outputs (", paste(col_names, collapse = ", "), ")."
      ))
    }
    return(true_params[common])
  }
  if (length(true_params) > length(col_names)) {
    rlang::abort("`true_params` has more elements than parameters returned by `estimator`.")
  }
  stats::setNames(as.numeric(true_params), col_names[seq_along(true_params)])
}

evaluate_recovery <- function(sim, true_params, alpha, mc_alpha, bias_tol_mc, se_ratio_bounds) {
  if (sim$n_valid == 0L || is.null(true_params)) {
    return(tibble::tibble(
      parameter = character(0), truth = numeric(0), n_valid = integer(0),
      mean_est = numeric(0), bias = numeric(0), bias_mcse = numeric(0),
      bias_z = numeric(0), rel_bias = numeric(0), rmse = numeric(0),
      emp_se = numeric(0), mean_model_se = numeric(0), se_ratio = numeric(0),
      se_ratio_mcse = numeric(0), coverage = numeric(0), coverage_mcse = numeric(0),
      coverage_ci_low = numeric(0), coverage_ci_high = numeric(0),
      coverage_p = numeric(0), bias_status = character(0),
      coverage_status = character(0), se_ratio_status = character(0),
      status = character(0)
    ))
  }

  tp <- match_true_params(true_params, colnames(sim$estimates))
  nominal_cov <- 1 - alpha
  rows <- vector("list", length(tp))

  for (j in seq_along(tp)) {
    pnm <- names(tp)[j]
    truth <- as.numeric(tp[j])
    est <- sim$estimates[, pnm]
    est_ok <- est[is.finite(est)]
    M <- length(est_ok)

    if (M < 2L) {
      rows[[j]] <- tibble::tibble(
        parameter = pnm, truth = truth, n_valid = M,
        mean_est = NA_real_, bias = NA_real_, bias_mcse = NA_real_,
        bias_z = NA_real_, rel_bias = NA_real_, rmse = NA_real_,
        emp_se = NA_real_, mean_model_se = NA_real_, se_ratio = NA_real_,
        se_ratio_mcse = NA_real_, coverage = NA_real_, coverage_mcse = NA_real_,
        coverage_ci_low = NA_real_, coverage_ci_high = NA_real_,
        coverage_p = NA_real_, bias_status = "fail",
        coverage_status = "fail", se_ratio_status = "fail", status = "fail"
      )
      next
    }

    mean_est <- mean(est_ok)
    bias <- mean_est - truth
    emp_se <- stats::sd(est_ok)
    bias_mcse <- emp_se / sqrt(M)
    bias_z <- if (bias_mcse > 0) abs(bias) / bias_mcse else if (abs(bias) > 1e-8) Inf else 0
    rel_bias <- if (abs(truth) > 1e-12) bias / truth else NA_real_
    rmse <- sqrt(mean((est_ok - truth)^2))

    bias_status <- if (bias_z > bias_tol_mc && abs(bias) > 1e-8) {
      "fail"
    } else if (bias_z > 2.0 && abs(bias) > 1e-8) {
      "warn"
    } else {
      "pass"
    }

    # Model SE vs Empirical SE ratio
    se_vals <- sim$se[, pnm]
    se_vals <- se_vals[is.finite(se_vals) & se_vals > 0]
    if (length(se_vals) >= 2L && emp_se > 0) {
      mean_model_se <- mean(se_vals)
      se_ratio <- mean_model_se / emp_se
      se_ratio_mcse <- se_ratio / sqrt(2 * (M - 1))
      ci_r_lo <- se_ratio - 1.96 * se_ratio_mcse
      ci_r_hi <- se_ratio + 1.96 * se_ratio_mcse
      z_r_unit <- if (se_ratio_mcse > 0) abs(se_ratio - 1) / se_ratio_mcse else 0
      if (ci_r_hi < se_ratio_bounds[1L] || ci_r_lo > se_ratio_bounds[2L]) {
        se_ratio_status <- "fail"
      } else if ((se_ratio < se_ratio_bounds[1L] || se_ratio > se_ratio_bounds[2L]) && z_r_unit > 2.0) {
        se_ratio_status <- "warn"
      } else {
        se_ratio_status <- "pass"
      }
    } else {
      mean_model_se <- NA_real_
      se_ratio <- NA_real_
      se_ratio_mcse <- NA_real_
      se_ratio_status <- "pass"
    }

    # Exact Clopper-Pearson coverage test
    cil <- sim$ci_lower[, pnm]
    ciu <- sim$ci_upper[, pnm]
    ci_ok <- is.finite(cil) & is.finite(ciu)
    if (sum(ci_ok) >= 2L) {
      m_ci <- sum(ci_ok)
      k_cov <- sum(cil[ci_ok] <= truth & ciu[ci_ok] >= truth)
      cov_hat <- k_cov / m_ci
      cov_mcse <- sqrt(cov_hat * (1 - cov_hat) / m_ci)
      bt <- stats::binom.test(k_cov, m_ci, p = nominal_cov, conf.level = 1 - alpha)
      cov_p <- as.numeric(bt$p.value)
      cov_ci_lo <- as.numeric(bt$conf.int[1L])
      cov_ci_hi <- as.numeric(bt$conf.int[2L])
      cov_status <- if (cov_p < mc_alpha && abs(cov_hat - nominal_cov) > 0.015) {
        "fail"
      } else if (cov_p < 0.05) {
        "warn"
      } else {
        "pass"
      }
    } else {
      cov_hat <- NA_real_
      cov_mcse <- NA_real_
      cov_ci_lo <- NA_real_
      cov_ci_hi <- NA_real_
      cov_p <- NA_real_
      cov_status <- "pass"
    }

    row_status <- combine_status(c(bias_status, cov_status, se_ratio_status))

    rows[[j]] <- tibble::tibble(
      parameter = pnm,
      truth = truth,
      n_valid = as.integer(M),
      mean_est = mean_est,
      bias = bias,
      bias_mcse = bias_mcse,
      bias_z = bias_z,
      rel_bias = rel_bias,
      rmse = rmse,
      emp_se = emp_se,
      mean_model_se = mean_model_se,
      se_ratio = se_ratio,
      se_ratio_mcse = se_ratio_mcse,
      coverage = cov_hat,
      coverage_mcse = cov_mcse,
      coverage_ci_low = cov_ci_lo,
      coverage_ci_high = cov_ci_hi,
      coverage_p = cov_p,
      bias_status = bias_status,
      coverage_status = cov_status,
      se_ratio_status = se_ratio_status,
      status = row_status
    )
  }

  tibble::as_tibble(do.call(rbind, rows))
}

evaluate_type1 <- function(sim, target_params, alpha, mc_alpha) {
  if (sim$n_valid == 0L || ncol(sim$p_value) == 0L) {
    return(tibble::tibble(
      parameter = character(0), n_valid = integer(0), nominal_alpha = numeric(0),
      empirical_alpha = numeric(0), mcse = numeric(0), ci_low = numeric(0),
      ci_high = numeric(0), binom_p = numeric(0), status = character(0)
    ))
  }
  pnames <- if (is.null(target_params)) colnames(sim$p_value) else intersect(target_params, colnames(sim$p_value))
  rows <- vector("list", length(pnames))

  for (j in seq_along(pnames)) {
    pnm <- pnames[j]
    pv <- sim$p_value[, pnm]
    pv <- pv[is.finite(pv)]
    m <- length(pv)
    if (m < 2L) {
      # If estimates converged but no finite p-values were provided (e.g. degenerate zero-SE estimator), warn cleanly
      st_m <- if (sim$n_valid == 0L) "fail" else "warn"
      rows[[j]] <- tibble::tibble(
        parameter = pnm, n_valid = as.integer(m), nominal_alpha = alpha,
        empirical_alpha = NA_real_, mcse = NA_real_, ci_low = NA_real_,
        ci_high = NA_real_, binom_p = NA_real_, status = st_m
      )
      next
    }
    k <- sum(pv <= alpha)
    emp_a <- k / m
    mcse <- sqrt(emp_a * (1 - emp_a) / m)
    bt <- stats::binom.test(k, m, p = alpha)
    bp <- as.numeric(bt$p.value)
    st <- if (bp < mc_alpha && (emp_a > alpha || emp_a < alpha / 4)) {
      "fail"
    } else if (bp < 0.05) {
      "warn"
    } else {
      "pass"
    }
    rows[[j]] <- tibble::tibble(
      parameter = pnm,
      n_valid = as.integer(m),
      nominal_alpha = alpha,
      empirical_alpha = emp_a,
      mcse = mcse,
      ci_low = as.numeric(bt$conf.int[1L]),
      ci_high = as.numeric(bt$conf.int[2L]),
      binom_p = bp,
      status = st
    )
  }

  tibble::as_tibble(do.call(rbind, rows))
}

#' @export
as_tibble.statci_estimator_audit <- function(x, ...) {
  x$recovery
}

#' @export
print.statci_estimator_audit <- function(x, ...) {
  badge <- format_status_badge(x$status)
  cv <- x$convergence
  cli::cli_text(
    "{badge} {.strong statci Estimator Audit} ",
    "(n_sim = {cv$n_total}, valid = {cv$n_valid}, fail_rate = {fmt_pct(cv$fail_rate)}, ",
    "singular_rate = {fmt_pct(cv$singular_rate)})"
  )
  if (!is.null(x$recovery) && nrow(x$recovery) > 0L) {
    cli::cli_text("  {.emph Parameter Recovery}:")
    for (i in seq_len(nrow(x$recovery))) {
      r <- x$recovery[i, ]
      b <- format_status_badge(r$status)
      cli::cli_text(
        "    {b} {r$parameter}: bias = {fmt_num(r$bias, 4L)} (z = {fmt_num(r$bias_z, 2L)}), ",
        "RMSE = {fmt_num(r$rmse, 4L)}, SE ratio = {fmt_num(r$se_ratio, 3L)}, ",
        "coverage = {fmt_pct(r$coverage)}"
      )
    }
  }
  if (!is.null(x$type1) && nrow(x$type1) > 0L) {
    cli::cli_text("  {.emph Type-I Error Calibration} (nominal = {fmt_pct(x$settings$alpha)}):")
    for (i in seq_len(nrow(x$type1))) {
      r <- x$type1[i, ]
      b <- format_status_badge(r$status)
      cli::cli_text(
        "    {b} {r$parameter}: empirical = {fmt_pct(r$empirical_alpha)} ",
        "[{fmt_pct(r$ci_low)}, {fmt_pct(r$ci_high)}], binom p = {fmt_num(r$binom_p, 4L)}"
      )
    }
  }
  if (!is.null(x$sbc)) {
    print(x$sbc)
  }
  if (!is.null(x$parallel_rng)) {
    print(x$parallel_rng)
  }
  invisible(x)
}
