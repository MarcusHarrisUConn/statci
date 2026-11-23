# Power-Curve Auditing with Exact Clopper-Pearson & Monotonicity Checks

#' Audit an estimator's empirical power curve across an effect-size grid
#'
#' Simulates datasets across a grid of true effect sizes `effect_grid` and
#' audits: (1) empirical rejection probability with exact Clopper-Pearson 95%
#' confidence intervals and Monte Carlo standard errors, (2) Type-I error
#' calibration at `effect = 0` (when present in `effect_grid`), (3) statistical
#' monotonicity of empirical power as effect magnitude increases (via
#' MCSE-adjusted two-proportion z-tests so Monte Carlo noise does not trigger
#' spurious failures), and (4) achievement of `target_power` at
#' `target_effect`, along with linear-interpolated minimum detectable effect
#' (`mde`).
#'
#' @param dgp Function `function(effect)` generating a dataset for the given
#'   numeric effect size.
#' @param test_fn Function `function(data)` returning either a scalar numeric
#'   p-value, a logical scalar (`TRUE` = reject), a list containing `$p_value`
#'   or `$p.value`, or an `htest`, `lm`, `glm`, or `lmerMod` object.
#' @param effect_grid Numeric vector of at least two effect sizes to evaluate
#'   (e.g., `c(0, 0.2, 0.5, 0.8)`).
#' @param param Optional character parameter name when `test_fn` returns an
#'   `lm`, `glm`, or `lmerMod` object with multiple predictors (defaults to the
#'   first non-intercept coefficient).
#' @param n_sim Integer number of Monte Carlo replications per grid point
#'   (default `100L`).
#' @param alpha Nominal two-sided significance level (default `0.05`).
#' @param target_power Target statistical power threshold (default `0.80`).
#' @param target_effect Optional numeric effect size in `effect_grid` at which
#'   `target_power` must be achieved. If `NULL`, no target-power gate is
#'   enforced (though `mde` is still estimated).
#' @param check_monotonic Logical; if `TRUE` (default), verify empirical power
#'   does not drop significantly as `abs(effect)` increases.
#' @param check_null_type1 Logical; if `TRUE` (default) and `0` is included in
#'   `effect_grid`, audit Type-I error calibration against `alpha`.
#' @param seed Integer RNG seed for reproducibility (default `42L`).
#' @param mc_alpha Exact binomial / Monte Carlo failure significance threshold
#'   (default `0.01`).
#'
#' @return An S3 object of class `"statci_power_curve"` containing `curve` (a
#'   [tibble::tibble]), `monotonicity`, `target_check`, `null_check`, `mde`,
#'   `settings`, and overall `status` (`"pass"`, `"warn"`, or `"fail"`).
#' @export
#'
#' @examples
#' pc <- audit_power_curve(
#'   dgp = function(d) rnorm(40, mean = d, sd = 1),
#'   test_fn = function(y) t.test(y, mu = 0),
#'   effect_grid = c(0, 0.25, 0.5, 0.75),
#'   n_sim = 80L,
#'   target_effect = 0.5,
#'   target_power = 0.80,
#'   seed = 42L
#' )
#' pc
audit_power_curve <- function(dgp,
                              test_fn,
                              effect_grid,
                              param = NULL,
                              n_sim = 100L,
                              alpha = 0.05,
                              target_power = 0.80,
                              target_effect = NULL,
                              check_monotonic = TRUE,
                              check_null_type1 = TRUE,
                              seed = 42L,
                              mc_alpha = 0.01) {
  if (!is.function(dgp)) rlang::abort("`dgp` must be a function taking an `effect` scalar.")
  if (!is.function(test_fn)) rlang::abort("`test_fn` must be a function taking a simulated dataset.")
  if (!is.numeric(effect_grid) || length(effect_grid) < 2L || any(!is.finite(effect_grid))) {
    rlang::abort("`effect_grid` must be a finite numeric vector of length >= 2.")
  }
  n_sim <- as.integer(n_sim)[1L]
  if (is.na(n_sim) || n_sim < 10L) rlang::abort("`n_sim` must be an integer >= 10.")
  if (!is.numeric(alpha) || length(alpha) != 1L || alpha <= 0 || alpha >= 1) {
    rlang::abort("`alpha` must be strictly between 0 and 1.")
  }
  if (!is.numeric(target_power) || length(target_power) != 1L || target_power <= 0 || target_power >= 1) {
    rlang::abort("`target_power` must be strictly between 0 and 1.")
  }

  effect_grid <- sort(unique(as.numeric(effect_grid)))
  withr::local_seed(
    as.integer(seed)[1L],
    .rng_kind = "Mersenne-Twister",
    .rng_normal_kind = "Inversion",
    .rng_sample_kind = "Rejection"
  )

  extract_reject <- function(res) {
    if (is.logical(res) && length(res) == 1L) return(res)
    if (is.numeric(res) && length(res) == 1L) return(res <= alpha)
    if (inherits(res, "htest")) return(as.numeric(res$p.value)[1L] <= alpha)
    if (is.list(res) && !inherits(res, c("lm", "glm", "lmerMod", "glmerMod"))) {
      pv <- res$p_value %||% res$p.value
      if (!is.null(pv)) return(as.numeric(pv)[1L] <= alpha)
      if (!is.null(res$reject)) return(isTRUE(res$reject))
    }
    norm <- normalize_estimator_result(res, alpha = alpha)
    pvs <- norm$p_value
    if (!is.null(param)) {
      if (!(param %in% names(pvs))) rlang::abort(paste0("Parameter `", param, "` not found in test output."))
      return(as.numeric(pvs[[param]]) <= alpha)
    }
    non_int <- setdiff(names(pvs), "(Intercept)")
    pick <- if (length(non_int) > 0L) non_int[1L] else names(pvs)[1L]
    as.numeric(pvs[[pick]]) <= alpha
  }

  rows <- vector("list", length(effect_grid))
  for (g in seq_along(effect_grid)) {
    eff <- effect_grid[g]
    rejs <- logical(n_sim)
    ok <- logical(n_sim)
    for (m in seq_len(n_sim)) {
      val <- tryCatch(
        suppressWarnings(extract_reject(test_fn(dgp(eff)))),
        error = function(e) NA
      )
      if (is.logical(val) && length(val) == 1L && !is.na(val)) {
        rejs[m] <- val
        ok[m] <- TRUE
      }
    }
    n_valid <- sum(ok)
    if (n_valid < 2L) {
      rows[[g]] <- tibble::tibble(
        effect = eff, n_valid = as.integer(n_valid), n_reject = 0L,
        power = NA_real_, mcse = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
        fail_rate = 1 - n_valid / n_sim
      )
    } else {
      k <- sum(rejs[ok])
      pw <- k / n_valid
      bt <- stats::binom.test(k, n_valid, conf.level = 0.95)
      rows[[g]] <- tibble::tibble(
        effect = eff,
        n_valid = as.integer(n_valid),
        n_reject = as.integer(k),
        power = pw,
        mcse = sqrt(pw * (1 - pw) / n_valid),
        ci_low = as.numeric(bt$conf.int[1L]),
        ci_high = as.numeric(bt$conf.int[2L]),
        fail_rate = 1 - n_valid / n_sim
      )
    }
  }
  curve <- tibble::as_tibble(do.call(rbind, rows))

  statuses <- character(0)
  if (any(curve$fail_rate > 0.05) || any(is.na(curve$power))) {
    statuses <- c(statuses, "fail")
  }

  # 1. Null Type-I error check (at effect == 0)
  null_check <- list(evaluated = FALSE, status = "pass", empirical_alpha = NA_real_, binom_p = NA_real_)
  zero_idx <- which(abs(curve$effect) < 1e-12)
  if (isTRUE(check_null_type1) && length(zero_idx) == 1L && curve$n_valid[zero_idx] >= 2L) {
    k0 <- curve$n_reject[zero_idx]
    m0 <- curve$n_valid[zero_idx]
    bt0 <- stats::binom.test(k0, m0, p = alpha)
    emp_a <- k0 / m0
    st0 <- if (bt0$p.value < mc_alpha && (emp_a > alpha || emp_a < alpha / 4)) {
      "fail"
    } else if (bt0$p.value < 0.05) {
      "warn"
    } else {
      "pass"
    }
    null_check <- list(evaluated = TRUE, status = st0, empirical_alpha = emp_a, binom_p = as.numeric(bt0$p.value))
    statuses <- c(statuses, st0)
  }

  # 2. Monotonicity check across non-decreasing |effect| (MCSE-adjusted)
  mono_status <- "pass"
  max_drop_z <- 0
  violations <- integer(0)
  if (isTRUE(check_monotonic)) {
    ord_abs <- order(abs(curve$effect))
    c_ord <- curve[ord_abs, , drop = FALSE]
    for (idx in seq_len(nrow(c_ord) - 1L)) {
      if (abs(c_ord$effect[idx + 1L]) <= abs(c_ord$effect[idx]) + 1e-12) next
      p1 <- c_ord$power[idx]
      p2 <- c_ord$power[idx + 1L]
      if (is.na(p1) || is.na(p2)) next
      if (p1 > p2) {
        n1 <- c_ord$n_valid[idx]
        n2 <- c_ord$n_valid[idx + 1L]
        p_pool <- (c_ord$n_reject[idx] + c_ord$n_reject[idx + 1L]) / (n1 + n2)
        se_diff <- sqrt(p_pool * (1 - p_pool) * (1 / n1 + 1 / n2))
        z_drop <- if (se_diff > 0) (p1 - p2) / se_diff else Inf
        if (z_drop > max_drop_z) max_drop_z <- z_drop
        if (z_drop > 2.5) {
          violations <- c(violations, idx)
        }
      }
    }
    mono_status <- if (length(violations) > 0L) "fail" else if (max_drop_z > 1.96) "warn" else "pass"
    statuses <- c(statuses, mono_status)
  }
  monotonicity <- list(
    checked = isTRUE(check_monotonic),
    n_violations = length(violations),
    max_drop_z = max_drop_z,
    status = mono_status
  )

  # 3. Target power check at target_effect
  target_check <- list(
    evaluated = FALSE, target_effect = target_effect, target_power = target_power,
    achieved_power = NA_real_, binom_p = NA_real_, status = "pass"
  )
  if (!is.null(target_effect)) {
    t_idx <- which.min(abs(curve$effect - target_effect))
    if (curve$n_valid[t_idx] >= 2L) {
      kt <- curve$n_reject[t_idx]
      mt <- curve$n_valid[t_idx]
      bt_t <- stats::binom.test(kt, mt, p = target_power, alternative = "less")
      p_ach <- kt / mt
      st_t <- if (p_ach < target_power && bt_t$p.value < mc_alpha) {
        "fail"
      } else if (p_ach < target_power && bt_t$p.value < 0.05) {
        "warn"
      } else {
        "pass"
      }
      target_check <- list(
        evaluated = TRUE,
        target_effect = curve$effect[t_idx],
        target_power = target_power,
        achieved_power = p_ach,
        binom_p = as.numeric(bt_t$p.value),
        status = st_t
      )
      statuses <- c(statuses, st_t)
    }
  }

  # 4. Estimate Minimum Detectable Effect (MDE) via linear interpolation on positive branch
  mde <- NA_real_
  pos_c <- curve[curve$effect >= 0 & is.finite(curve$power), , drop = FALSE]
  if (nrow(pos_c) >= 2L) {
    above <- which(pos_c$power >= target_power)
    if (length(above) >= 1L) {
      i_hi <- above[1L]
      if (i_hi == 1L) {
        mde <- pos_c$effect[1L]
      } else {
        p_lo <- pos_c$power[i_hi - 1L]
        p_hi <- pos_c$power[i_hi]
        e_lo <- pos_c$effect[i_hi - 1L]
        e_hi <- pos_c$effect[i_hi]
        mde <- if (p_hi > p_lo) e_lo + (target_power - p_lo) / (p_hi - p_lo) * (e_hi - e_lo) else e_hi
      }
    }
  }

  overall <- combine_status(statuses)

  structure(
    list(
      curve = curve,
      monotonicity = monotonicity,
      null_check = null_check,
      target_check = target_check,
      mde = mde,
      settings = list(
        n_sim = n_sim, alpha = alpha, target_power = target_power,
        target_effect = target_effect, seed = as.integer(seed)[1L], mc_alpha = mc_alpha
      ),
      status = overall
    ),
    class = "statci_power_curve"
  )
}

#' @export
as_tibble.statci_power_curve <- function(x, ...) {
  x$curve
}

#' @export
print.statci_power_curve <- function(x, ...) {
  badge <- format_status_badge(x$status)
  cli::cli_text(
    "{badge} {.strong statci Power-Curve Audit} ",
    "({nrow(x$curve)} grid points, n_sim = {x$settings$n_sim}, MDE({fmt_pct(x$settings$target_power, 0L)}) = {fmt_num(x$mde, 3L)})"
  )
  for (i in seq_len(nrow(x$curve))) {
    r <- x$curve[i, ]
    cli::cli_text(
      "  effect = {fmt_num(r$effect, 3L)}: power = {fmt_pct(r$power)} ",
      "[{fmt_pct(r$ci_low)}, {fmt_pct(r$ci_high)}] (MCSE = {fmt_num(r$mcse, 3L)})"
    )
  }
  if (isTRUE(x$null_check$evaluated)) {
    b0 <- format_status_badge(x$null_check$status)
    cli::cli_text("  {b0} Null Type-I error: {fmt_pct(x$null_check$empirical_alpha)} (binom p = {fmt_num(x$null_check$binom_p, 4L)})")
  }
  bm <- format_status_badge(x$monotonicity$status)
  cli::cli_text("  {bm} Monotonicity: {x$monotonicity$n_violations} significant drop(s) (max drop z = {fmt_num(x$monotonicity$max_drop_z, 2L)})")
  if (isTRUE(x$target_check$evaluated)) {
    bt <- format_status_badge(x$target_check$status)
    cli::cli_text(
      "  {bt} Target power ({fmt_pct(x$target_check$target_power, 0L)} at effect = {fmt_num(x$target_check$target_effect, 3L)}): ",
      "achieved {fmt_pct(x$target_check$achieved_power)}"
    )
  }
  invisible(x)
}

#' Plot empirical power curve with exact Clopper-Pearson confidence band
#'
#' @param x A `"statci_power_curve"` object from [audit_power_curve()].
#' @param ... Unused.
#'
#' @return A `ggplot` object if `ggplot2` is installed, otherwise draws a base
#'   R plot and returns `x` invisibly.
#' @export
#'
#' @examples
#' pc <- audit_power_curve(
#'   dgp = function(d) rnorm(30, mean = d, sd = 1),
#'   test_fn = function(y) t.test(y, mu = 0),
#'   effect_grid = c(0, 0.3, 0.6),
#'   n_sim = 30L,
#'   seed = 42L
#' )
#' plot(pc)
plot.statci_power_curve <- function(x, ...) {
  df <- x$curve
  tp <- x$settings$target_power
  al <- x$settings$alpha

  if (rlang::is_installed("ggplot2")) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["effect"]], y = .data[["power"]])) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data[["ci_low"]], ymax = .data[["ci_high"]]),
        fill = "#264653", alpha = 0.18
      ) +
      ggplot2::geom_hline(yintercept = tp, linetype = "dashed", color = "#15803D") +
      ggplot2::geom_hline(yintercept = al, linetype = "dotted", color = "#B91C1C") +
      ggplot2::geom_line(color = "#264653", linewidth = 0.9) +
      ggplot2::geom_point(color = "#264653", size = 2.6) +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::labs(
        title = "Empirical Power Curve Audit",
        subtitle = paste0("95% Clopper-Pearson band (n_sim = ", x$settings$n_sim, "); dashed = target power, dotted = alpha"),
        x = "Effect Size",
        y = "Rejection Probability"
      ) +
      ggplot2::theme_minimal()
    return(p)
  }

  graphics::plot(
    df$effect, df$power, type = "b", ylim = c(0, 1), pch = 19, col = "#264653",
    xlab = "Effect Size", ylab = "Rejection Probability",
    main = "Empirical Power Curve Audit"
  )
  graphics::abline(h = c(al, tp), lty = c(3, 2), col = c("#B91C1C", "#15803D"))
  invisible(x)
}
