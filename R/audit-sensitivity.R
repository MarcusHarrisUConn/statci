# Adversarial Sensitivity & Robustness Battery

#' Run an adversarial sensitivity and robustness battery on a statistical pipeline
#'
#' Automatically stress-tests a fitted modeling pipeline across four structured
#' perturbation families:
#' 1. **Outlier winsorization** of continuous predictors/outcomes at tail
#'    quantiles (e.g., 1% and 5%).
#' 2. **Leave-one-cluster-out (LOCO) / leave-one-out (LOO)** jackknife
#'    influence analysis.
#' 3. **Missingness stress tests** (contrasting complete-case analysis against
#'    median/mean imputation under observed and injected MCAR missingness).
#' 4. **Nonparametric bootstrap stability** (cluster bootstrap when
#'    `cluster_col` is given, otherwise row bootstrap).
#'
#' @param data A `data.frame` containing the analysis dataset.
#' @param fit_fn Function `function(df)` fitting the model on a perturbed data
#'   frame and returning a fitted model or numeric/list result.
#' @param extract_fn Optional function `function(fit)` returning a named numeric
#'   vector of target parameter estimates. If `NULL`, `statci` automatically
#'   extracts non-intercept coefficients from `lm`, `glm`, `lmerMod`, `htest`,
#'   or lists with `$estimate`.
#' @param cluster_col Optional character scalar naming the clustering column
#'   (e.g., `"school"`, `"subject_id"`). Enables leave-one-cluster-out (LOCO)
#'   influence checks and cluster resampling in the bootstrap.
#' @param numeric_cols Optional character vector of numeric columns to winsorize
#'   and perturb. Defaults to all non-binary numeric columns excluding
#'   `cluster_col`.
#' @param winsor_probs Numeric vector of tail probabilities for winsorization
#'   (default `c(0.01, 0.05)`). Set to `NULL` or `numeric(0)` to skip.
#' @param leave_one_out Logical; run LOO/LOCO influence check? Default `TRUE`.
#' @param max_loo_units Integer cap on units dropped during LOO/LOCO (default
#'   `50L`) to keep CI runtimes bounded on large datasets.
#' @param missingness_rates Numeric vector of injected MCAR missingness rates
#'   (default `c(0.05, 0.10)`). Set to `NULL` or `numeric(0)` to skip injected
#'   missingness.
#' @param imputation_methods Character vector of missing-data handlers to
#'   evaluate (`"complete_case"`, `"median"`, `"mean"`).
#' @param n_boot Integer number of nonparametric bootstrap resamples (default
#'   `100L`). Set to `0L` to skip bootstrap.
#' @param seed Integer RNG seed for reproducibility (default `42L`).
#' @param max_pct_change_warn Relative change threshold (`|delta / baseline|`)
#'   triggering `"warn"` (default `0.15`, i.e., 15%).
#' @param max_pct_change_fail Relative change threshold triggering `"fail"`
#'   (default `0.35`, i.e., 35%).
#' @param sign_flip_fail Logical; if `TRUE` (default), a sign reversal relative
#'   to a non-zero baseline triggers `"fail"`.
#' @param boot_ci_level Confidence level for bootstrap percentile intervals
#'   (default `0.95`).
#'
#' @return An S3 object of class `"statci_sensitivity"` containing `baseline`,
#'   `results` (tidy per-scenario tibble), `loo_details`, `bootstrap`,
#'   `parameter_summary`, and overall `status` (`"pass"`, `"warn"`, `"fail"`).
#' @export
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(x = rnorm(80), y = 2 + 1.5 * rnorm(80))
#' df$y <- 1 + 1.2 * df$x + rnorm(80, sd = 0.5)
#' sens <- audit_sensitivity(
#'   data = df,
#'   fit_fn = function(d) lm(y ~ x, data = d),
#'   n_boot = 40L,
#'   max_loo_units = 20L,
#'   seed = 42L
#' )
#' sens
audit_sensitivity <- function(data,
                              fit_fn,
                              extract_fn = NULL,
                              cluster_col = NULL,
                              numeric_cols = NULL,
                              winsor_probs = c(0.01, 0.05),
                              leave_one_out = TRUE,
                              max_loo_units = 50L,
                              missingness_rates = c(0.05, 0.10),
                              imputation_methods = c("complete_case", "median"),
                              n_boot = 100L,
                              seed = 42L,
                              max_pct_change_warn = 0.15,
                              max_pct_change_fail = 0.35,
                              sign_flip_fail = TRUE,
                              boot_ci_level = 0.95) {
  if (!is.data.frame(data) || nrow(data) < 4L) {
    rlang::abort("`data` must be a data.frame with at least 4 rows.")
  }
  if (!is.function(fit_fn)) {
    rlang::abort("`fit_fn` must be a function taking a data.frame.")
  }
  if (!is.null(cluster_col) && !(cluster_col %in% names(data))) {
    rlang::abort(paste0("`cluster_col` (`", cluster_col, "`) not found in `data`."))
  }

  withr::local_seed(
    as.integer(seed)[1L],
    .rng_kind = "Mersenne-Twister",
    .rng_normal_kind = "Inversion",
    .rng_sample_kind = "Rejection"
  )

  extract <- extract_fn %||% function(fit) default_extract_estimates(fit, include_intercept = FALSE)
  base_fit <- fit_fn(data)
  baseline <- extract(base_fit)
  if (!is.numeric(baseline) || length(baseline) == 0L) {
    rlang::abort("`extract_fn(fit_fn(data))` must return a non-empty numeric vector.")
  }
  if (is.null(names(baseline))) {
    names(baseline) <- paste0("param_", seq_along(baseline))
  }
  pnames <- names(baseline)

  # Identify continuous numeric columns suitable for winsorizing / perturbation
  if (is.null(numeric_cols)) {
    numeric_cols <- names(data)[vapply(names(data), function(col) {
      if (!is.null(cluster_col) && col == cluster_col) return(FALSE)
      v <- data[[col]]
      is.numeric(v) && length(unique(v[!is.na(v)])) > 2L
    }, logical(1L))]
  }

  safe_eval <- function(df) {
    tryCatch(
      suppressWarnings({
        est <- extract(fit_fn(df))
        if (is.null(names(est))) names(est) <- pnames[seq_along(est)]
        vals <- as.numeric(est[pnames])
        vals[!is.finite(vals)] <- NA_real_
        vals
      }),
      error = function(e) rep(NA_real_, length(pnames))
    )
  }

  score_shift <- function(base_v, pert_v) {
    if (is.na(pert_v) || !is.finite(pert_v)) {
      return(list(abs_diff = NA_real_, pct_change = NA_real_, sign_flip = NA, status = "fail"))
    }
    ad <- abs(pert_v - base_v)
    pc <- if (abs(base_v) > 1e-8) ad / abs(base_v) else ad
    sf <- abs(base_v) > 1e-8 && pert_v != 0 && (sign(pert_v) != sign(base_v))
    st <- if ((isTRUE(sign_flip_fail) && isTRUE(sf)) || pc > max_pct_change_fail) {
      "fail"
    } else if (pc > max_pct_change_warn || isTRUE(sf)) {
      "warn"
    } else {
      "pass"
    }
    list(abs_diff = ad, pct_change = pc, sign_flip = sf, status = st)
  }

  rows <- list()
  add_row <- function(category, scenario, param, base_v, pert_v, detail = "") {
    sc <- score_shift(base_v, pert_v)
    rows[[length(rows) + 1L]] <<- tibble::tibble(
      category = category,
      scenario = scenario,
      parameter = param,
      baseline = base_v,
      perturbed = pert_v,
      abs_diff = sc$abs_diff,
      pct_change = sc$pct_change,
      sign_flip = sc$sign_flip,
      detail = detail,
      status = sc$status
    )
  }

  # -------------------------------------------------------------------------
  # (a) Outlier Winsorization
  # -------------------------------------------------------------------------
  if (!is.null(winsor_probs) && length(winsor_probs) > 0L && length(numeric_cols) > 0L) {
    for (p in winsor_probs) {
      if (!is.numeric(p) || p <= 0 || p >= 0.5) next
      df_w <- data
      for (col in numeric_cols) {
        v <- df_w[[col]]
        if (all(is.na(v))) next
        qs <- stats::quantile(v, probs = c(p, 1 - p), na.rm = TRUE, names = FALSE)
        if (!all(is.finite(qs))) next
        v[!is.na(v) & v < qs[1L]] <- qs[1L]
        v[!is.na(v) & v > qs[2L]] <- qs[2L]
        df_w[[col]] <- v
      }
      est_w <- safe_eval(df_w)
      scen <- paste0("winsor_", formatC(100 * p, digits = 0L, format = "f"), "pct")
      for (j in seq_along(pnames)) {
        add_row(
          "winsorization", scen, pnames[j], baseline[j], est_w[j],
          paste0("Winsorized ", length(numeric_cols), " numeric col(s) at [", p, ", ", 1 - p, "]")
        )
      }
    }
  }

  # -------------------------------------------------------------------------
  # (b) Leave-One-Cluster-Out (LOCO) / Leave-One-Out (LOO) Jackknife
  # -------------------------------------------------------------------------
  loo_details <- tibble::tibble(
    unit = character(0), parameter = character(0), estimate = numeric(0),
    abs_diff = numeric(0), pct_change = numeric(0), sign_flip = logical(0)
  )

  if (isTRUE(leave_one_out)) {
    max_u <- as.integer(max_loo_units)[1L]
    if (!is.null(cluster_col)) {
      units_all <- unique(data[[cluster_col]])
      if (length(units_all) < 2L) {
        for (j in seq_along(pnames)) {
          rows[[length(rows) + 1L]] <- tibble::tibble(
            category = "influence", scenario = "leave_one_cluster_out",
            parameter = pnames[j], baseline = baseline[j], perturbed = baseline[j],
            abs_diff = 0, pct_change = 0, sign_flip = FALSE,
            detail = "Only 1 unique cluster present; LOCO skipped",
            status = "warn"
          )
        }
        units_use <- character(0)
      } else if (length(units_all) > max_u) {
        units_use <- sample(units_all, max_u)
      } else {
        units_use <- units_all
      }
      loo_label <- "leave_one_cluster_out"
    } else {
      n_row <- nrow(data)
      if (n_row > max_u) {
        # Prioritize highest leverage rows when baseline is lm/glm
        lev <- tryCatch(stats::hatvalues(base_fit), error = function(e) NULL)
        if (!is.null(lev) && length(lev) == n_row) {
          units_use <- order(lev, decreasing = TRUE)[seq_len(max_u)]
        } else {
          units_use <- sort(sample.int(n_row, max_u))
        }
      } else {
        units_use <- seq_len(n_row)
      }
      loo_label <- "leave_one_out"
    }

    if (length(units_use) > 0L) {
    loo_mat <- matrix(NA_real_, nrow = length(units_use), ncol = length(pnames))
    colnames(loo_mat) <- pnames

    for (u_i in seq_along(units_use)) {
      u_val <- units_use[u_i]
      df_sub <- if (!is.null(cluster_col)) {
        data[data[[cluster_col]] != u_val, , drop = FALSE]
      } else {
        data[-u_val, , drop = FALSE]
      }
      loo_mat[u_i, ] <- safe_eval(df_sub)
    }

    det_rows <- list()
    for (j in seq_along(pnames)) {
      vals <- loo_mat[, j]
      ok <- is.finite(vals)
      if (!any(ok)) {
        add_row("influence", loo_label, pnames[j], baseline[j], NA_real_, "All LOO refits failed")
        next
      }
      diffs <- abs(vals - baseline[j])
      worst_i <- which.max(ifelse(ok, diffs, -Inf))
      worst_est <- vals[worst_i]
      worst_unit <- as.character(units_use[worst_i])
      any_flip <- any(ok & abs(baseline[j]) > 1e-8 & vals != 0 & sign(vals) != sign(baseline[j]))
      n_ok <- sum(ok)
      jk_se <- if (n_ok >= 2L) sqrt((n_ok - 1) / n_ok * sum((vals[ok] - mean(vals[ok]))^2)) else NA_real_

      detail_msg <- paste0(
        "Most influential unit: ", worst_unit,
        " (", length(units_use), " evaluated; jackknife SE = ", fmt_num(jk_se, 4L),
        if (any_flip) "; SIGN FLIP DETECTED" else "", ")"
      )
      add_row("influence", loo_label, pnames[j], baseline[j], worst_est, detail_msg)

      for (u_i in seq_along(units_use)) {
        v <- vals[u_i]
        ad <- abs(v - baseline[j])
        pc <- if (abs(baseline[j]) > 1e-8) ad / abs(baseline[j]) else ad
        sf <- is.finite(v) && abs(baseline[j]) > 1e-8 && v != 0 && sign(v) != sign(baseline[j])
        det_rows[[length(det_rows) + 1L]] <- tibble::tibble(
          unit = as.character(units_use[u_i]),
          parameter = pnames[j],
          estimate = v,
          abs_diff = ad,
          pct_change = pc,
          sign_flip = sf
        )
      }
    }
    if (length(det_rows) > 0L) {
      loo_details <- tibble::as_tibble(do.call(rbind, det_rows))
    }
    }
  }

  # -------------------------------------------------------------------------
  # (c) Missingness & Imputation Stress Tests
  # -------------------------------------------------------------------------
  imputation_methods <- intersect(imputation_methods, c("complete_case", "median", "mean"))
  outcome_col <- tryCatch(
    {
      fv <- all.vars(stats::formula(base_fit))
      if (length(fv) >= 1L && fv[1L] %in% names(data)) fv[1L] else NULL
    },
    error = function(e) NULL
  )
  apply_imputation <- function(df, method) {
    if (method == "complete_case") {
      return(df[stats::complete.cases(df), , drop = FALSE])
    }
    df_imp <- df
    # Do not impute outcome column with a constant (causes severe attenuation bias)
    if (!is.null(outcome_col) && anyNA(df_imp[[outcome_col]])) {
      df_imp <- df_imp[!is.na(df_imp[[outcome_col]]), , drop = FALSE]
    }
    imp_cols <- setdiff(names(df_imp), c(outcome_col, cluster_col))
    for (col in imp_cols) {
      v <- df_imp[[col]]
      if (is.numeric(v) && anyNA(v)) {
        fill <- if (method == "median") stats::median(v, na.rm = TRUE) else mean(v, na.rm = TRUE)
        if (is.finite(fill)) v[is.na(v)] <- fill
        df_imp[[col]] <- v
      }
    }
    df_imp
  }

  # Observed missingness contrast if NAs already exist
  if (anyNA(data) && length(imputation_methods) > 0L) {
    for (mth in imputation_methods) {
      df_m <- apply_imputation(data, mth)
      est_m <- safe_eval(df_m)
      for (j in seq_along(pnames)) {
        add_row(
          "missingness", paste0("observed_", mth), pnames[j], baseline[j], est_m[j],
          paste0("Observed NAs handled via ", mth, " (n = ", nrow(df_m), ")")
        )
      }
    }
  }

  # Controlled MCAR missingness injection
  if (!is.null(missingness_rates) && length(missingness_rates) > 0L && length(imputation_methods) > 0L) {
    target_cols <- setdiff(names(data), cluster_col %||% character(0))
    for (rate in missingness_rates) {
      if (!is.numeric(rate) || rate <= 0 || rate >= 0.8) next
      df_mcar <- data
      for (col in target_cols) {
        obs_idx <- which(!is.na(df_mcar[[col]]))
        n_drop <- floor(length(obs_idx) * rate)
        if (n_drop >= 1L && n_drop < length(obs_idx) - 2L) {
          drop_idx <- sample(obs_idx, n_drop)
          df_mcar[[col]][drop_idx] <- NA
        }
      }
      for (mth in imputation_methods) {
        df_h <- apply_imputation(df_mcar, mth)
        est_h <- safe_eval(df_h)
        scen <- paste0("mcar_", formatC(100 * rate, digits = 0L, format = "f"), "pct_", mth)
        for (j in seq_along(pnames)) {
          add_row(
            "missingness", scen, pnames[j], baseline[j], est_h[j],
            paste0("Injected ", fmt_pct(rate, 0L), " MCAR + ", mth, " (n = ", nrow(df_h), ")")
          )
        }
      }
    }
  }

  # -------------------------------------------------------------------------
  # (d) Nonparametric Bootstrap Stability
  # -------------------------------------------------------------------------
  boot_tbl <- tibble::tibble(
    parameter = character(0), baseline = numeric(0), boot_mean = numeric(0),
    boot_se = numeric(0), ci_low = numeric(0), ci_high = numeric(0),
    sign_consistency = numeric(0), fail_rate = numeric(0), status = character(0)
  )

  n_boot <- as.integer(n_boot)[1L]
  if (!is.na(n_boot) && n_boot >= 5L) {
    boot_mat <- matrix(NA_real_, nrow = n_boot, ncol = length(pnames))
    colnames(boot_mat) <- pnames
    if (!is.null(cluster_col)) {
      cl_split <- split(seq_len(nrow(data)), data[[cluster_col]])
      n_cl <- length(cl_split)
      for (b in seq_len(n_boot)) {
        sampled_cl <- sample.int(n_cl, n_cl, replace = TRUE)
        idx <- unlist(cl_split[sampled_cl], use.names = FALSE)
        df_b <- data[idx, , drop = FALSE]
        # Give each resampled cluster copy a distinct cluster ID so MLM random effects work
        cl_lens <- vapply(cl_split[sampled_cl], length, integer(1L))
        df_b[[cluster_col]] <- rep(paste0("bcl_", seq_len(n_cl)), times = cl_lens)
        boot_mat[b, ] <- safe_eval(df_b)
      }
    } else {
      n_row <- nrow(data)
      for (b in seq_len(n_boot)) {
        idx <- sample.int(n_row, n_row, replace = TRUE)
        boot_mat[b, ] <- safe_eval(data[idx, , drop = FALSE])
      }
    }

    alpha_b <- (1 - boot_ci_level) / 2
    b_rows <- vector("list", length(pnames))
    for (j in seq_along(pnames)) {
      vals <- boot_mat[, j]
      ok <- is.finite(vals)
      f_rate <- 1 - mean(ok)
      if (sum(ok) < 3L) {
        b_rows[[j]] <- tibble::tibble(
          parameter = pnames[j], baseline = baseline[j], boot_mean = NA_real_,
          boot_se = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
          sign_consistency = NA_real_, fail_rate = f_rate, status = "fail"
        )
        next
      }
      v_ok <- vals[ok]
      b_mean <- mean(v_ok)
      b_se <- stats::sd(v_ok)
      qs <- stats::quantile(v_ok, probs = c(alpha_b, 1 - alpha_b), names = FALSE)
      sc <- if (abs(baseline[j]) > 1e-8) mean(sign(v_ok) == sign(baseline[j])) else 1.0
      b_st <- if (f_rate > 0.20 || (abs(baseline[j]) > 1e-8 && sc < 0.80)) {
        "fail"
      } else if (f_rate > 0.05 || (abs(baseline[j]) > 1e-8 && sc < 0.90)) {
        "warn"
      } else {
        "pass"
      }
      b_rows[[j]] <- tibble::tibble(
        parameter = pnames[j],
        baseline = baseline[j],
        boot_mean = b_mean,
        boot_se = b_se,
        ci_low = qs[1L],
        ci_high = qs[2L],
        sign_consistency = sc,
        fail_rate = f_rate,
        status = b_st
      )
      add_row(
        "bootstrap", paste0("bootstrap_", n_boot), pnames[j], baseline[j], b_mean,
        paste0("Boot SE = ", fmt_num(b_se, 4L), ", sign stability = ", fmt_pct(sc),
               ", CI [", fmt_num(qs[1L], 3L), ", ", fmt_num(qs[2L], 3L), "]")
      )
      # Override bootstrap row status with bootstrap-specific stability criteria
      rows[[length(rows)]]$status <- combine_status(c(rows[[length(rows)]]$status, b_st))
    }
    boot_tbl <- tibble::as_tibble(do.call(rbind, b_rows))
  }

  results_tbl <- if (length(rows) > 0L) {
    tibble::as_tibble(do.call(rbind, rows))
  } else {
    tibble::tibble(
      category = character(0), scenario = character(0), parameter = character(0),
      baseline = numeric(0), perturbed = numeric(0), abs_diff = numeric(0),
      pct_change = numeric(0), sign_flip = logical(0), detail = character(0),
      status = character(0)
    )
  }

  # Per-parameter rollup summary
  ps_rows <- lapply(pnames, function(pnm) {
    sub <- results_tbl[results_tbl$parameter == pnm, , drop = FALSE]
    b_row <- boot_tbl[boot_tbl$parameter == pnm, , drop = FALSE]
    tibble::tibble(
      parameter = pnm,
      baseline = baseline[[pnm]],
      n_scenarios = nrow(sub),
      max_pct_change = if (nrow(sub) > 0L && any(is.finite(sub$pct_change))) max(sub$pct_change, na.rm = TRUE) else NA_real_,
      any_sign_flip = any(isTRUE_vec(sub$sign_flip)),
      boot_sign_consistency = if (nrow(b_row) > 0L) b_row$sign_consistency[1L] else NA_real_,
      status = combine_status(c(sub$status, b_row$status))
    )
  })
  param_summary <- tibble::as_tibble(do.call(rbind, ps_rows))
  overall <- combine_status(param_summary$status)

  structure(
    list(
      baseline = baseline,
      results = results_tbl,
      loo_details = loo_details,
      bootstrap = boot_tbl,
      parameter_summary = param_summary,
      settings = list(
        cluster_col = cluster_col,
        winsor_probs = winsor_probs,
        missingness_rates = missingness_rates,
        n_boot = n_boot,
        max_pct_change_warn = max_pct_change_warn,
        max_pct_change_fail = max_pct_change_fail
      ),
      status = overall
    ),
    class = "statci_sensitivity"
  )
}

isTRUE_vec <- function(x) {
  !is.na(x) & as.logical(x)
}

#' @export
as_tibble.statci_sensitivity <- function(x, ...) {
  x$results
}

#' @export
print.statci_sensitivity <- function(x, ...) {
  badge <- format_status_badge(x$status)
  cli::cli_text(
    "{badge} {.strong statci Adversarial Sensitivity Battery}: ",
    "{length(x$baseline)} parameter(s), {nrow(x$results)} scenario check(s)"
  )
  for (i in seq_len(nrow(x$parameter_summary))) {
    r <- x$parameter_summary[i, ]
    b <- format_status_badge(r$status)
    cli::cli_text(
      "  {b} {r$parameter}: baseline = {fmt_num(r$baseline, 4L)}, ",
      "max % change = {fmt_pct(r$max_pct_change)}, sign flip = {r$any_sign_flip}, ",
      "boot sign stability = {fmt_pct(r$boot_sign_consistency)}"
    )
  }
  invisible(x)
}

#' Plot adversarial sensitivity shifts across scenarios
#'
#' @param x A `"statci_sensitivity"` object from [audit_sensitivity()].
#' @param parameter Optional parameter name to plot (defaults to all).
#' @param ... Unused.
#'
#' @return A `ggplot` object if `ggplot2` is installed, otherwise draws a base
#'   R dotchart and returns `x` invisibly.
#' @export
#'
#' @examples
#' sens <- audit_sensitivity(
#'   data = mtcars,
#'   fit_fn = function(d) lm(mpg ~ wt, data = d),
#'   n_boot = 20L,
#'   max_loo_units = 15L,
#'   seed = 42L
#' )
#' plot(sens)
plot.statci_sensitivity <- function(x, parameter = NULL, ...) {
  df <- x$results
  if (!is.null(parameter)) {
    df <- df[df$parameter %in% parameter, , drop = FALSE]
  }
  if (nrow(df) == 0L) {
    rlang::abort("No sensitivity scenarios available to plot.")
  }

  if (rlang::is_installed("ggplot2")) {
    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = .data[["perturbed"]], y = .data[["scenario"]], color = .data[["status"]])
    ) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = .data[["baseline"]]), linetype = "dashed", color = "#334155") +
      ggplot2::geom_point(size = 3) +
      ggplot2::scale_color_manual(values = c(pass = "#16A34A", warn = "#D97706", fail = "#DC2626")) +
      ggplot2::facet_wrap(~ parameter, scales = "free_x") +
      ggplot2::labs(
        title = "Adversarial Sensitivity Perturbations vs. Baseline",
        x = "Estimate (dashed line = baseline)",
        y = "Perturbation Scenario",
        color = "Status"
      ) +
      ggplot2::theme_minimal()
    return(p)
  }

  first_p <- df$parameter[1L]
  sub <- df[df$parameter == first_p, , drop = FALSE]
  graphics::dotchart(
    sub$perturbed,
    labels = sub$scenario,
    main = paste0("Sensitivity Perturbations: ", first_p),
    xlab = "Estimate"
  )
  graphics::abline(v = sub$baseline[1L], lty = 2)
  invisible(x)
}
