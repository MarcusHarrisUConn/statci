# Specification Curve / Multiverse Robustness Auditor

#' Audit analytical robustness across a multiverse specification curve
#'
#' Evaluates a modeling pipeline across the full Cartesian grid of analytical
#' decision forks (e.g., outlier handling, covariate sets, missing-data rules,
#' and estimator choices) following specification-curve / multiverse analysis.
#' Summarizes specification-level estimates, standard errors, confidence
#' intervals, p-values, convergence rates, overall median and interquartile
#' range (IQR), sign consistency, proportion statistically significant
#' (\eqn{p \le \alpha}), and per-fork variance decomposition (`eta_squared`)
#' identifying which analytical decision drives fragility.
#'
#' @param data Analysis dataset (typically a `data.frame`) passed as the first
#'   argument to `fit_fn`.
#' @param decisions Non-empty named list of analytical decision forks, where
#'   each element is a vector of candidate choices (e.g.,
#'   `list(outliers = c("none", "winsor"), covariates = c("minimal", "full"))`).
#' @param fit_fn Function `function(data, spec)` receiving `data` and a named
#'   list `spec` specifying one choice per decision fork, and returning either
#'   a fitted model (`lm`, `glm`, `lmerMod`, `htest`), a named list with
#'   `estimate` (and optional `se`, `ci_lower`, `ci_upper`, `p_value`,
#'   `converged`), or a numeric scalar/vector.
#' @param extract_fn Optional extractor `function(fit)` returning a named list
#'   (`estimate`, optional `se`, `ci_lower`, `ci_upper`, `p_value`,
#'   `converged`) or numeric vector. When `NULL`, `statci` auto-extracts from
#'   standard model objects and lists.
#' @param param Optional character scalar naming the focal parameter to audit
#'   when `fit_fn` returns multiple coefficients (defaults to the first
#'   non-intercept parameter).
#' @param alpha Nominal two-sided significance threshold for computing
#'   `prop_significant` and Wald confidence intervals (default `0.05`).
#' @param max_specs Maximum allowed Cartesian grid size (default `256L`) to
#'   prevent accidental combinatorial explosion in CI.
#' @param min_sign_consistency_warn Minimum sign-consistency proportion before
#'   emitting `"warn"` (default `0.90`).
#' @param min_sign_consistency_fail Minimum sign-consistency proportion before
#'   emitting `"fail"` (default `0.75`).
#' @param max_iqr_ratio_warn Maximum `IQR / |median|` spread ratio before
#'   emitting `"warn"` (default `0.50`).
#' @param max_iqr_ratio_fail Maximum `IQR / |median|` spread ratio before
#'   emitting `"fail"` (default `1.00`).
#' @param max_fail_rate Maximum tolerable specification failure/non-convergence
#'   rate before emitting `"fail"` (default `0.10`).
#'
#' @return An S3 object of class `"statci_multiverse"` containing
#'   `specifications` (sorted [tibble::tibble]), `summary` (list of overall
#'   robustness metrics), `fork_sensitivity` ([tibble::tibble] ranking each
#'   decision fork by variance explained), `decisions`, `settings`, and overall
#'   `status` (`"pass"`, `"warn"`, or `"fail"`).
#' @export
#'
#' @examples
#' set.seed(42)
#' df <- data.frame(x = rnorm(80), c1 = rnorm(80))
#' df$y <- 1 + 0.9 * df$x + 0.3 * df$c1 + rnorm(80, sd = 0.6)
#' mv <- audit_multiverse(
#'   data = df,
#'   decisions = list(
#'     outliers = c("none", "winsor_5pct"),
#'     covariates = c("bivariate", "adjusted")
#'   ),
#'   fit_fn = function(d, spec) {
#'     if (spec$outliers == "winsor_5pct") {
#'       qs <- quantile(d$y, c(0.05, 0.95))
#'       d$y <- pmin(pmax(d$y, qs[1]), qs[2])
#'     }
#'     if (spec$covariates == "bivariate") lm(y ~ x, data = d) else lm(y ~ x + c1, data = d)
#'   },
#'   param = "x"
#' )
#' mv
audit_multiverse <- function(data,
                             decisions,
                             fit_fn,
                             extract_fn = NULL,
                             param = NULL,
                             alpha = 0.05,
                             max_specs = 256L,
                             min_sign_consistency_warn = 0.90,
                             min_sign_consistency_fail = 0.75,
                             max_iqr_ratio_warn = 0.50,
                             max_iqr_ratio_fail = 1.00,
                             max_fail_rate = 0.10) {
  if (!is.list(decisions) || length(decisions) < 1L || is.null(names(decisions)) || any(!nzchar(names(decisions)))) {
    rlang::abort("`decisions` must be a non-empty named list of analytical decision choices.")
  }
  if (anyDuplicated(names(decisions))) {
    rlang::abort("`decisions` contains duplicate decision fork names.")
  }
  reserved_cols <- c(
    "spec_rank", "spec_id", "estimate", "se", "ci_lower", "ci_upper",
    "p_value", "significant", "converged", "error"
  )
  bad_forks <- intersect(names(decisions), reserved_cols)
  if (length(bad_forks) > 0L) {
    rlang::abort(paste0(
      "Decision fork name(s) collide with reserved result column(s): ",
      paste(bad_forks, collapse = ", "),
      ". Rename these decision forks (e.g., `se_type` instead of `se`)."
    ))
  }
  for (nm in names(decisions)) {
    if (length(decisions[[nm]]) < 1L) {
      rlang::abort(paste0("Decision fork `", nm, "` must contain at least one choice."))
    }
  }
  if (!is.function(fit_fn)) {
    rlang::abort("`fit_fn` must be a function of signature `function(data, spec)`.")
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    rlang::abort("`alpha` must be a finite numeric scalar strictly between 0 and 1.")
  }
  if (!is.numeric(max_specs) || length(max_specs) != 1L || !is.finite(max_specs) || max_specs < 1) {
    rlang::abort("`max_specs` must be a positive finite integer.")
  }
  max_specs <- as.integer(max_specs)[1L]

  # Guard combinatorial size BEFORE materializing expand.grid() to prevent OOM
  n_specs_est <- prod(as.numeric(lengths(decisions)))
  if (!is.finite(n_specs_est) || n_specs_est > max_specs) {
    rlang::abort(paste0(
      "Multiverse grid contains ", format(n_specs_est, scientific = FALSE, trim = TRUE),
      " specifications, exceeding `max_specs = ", max_specs,
      "`. Prune decision levels or raise `max_specs`."
    ))
  }

  grid_df <- expand.grid(decisions, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  n_specs <- nrow(grid_df)

  focal_name <- param
  est_v <- rep(NA_real_, n_specs)
  se_v <- rep(NA_real_, n_specs)
  cil_v <- rep(NA_real_, n_specs)
  ciu_v <- rep(NA_real_, n_specs)
  pv_v <- rep(NA_real_, n_specs)
  conv_v <- rep(FALSE, n_specs)
  err_v <- rep(NA_character_, n_specs)

  for (s in seq_len(n_specs)) {
    spec_list <- as.list(grid_df[s, , drop = FALSE])
    rec <- tryCatch(
      suppressWarnings({
        raw <- fit_fn(data, spec_list)
        if (!is.null(extract_fn)) raw <- extract_fn(raw)
        normalize_estimator_result(raw, alpha = alpha)
      }),
      error = function(e) list(error = conditionMessage(e))
    )

    if (!is.null(rec$error)) {
      err_v[s] <- rec$error
      next
    }
    if (!isTRUE(rec$converged)) {
      err_v[s] <- "Model did not converge"
      next
    }

    pnames <- names(rec$estimate)
    if (is.null(focal_name)) {
      non_int <- setdiff(pnames, "(Intercept)")
      focal_name <- if (length(non_int) > 0L) non_int[1L] else pnames[1L]
    }
    if (!(focal_name %in% pnames)) {
      err_v[s] <- paste0("Parameter `", focal_name, "` missing from specification output")
      next
    }

    ev <- as.numeric(rec$estimate[[focal_name]])
    if (!is.finite(ev)) {
      err_v[s] <- "Non-finite parameter estimate"
      next
    }
    est_v[s] <- ev
    se_v[s] <- as.numeric(rec$se[[focal_name]])
    cil_v[s] <- as.numeric(rec$ci_lower[[focal_name]])
    ciu_v[s] <- as.numeric(rec$ci_upper[[focal_name]])
    pv_v[s] <- as.numeric(rec$p_value[[focal_name]])
    conv_v[s] <- TRUE
  }

  spec_tbl <- tibble::as_tibble(grid_df)
  spec_tbl$spec_id <- seq_len(n_specs)
  spec_tbl$estimate <- est_v
  spec_tbl$se <- se_v
  spec_tbl$ci_lower <- cil_v
  spec_tbl$ci_upper <- ciu_v
  spec_tbl$p_value <- pv_v
  spec_tbl$significant <- !is.na(pv_v) & (pv_v <= alpha)
  spec_tbl$converged <- conv_v
  spec_tbl$error <- err_v

  # Order by estimate ascending (canonical specification curve ordering; failed specs last)
  ord <- order(spec_tbl$estimate, na.last = TRUE)
  spec_tbl <- spec_tbl[ord, , drop = FALSE]
  spec_tbl$spec_rank <- seq_len(n_specs)
  col_order <- c(
    "spec_rank", "spec_id", names(decisions),
    "estimate", "se", "ci_lower", "ci_upper", "p_value", "significant", "converged", "error"
  )
  spec_tbl <- spec_tbl[, col_order, drop = FALSE]

  ok <- is.finite(spec_tbl$estimate) & spec_tbl$converged
  n_valid <- sum(ok)
  fail_rate <- 1 - n_valid / n_specs

  if (n_valid == 0L) {
    sum_list <- list(
      parameter = focal_name %||% NA_character_,
      n_specs = as.integer(n_specs),
      n_valid = 0L,
      fail_rate = 1.0,
      median_est = NA_real_,
      mean_est = NA_real_,
      iqr_est = NA_real_,
      sd_est = NA_real_,
      min_est = NA_real_,
      max_est = NA_real_,
      iqr_ratio = NA_real_,
      dominant_sign = NA_integer_,
      sign_consistency = 0.0,
      prop_significant = NA_real_,
      most_influential_fork = NA_character_
    )
    fork_tbl <- tibble::tibble(
      decision = names(decisions),
      n_levels = vapply(decisions, length, integer(1L)),
      median_range = rep(NA_real_, length(decisions)),
      eta_squared = rep(NA_real_, length(decisions)),
      min_level_median = rep(NA_real_, length(decisions)),
      max_level_median = rep(NA_real_, length(decisions))
    )
    return(structure(
      list(
        specifications = spec_tbl,
        summary = sum_list,
        fork_sensitivity = fork_tbl,
        decisions = decisions,
        settings = list(alpha = alpha, max_specs = max_specs),
        status = "fail"
      ),
      class = "statci_multiverse"
    ))
  }

  v_ok <- spec_tbl$estimate[ok]
  p_ok <- spec_tbl$p_value[ok]
  med_est <- stats::median(v_ok)
  mean_est <- mean(v_ok)
  qs <- stats::quantile(v_ok, probs = c(0.25, 0.75), names = FALSE)
  iqr_est <- qs[2L] - qs[1L]
  sd_est <- if (n_valid >= 2L) stats::sd(v_ok) else 0
  iqr_ratio <- if (abs(med_est) > 1e-8) iqr_est / abs(med_est) else NA_real_

  pos_prop <- mean(v_ok > 0)
  neg_prop <- mean(v_ok < 0)
  zero_prop <- mean(v_ok == 0)
  if (pos_prop >= neg_prop && pos_prop >= zero_prop) {
    dom_sign <- 1L
    sign_cons <- pos_prop
  } else if (neg_prop >= pos_prop && neg_prop >= zero_prop) {
    dom_sign <- -1L
    sign_cons <- neg_prop
  } else {
    dom_sign <- 0L
    sign_cons <- zero_prop
  }

  prop_sig <- if (any(is.finite(p_ok))) mean(p_ok[is.finite(p_ok)] <= alpha) else NA_real_

  # Decompose variance & median range explained by each decision fork
  ss_tot <- sum((v_ok - mean_est)^2)
  valid_df <- spec_tbl[ok, , drop = FALSE]
  fork_rows <- lapply(names(decisions), function(dec_nm) {
    grp_keys <- as.character(valid_df[[dec_nm]])
    grp_keys[is.na(grp_keys)] <- "<NA>"
    grp_vals <- split(valid_df$estimate, grp_keys)
    meds <- vapply(grp_vals, stats::median, numeric(1L))
    means <- vapply(grp_vals, mean, numeric(1L))
    ns <- vapply(grp_vals, length, integer(1L))
    ss_between <- sum(ns * (means - mean_est)^2)
    eta2 <- if (ss_tot > 1e-12) ss_between / ss_tot else 0
    tibble::tibble(
      decision = dec_nm,
      n_levels = as.integer(length(decisions[[dec_nm]])),
      median_range = max(meds) - min(meds),
      eta_squared = eta2,
      min_level_median = min(meds),
      max_level_median = max(meds)
    )
  })
  fork_tbl <- tibble::as_tibble(do.call(rbind, fork_rows))
  fork_tbl <- fork_tbl[order(fork_tbl$eta_squared, fork_tbl$median_range, decreasing = TRUE), , drop = FALSE]
  most_inf <- if (nrow(fork_tbl) > 0L && (fork_tbl$eta_squared[1L] > 1e-12 || fork_tbl$median_range[1L] > 1e-12)) {
    fork_tbl$decision[1L]
  } else {
    NA_character_
  }

  # Unified status gate
  st <- if (fail_rate > max_fail_rate ||
            sign_cons < min_sign_consistency_fail ||
            (!is.na(iqr_ratio) && iqr_ratio > max_iqr_ratio_fail)) {
    "fail"
  } else if (fail_rate > 0 ||
             sign_cons < min_sign_consistency_warn ||
             (!is.na(iqr_ratio) && iqr_ratio > max_iqr_ratio_warn)) {
    "warn"
  } else {
    "pass"
  }

  sum_list <- list(
    parameter = focal_name,
    n_specs = as.integer(n_specs),
    n_valid = as.integer(n_valid),
    fail_rate = fail_rate,
    median_est = med_est,
    mean_est = mean_est,
    iqr_est = iqr_est,
    sd_est = sd_est,
    min_est = min(v_ok),
    max_est = max(v_ok),
    iqr_ratio = iqr_ratio,
    dominant_sign = dom_sign,
    sign_consistency = sign_cons,
    prop_significant = prop_sig,
    most_influential_fork = most_inf
  )

  structure(
    list(
      specifications = spec_tbl,
      summary = sum_list,
      fork_sensitivity = fork_tbl,
      decisions = decisions,
      settings = list(
        alpha = alpha,
        max_specs = max_specs,
        min_sign_consistency_warn = min_sign_consistency_warn,
        min_sign_consistency_fail = min_sign_consistency_fail,
        max_iqr_ratio_warn = max_iqr_ratio_warn,
        max_iqr_ratio_fail = max_iqr_ratio_fail,
        max_fail_rate = max_fail_rate
      ),
      status = st
    ),
    class = "statci_multiverse"
  )
}

#' @export
as_tibble.statci_multiverse <- function(x, ...) {
  x$specifications
}

#' @export
print.statci_multiverse <- function(x, ...) {
  badge <- format_status_badge(x$status)
  s <- x$summary
  cli::cli_text(
    "{badge} {.strong statci Multiverse Specification Curve} ",
    "({s$parameter}: {s$n_valid}/{s$n_specs} valid specs)"
  )
  cli::cli_text(
    "  median = {fmt_num(s$median_est, 4L)}, IQR = {fmt_num(s$iqr_est, 4L)} ",
    "[min = {fmt_num(s$min_est, 4L)}, max = {fmt_num(s$max_est, 4L)}]"
  )
  cli::cli_text(
    "  sign consistency = {fmt_pct(s$sign_consistency)}, ",
    "p <= {x$settings$alpha} = {fmt_pct(s$prop_significant)}, ",
    "top driver = {s$most_influential_fork %||% 'none'}"
  )
  invisible(x)
}

#' Summarize a multiverse specification-curve audit
#'
#' @param object A `"statci_multiverse"` object from [audit_multiverse()].
#' @param ... Unused.
#'
#' @return Invisibly returns a list containing `overall` (1-row [tibble::tibble])
#'   and `fork_sensitivity` ([tibble::tibble]).
#' @export
#'
#' @examples
#' df <- data.frame(x = rnorm(50), y = rnorm(50))
#' mv <- audit_multiverse(
#'   df,
#'   decisions = list(model = c("a", "b")),
#'   fit_fn = function(d, spec) lm(y ~ x, data = d)
#' )
#' summary(mv)
summary.statci_multiverse <- function(object, ...) {
  s <- object$summary
  overall_tbl <- tibble::tibble(
    parameter = s$parameter,
    n_specs = s$n_specs,
    n_valid = s$n_valid,
    median = s$median_est,
    iqr = s$iqr_est,
    min = s$min_est,
    max = s$max_est,
    sign_consistency = s$sign_consistency,
    prop_significant = s$prop_significant,
    top_driver = s$most_influential_fork %||% NA_character_,
    status = object$status
  )
  print(object)
  if (nrow(object$fork_sensitivity) > 0L) {
    cli::cli_text("  {.emph Decision Fork Variance Decomposition}:")
    for (i in seq_len(nrow(object$fork_sensitivity))) {
      r <- object$fork_sensitivity[i, ]
      cli::cli_text(
        "    - {r$decision} ({r$n_levels} levels): median range = {fmt_num(r$median_range, 4L)}, ",
        "eta^2 = {fmt_pct(r$eta_squared)}"
      )
    }
  }
  invisible(list(overall = overall_tbl, fork_sensitivity = object$fork_sensitivity))
}

#' Plot a two-panel publication-ready specification curve and decision matrix
#'
#' Renders a two-panel visualization aligning each sorted specification's
#' point estimate and 95% confidence interval (top panel) directly above its
#' active analytical decision choices (bottom panel).
#'
#' @param x A `"statci_multiverse"` object from [audit_multiverse()].
#' @param ... Unused.
#'
#' @return A `ggplot` object if `ggplot2` is installed, otherwise draws a
#'   two-panel base R plot and returns `x` invisibly.
#' @export
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(x = rnorm(60), c1 = rnorm(60))
#' df$y <- 1 + 0.8 * df$x + rnorm(60)
#' mv <- audit_multiverse(
#'   df,
#'   decisions = list(cov = c("none", "c1"), trim = c("no", "yes")),
#'   fit_fn = function(d, spec) {
#'     if (spec$trim == "yes") d <- d[-1, ]
#'     if (spec$cov == "none") lm(y ~ x, data = d) else lm(y ~ x + c1, data = d)
#'   },
#'   param = "x"
#' )
#' plot(mv)
plot.statci_multiverse <- function(x, ...) {
  df <- x$specifications[x$specifications$converged & is.finite(x$specifications$estimate), , drop = FALSE]
  if (nrow(df) == 0L) {
    rlang::abort("No converged specifications available to plot.")
  }
  df$plot_rank <- seq_len(nrow(df))
  dec_names <- names(x$decisions)

  # Build long decision-matrix rows with unique ordered choice labels
  choice_labels <- character(0)
  for (dn in dec_names) {
    lvls <- as.character(x$decisions[[dn]])
    lvls[is.na(lvls)] <- "<NA>"
    choice_labels <- c(choice_labels, paste0(dn, ": ", lvls))
  }
  choice_labels <- rev(unique(choice_labels))
  choice_y <- stats::setNames(seq_along(choice_labels), choice_labels)

  mat_rows <- list()
  for (i in seq_len(nrow(df))) {
    rk <- df$plot_rank[i]
    sig <- if (isTRUE(df$significant[i])) "Significant" else "Non-significant"
    for (dn in dec_names) {
      v_str <- as.character(df[[dn]][i])
      if (is.na(v_str)) v_str <- "<NA>"
      lbl <- paste0(dn, ": ", v_str)
      mat_rows[[length(mat_rows) + 1L]] <- data.frame(
        plot_rank = rk,
        y_val = unname(choice_y[[lbl]]),
        choice = lbl,
        sig_group = sig,
        stringsAsFactors = FALSE
      )
    }
  }
  mat_df <- do.call(rbind, mat_rows)

  if (rlang::is_installed("ggplot2")) {
    # Assemble two-panel faceted ggplot sharing exact x = plot_rank alignment
    top_df <- data.frame(
      plot_rank = df$plot_rank,
      y_val = df$estimate,
      ymin = ifelse(is.finite(df$ci_lower), df$ci_lower, df$estimate),
      ymax = ifelse(is.finite(df$ci_upper), df$ci_upper, df$estimate),
      sig_group = ifelse(df$significant, "Significant", "Non-significant"),
      panel = "Specification Curve (Estimate & 95% CI)",
      stringsAsFactors = FALSE
    )
    bot_df <- data.frame(
      plot_rank = mat_df$plot_rank,
      y_val = mat_df$y_val,
      ymin = mat_df$y_val,
      ymax = mat_df$y_val,
      sig_group = mat_df$sig_group,
      panel = "Analytical Decision Matrix",
      stringsAsFactors = FALSE
    )
    lbl_df <- data.frame(
      plot_rank = 0.5,
      y_val = seq_along(choice_labels),
      label = choice_labels,
      panel = "Analytical Decision Matrix",
      stringsAsFactors = FALSE
    )
    ref_df <- data.frame(
      y_ref = c(0, x$summary$median_est),
      lty = c("Zero", "Median"),
      panel = "Specification Curve (Estimate & 95% CI)",
      stringsAsFactors = FALSE
    )

    comb_df <- rbind(top_df, bot_df)
    comb_df$panel <- factor(
      comb_df$panel,
      levels = c("Specification Curve (Estimate & 95% CI)", "Analytical Decision Matrix")
    )
    lbl_df$panel <- factor(lbl_df$panel, levels = levels(comb_df$panel))
    ref_df$panel <- factor(ref_df$panel, levels = levels(comb_df$panel))

    p <- ggplot2::ggplot(comb_df, ggplot2::aes(x = .data[["plot_rank"]], y = .data[["y_val"]], color = .data[["sig_group"]])) +
      ggplot2::geom_hline(
        data = ref_df,
        ggplot2::aes(yintercept = .data[["y_ref"]], linetype = .data[["lty"]]),
        color = "#475569",
        inherit.aes = FALSE
      ) +
      ggplot2::geom_linerange(
        data = top_df,
        ggplot2::aes(ymin = .data[["ymin"]], ymax = .data[["ymax"]]),
        alpha = 0.45,
        linewidth = 0.7
      ) +
      ggplot2::geom_point(size = 2.4) +
      ggplot2::geom_text(
        data = lbl_df,
        ggplot2::aes(x = .data[["plot_rank"]], y = .data[["y_val"]], label = .data[["label"]]),
        hjust = 0,
        vjust = -0.35,
        size = 2.8,
        color = "#334155",
        inherit.aes = FALSE
      ) +
      ggplot2::scale_color_manual(values = c("Significant" = "#264653", "Non-significant" = "#94A3B8")) +
      ggplot2::scale_linetype_manual(values = c("Zero" = "dotted", "Median" = "dashed")) +
      ggplot2::facet_grid(panel ~ ., scales = "free_y") +
      ggplot2::labs(
        title = paste0("Multiverse Specification Curve: ", x$summary$parameter),
        subtitle = paste0(
          x$summary$n_valid, " specifications | median = ", fmt_num(x$summary$median_est, 3L),
          ", sign consistency = ", fmt_pct(x$summary$sign_consistency),
          ", p <= ", x$settings$alpha, " = ", fmt_pct(x$summary$prop_significant)
        ),
        x = "Specification Rank (Ordered by Estimate)",
        y = NULL,
        color = NULL,
        linetype = NULL
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")
    return(p)
  }

  # Base graphics fallback (preserving caller par() cleanly!)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mfrow = c(2, 1), mar = c(3, 8, 2.5, 1.5))
  cols <- ifelse(df$significant, "#264653", "#94A3B8")
  y_rng <- range(c(df$estimate, df$ci_lower, df$ci_upper, 0), na.rm = TRUE)
  graphics::plot(
    df$plot_rank, df$estimate, pch = 19, col = cols, ylim = y_rng,
    xlab = "", ylab = x$summary$parameter,
    main = paste0("Specification Curve: ", x$summary$parameter)
  )
  graphics::abline(h = c(0, x$summary$median_est), lty = c(3, 2), col = "#475569")
  graphics::plot(
    mat_df$plot_rank, mat_df$y_val, pch = 15, col = ifelse(mat_df$sig_group == "Significant", "#264653", "#94A3B8"),
    yaxt = "n", xlab = "Specification Rank", ylab = "", ylim = c(0.5, length(choice_labels) + 0.5)
  )
  graphics::axis(2, at = seq_along(choice_labels), labels = choice_labels, las = 1, cex.axis = 0.75)
  invisible(x)
}
