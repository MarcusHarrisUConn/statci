# Shared internal utilities for statci

#' Combine status strings ("pass", "warn", "fail") monotonically
#' @param statuses Character vector of statuses.
#' @return Single character scalar: `"fail"`, `"warn"`, or `"pass"`.
#' @noRd
combine_status <- function(statuses) {
  statuses <- tolower(as.character(statuses))
  statuses <- statuses[!is.na(statuses) & nzchar(statuses)]
  if (length(statuses) == 0L) {
    return("pass")
  }
  if (any(statuses == "fail")) {
    return("fail")
  }
  if (any(statuses == "warn")) {
    return("warn")
  }
  "pass"
}

#' Map lint severity ("error", "warning", "info") to report status
#' @param severities Character vector of lint severities.
#' @return Single character scalar: `"fail"`, `"warn"`, or `"pass"`.
#' @noRd
lint_severity_to_status <- function(severities) {
  severities <- tolower(as.character(severities))
  if (length(severities) == 0L) {
    return("pass")
  }
  if (any(severities == "error")) {
    return("fail")
  }
  if (any(severities == "warning")) {
    return("warn")
  }
  "pass"
}

#' Format status badge for CLI printing
#' @param status Character scalar ("pass", "warn", "fail").
#' @return Character string styled via cli.
#' @noRd
format_status_badge <- function(status) {
  switch(
    tolower(status),
    pass = cli::col_green("[PASS]"),
    warn = cli::col_yellow("[WARN]"),
    fail = cli::col_red("[FAIL]"),
    paste0("[", toupper(status), "]")
  )
}

#' Safely extract a standardized numeric vector from a model or list
#' @param x Object returned by a fitting or extraction function.
#' @param include_intercept Logical; whether to keep "(Intercept)" when
#'   auto-extracting from fitted models.
#' @return Named numeric vector.
#' @noRd
default_extract_estimates <- function(x, include_intercept = FALSE) {
  if (is.numeric(x)) {
    if (is.null(names(x))) {
      names(x) <- paste0("param_", seq_along(x))
    }
    return(x)
  }
  if (is.list(x) && !inherits(x, c("lm", "glm", "htest")) && "estimate" %in% names(x)) {
    est <- as.numeric(x$estimate)
    nm <- names(x$estimate)
    if (is.null(nm)) {
      nm <- paste0("param_", seq_along(est))
    }
    names(est) <- nm
    return(est)
  }
  if (inherits(x, c("lmerMod", "glmerMod", "nlmerMod"))) {
    if (rlang::is_installed("lme4")) {
      cf <- lme4::fixef(x)
    } else {
      cf <- stats::coef(summary(x))[, 1L]
    }
    if (!include_intercept) {
      keep <- setdiff(names(cf), "(Intercept)")
      if (length(keep) > 0L) cf <- cf[keep]
    }
    return(cf)
  }
  if (inherits(x, c("lm", "glm"))) {
    cf <- stats::coef(x)
    if (!include_intercept) {
      keep <- setdiff(names(cf), "(Intercept)")
      if (length(keep) > 0L) cf <- cf[keep]
    }
    return(cf)
  }
  if (inherits(x, "htest")) {
    est <- as.numeric(x$estimate)
    nm <- names(x$estimate) %||% "estimate"
    names(est) <- nm
    return(est)
  }
  rlang::abort(
    paste0(
      "Cannot auto-extract estimates from object of class `",
      paste(class(x), collapse = "/"),
      "`. Supply `extract_fn` explicitly."
    )
  )
}

#' Normalize a single estimator output into a standard record
#' @param res Raw return value from user `estimator(data)`.
#' @param alpha Nominal significance level for fallback Wald CIs.
#' @return Named list with estimate, se, ci_lower, ci_upper, p_value,
#'   converged, singular.
#' @noRd
normalize_estimator_result <- function(res, alpha = 0.05) {
  z_crit <- stats::qnorm(1 - alpha / 2)

  # Case 1: lmerMod / glmerMod
  if (inherits(res, c("lmerMod", "glmerMod", "nlmerMod"))) {
    sm <- summary(res)$coefficients
    est <- sm[, "Estimate"]
    se <- sm[, "Std. Error"]
    if ("Pr(>|t|)" %in% colnames(sm)) {
      pval <- sm[, "Pr(>|t|)"]
    } else if ("Pr(>|z|)" %in% colnames(sm)) {
      pval <- sm[, "Pr(>|z|)"]
    } else {
      stat_col <- if ("t value" %in% colnames(sm)) "t value" else "z value"
      pval <- 2 * stats::pnorm(-abs(sm[, stat_col]))
    }
    ci_l <- est - z_crit * se
    ci_u <- est + z_crit * se
    singular <- if (rlang::is_installed("lme4")) {
      isTRUE(tryCatch(lme4::isSingular(res), error = function(e) FALSE))
    } else {
      FALSE
    }
    conv <- TRUE
    optinfo <- tryCatch(res@optinfo, error = function(e) NULL)
    if (!is.null(optinfo)) {
      if (!is.null(optinfo$conv$opt) && !identical(as.integer(optinfo$conv$opt), 0L)) {
        conv <- FALSE
      }
      msgs <- c(optinfo$conv$lme4$messages, optinfo$warnings)
      if (length(msgs) > 0L && any(grepl("failed to converge|unable to evaluate", msgs, ignore.case = TRUE))) {
        conv <- FALSE
      }
    }
    return(list(
      estimate = est,
      se = se,
      ci_lower = ci_l,
      ci_upper = ci_u,
      p_value = pval,
      converged = conv,
      singular = singular
    ))
  }

  # Case 2: lm / glm
  if (inherits(res, c("lm", "glm"))) {
    sm <- summary(res)$coefficients
    est <- sm[, 1L]
    se <- sm[, 2L]
    pval <- sm[, 4L]
    names(est) <- rownames(sm)
    names(se) <- rownames(sm)
    names(pval) <- rownames(sm)
    ci <- tryCatch(
      suppressMessages(stats::confint.default(res, level = 1 - alpha)),
      error = function(e) cbind(est - z_crit * se, est + z_crit * se)
    )
    if (inherits(res, "lm") && !inherits(res, "glm")) {
      df_res <- stats::df.residual(res)
      if (!is.null(df_res) && is.finite(df_res) && df_res > 0) {
        t_crit <- stats::qt(1 - alpha / 2, df = df_res)
        ci <- cbind(est - t_crit * se, est + t_crit * se)
      }
    }
    ci_l <- ci[names(est), 1L]
    ci_u <- ci[names(est), 2L]
    conv <- if (inherits(res, "glm")) isTRUE(res$converged) else TRUE
    return(list(
      estimate = est,
      se = se,
      ci_lower = ci_l,
      ci_upper = ci_u,
      p_value = pval,
      converged = conv,
      singular = any(is.na(stats::coef(res)))
    ))
  }

  # Case 3: htest
  if (inherits(res, "htest")) {
    est <- as.numeric(res$estimate)
    if (length(est) == 2L && grepl("Two Sample|Welch", res$method %||% "")) {
      est <- est[1L] - est[2L]
      names(est) <- "diff"
    } else if (is.null(names(est))) {
      names(est) <- "estimate"
    }
    ci <- res$conf.int
    ci_l <- if (!is.null(ci) && length(ci) == 2L) stats::setNames(ci[1L], names(est)[1L]) else stats::setNames(NA_real_, names(est)[1L])
    ci_u <- if (!is.null(ci) && length(ci) == 2L) stats::setNames(ci[2L], names(est)[1L]) else stats::setNames(NA_real_, names(est)[1L])
    se <- if (!is.null(res$stderr)) {
      stats::setNames(as.numeric(res$stderr)[1L], names(est)[1L])
    } else if (is.finite(ci_l) && is.finite(ci_u)) {
      stats::setNames((ci_u - ci_l) / (2 * z_crit), names(est)[1L])
    } else {
      stats::setNames(NA_real_, names(est)[1L])
    }
    pval <- stats::setNames(as.numeric(res$p.value)[1L], names(est)[1L])
    return(list(
      estimate = est,
      se = se,
      ci_lower = ci_l,
      ci_upper = ci_u,
      p_value = pval,
      converged = TRUE,
      singular = FALSE
    ))
  }

  # Case 4: numeric vector (point estimates only)
  if (is.numeric(res)) {
    est <- res
    if (is.null(names(est))) {
      names(est) <- paste0("param_", seq_along(est))
    }
    na_vec <- stats::setNames(rep(NA_real_, length(est)), names(est))
    return(list(
      estimate = est,
      se = na_vec,
      ci_lower = na_vec,
      ci_upper = na_vec,
      p_value = na_vec,
      converged = TRUE,
      singular = FALSE
    ))
  }

  # Case 5: user-supplied list
  if (is.list(res) && "estimate" %in% names(res)) {
    est <- as.numeric(res$estimate)
    nm <- names(res$estimate)
    if (is.null(nm)) {
      nm <- if (!is.null(res$term)) as.character(res$term) else paste0("param_", seq_along(est))
    }
    names(est) <- nm

    se <- if (!is.null(res$se)) {
      stats::setNames(as.numeric(res$se), nm)
    } else {
      stats::setNames(rep(NA_real_, length(est)), nm)
    }
    ci_l <- if (!is.null(res$ci_lower)) {
      stats::setNames(as.numeric(res$ci_lower), nm)
    } else if (all(!is.na(se))) {
      stats::setNames(est - z_crit * se, nm)
    } else {
      stats::setNames(rep(NA_real_, length(est)), nm)
    }
    ci_u <- if (!is.null(res$ci_upper)) {
      stats::setNames(as.numeric(res$ci_upper), nm)
    } else if (all(!is.na(se))) {
      stats::setNames(est + z_crit * se, nm)
    } else {
      stats::setNames(rep(NA_real_, length(est)), nm)
    }
    pval <- if (!is.null(res$p_value)) {
      stats::setNames(as.numeric(res$p_value), nm)
    } else if (all(!is.na(se)) && all(se > 0)) {
      stats::setNames(2 * stats::pnorm(-abs(est / se)), nm)
    } else {
      stats::setNames(rep(NA_real_, length(est)), nm)
    }
    conv <- if (!is.null(res$converged)) isTRUE(res$converged) else TRUE
    singular <- if (!is.null(res$singular)) isTRUE(res$singular) else FALSE

    return(list(
      estimate = est,
      se = se,
      ci_lower = ci_l,
      ci_upper = ci_u,
      p_value = pval,
      converged = conv,
      singular = singular
    ))
  }

  rlang::abort(
    paste0(
      "Unsupported return type from `estimator`: class `",
      paste(class(res), collapse = "/"),
      "`. Return a named list with `estimate` (and optional `se`, `ci_lower`, `ci_upper`, `p_value`, `converged`, `singular`), a numeric vector, or an `lm`/`glm`/`lmerMod`/`htest` object."
    )
  )
}

#' Format numeric scalar nicely for reports
#' @noRd
fmt_num <- function(x, digits = 3L) {
  ifelse(is.na(x) | !is.finite(x), "\u2014", formatC(x, digits = digits, format = "f"))
}

#' Format percentage nicely
#' @noRd
fmt_pct <- function(x, digits = 1L) {
  ifelse(is.na(x) | !is.finite(x), "\u2014", paste0(formatC(100 * x, digits = digits, format = "f"), "%"))
}
