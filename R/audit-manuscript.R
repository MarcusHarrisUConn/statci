# Manuscript-to-Code Consistency Auditor for Quarto (.qmd) and RMarkdown (.Rmd)

#' Audit a Quarto/RMarkdown manuscript against saved R model objects
#'
#' Parses inline statistical claims in `.qmd`, `.Rmd`, or `.md` prose (point
#' estimates, standard errors, p-values, confidence intervals, test statistics,
#' sample sizes, \eqn{R^2}, AIC/BIC) and cross-checks them against fitted R
#' model objects (`lm`, `glm`, `lmerMod`, `htest`, or tidy summary data frames).
#' Detects exact value mismatches, sign flips, rounding discrepancies, stale
#' sample sizes, and violated p-value inequalities.
#'
#' Supports both explicit annotations for zero-ambiguity binding:
#' - HTML comment anchors: `<!-- statci: model=fit1 term=x1 -->`
#' - Quarto inline spans: `[b = 0.42, SE = 0.11, p = .003]{.statci model="fit1" term="x1"}`
#'
#' and automatic untagged prose matching (matching term names mentioned in prose
#' or single-model/cohesive clause matching).
#'
#' @param manuscript Path to a `.qmd`, `.Rmd`, or `.md` file.
#' @param models One of:
#'   - A named list of fitted model objects (`lm`, `glm`, `lmerMod`, `htest`)
#'     or tidy summary data frames;
#'   - A single fitted model object or tidy summary data frame;
#'   - Path to an `.rds` file containing a model or named list of models;
#'   - Path to a directory containing `.rds` model files (keyed by filename stem).
#' @param tol_extra Non-negative numeric extra tolerance added on top of the
#'   automatic rounding half-unit (`0.5 * 10^(-decimals)`). Default `0`.
#' @param warn_rounding Logical; if `TRUE` (default), values off by at most one
#'   unit in the last reported decimal place are flagged as `"warn"`
#'   (`rounding_discrepancy`) rather than `"fail"`.
#' @param default_model Optional default model name used for untagged claims
#'   when multiple models are provided.
#'
#' @return An S3 object of class `"statci_manuscript_audit"` containing `checks`
#'   (a [tibble::tibble]), `summary`, `manuscript`, `models_checked`, and
#'   overall `status` (`"pass"`, `"warn"`, or `"fail"`).
#' @export
#'
#' @examples
#' fit <- lm(mpg ~ wt, data = mtcars)
#' txt <- paste0(
#'   "<!-- statci: model=fit term=wt -->\n",
#'   "Weight predicted MPG (b = ", round(coef(fit)["wt"], 2),
#'   ", N = ", nobs(fit), ").\n"
#' )
#' audit_manuscript_text(txt, models = list(fit = fit))
audit_manuscript <- function(manuscript,
                             models,
                             tol_extra = 0,
                             warn_rounding = TRUE,
                             default_model = NULL) {
  if (!is.character(manuscript) || length(manuscript) != 1L || !file.exists(manuscript)) {
    rlang::abort(paste0("Manuscript file not found: `", manuscript, "`."))
  }
  lines <- readLines(manuscript, warn = FALSE, encoding = "UTF-8")
  audit_manuscript_lines(
    lines = lines,
    source_label = manuscript,
    models = models,
    tol_extra = tol_extra,
    warn_rounding = warn_rounding,
    default_model = default_model
  )
}

#' Audit manuscript prose supplied as an in-memory string
#'
#' In-memory counterpart to [audit_manuscript()].
#'
#' @param text Character scalar or vector of manuscript lines.
#' @inheritParams audit_manuscript
#'
#' @return An S3 object of class `"statci_manuscript_audit"`.
#' @export
#'
#' @examples
#' fit <- lm(mpg ~ wt, data = mtcars)
#' audit_manuscript_text(
#'   "<!-- statci: model=fit term=wt -->\nEffect: b = -5.34, N = 32.",
#'   models = list(fit = fit)
#' )
audit_manuscript_text <- function(text,
                                  models,
                                  tol_extra = 0,
                                  warn_rounding = TRUE,
                                  default_model = NULL) {
  if (!is.character(text)) {
    rlang::abort("`text` must be a character vector.")
  }
  lines <- unlist(strsplit(paste(text, collapse = "\n"), "\n", fixed = TRUE))
  if (length(lines) == 0L) lines <- ""
  audit_manuscript_lines(
    lines = lines,
    source_label = "<text>",
    models = models,
    tol_extra = tol_extra,
    warn_rounding = warn_rounding,
    default_model = default_model
  )
}

# ---------------------------------------------------------------------------
# Model catalog loader & uniform extractor
# ---------------------------------------------------------------------------

load_models_input <- function(models) {
  if (is.character(models) && length(models) == 1L) {
    if (dir.exists(models)) {
      rds_files <- sort(list.files(models, pattern = "\\.rds$", ignore.case = TRUE, full.names = TRUE))
      if (length(rds_files) == 0L) {
        rlang::abort(paste0("No `.rds` files found in directory `", models, "`."))
      }
      objs <- lapply(rds_files, readRDS)
      names(objs) <- tools::file_path_sans_ext(basename(rds_files))
      return(objs)
    }
    if (file.exists(models)) {
      obj <- readRDS(models)
      if (is.list(obj) && !inherits(obj, c("lm", "glm", "htest", "data.frame", "lmerMod", "glmerMod"))) {
        if (is.null(names(obj))) {
          names(obj) <- paste0("model_", seq_along(obj))
        }
        return(obj)
      }
      nm <- tools::file_path_sans_ext(basename(models))
      return(stats::setNames(list(obj), nm))
    }
    rlang::abort(paste0("Model path not found: `", models, "`."))
  }

  # Single model object or data frame
  if (inherits(models, c("lm", "glm", "htest", "lmerMod", "glmerMod", "nlmerMod", "data.frame"))) {
    return(list(model = models))
  }

  if (is.list(models) && length(models) > 0L) {
    if (is.null(names(models)) || any(!nzchar(names(models)))) {
      nm <- names(models) %||% rep("", length(models))
      empty <- !nzchar(nm)
      nm[empty] <- paste0("model_", which(empty))
      names(models) <- nm
    }
    return(models)
  }

  rlang::abort("`models` must be a model object, a named list of models, an `.rds` file, or a directory of `.rds` files.")
}

extract_single_model <- function(m) {
  fit_stats <- list(
    n = NA_real_, r_squared = NA_real_, adj_r_squared = NA_real_,
    aic = NA_real_, bic = NA_real_, df_residual = NA_real_, r = NA_real_
  )

  # 1. lmerMod / glmerMod / lmerModLmerTest
  if (inherits(m, c("lmerMod", "glmerMod", "nlmerMod"))) {
    sm <- summary(m)$coefficients
    est <- sm[, "Estimate"]
    se <- sm[, "Std. Error"]
    stat_col <- intersect(c("t value", "z value"), colnames(sm))
    stat_v <- if (length(stat_col) > 0L) sm[, stat_col[1L]] else est / se
    df_col <- intersect(c("df"), colnames(sm))
    df_v <- if (length(df_col) > 0L) sm[, df_col[1L]] else rep(NA_real_, length(est))
    p_col <- intersect(c("Pr(>|t|)", "Pr(>|z|)"), colnames(sm))
    pval <- if (length(p_col) > 0L) sm[, p_col[1L]] else 2 * stats::pnorm(-abs(stat_v))
    z95 <- stats::qnorm(0.975)

    terms_tbl <- tibble::tibble(
      term = rownames(sm),
      estimate = as.numeric(est),
      or = exp(as.numeric(est)),
      se = as.numeric(se),
      statistic = as.numeric(stat_v),
      df = as.numeric(df_v),
      p_value = as.numeric(pval),
      ci_lower = as.numeric(est - z95 * se),
      ci_upper = as.numeric(est + z95 * se)
    )
    fit_stats$n <- tryCatch(as.numeric(stats::nobs(m)), error = function(e) NA_real_)
    fit_stats$aic <- tryCatch(as.numeric(stats::AIC(m)), error = function(e) NA_real_)
    fit_stats$bic <- tryCatch(as.numeric(stats::BIC(m)), error = function(e) NA_real_)
    return(list(terms = terms_tbl, fit = fit_stats, class = class(m)[1L]))
  }

  # 2. lm / glm
  if (inherits(m, c("lm", "glm"))) {
    sm_obj <- summary(m)
    sm <- sm_obj$coefficients
    est <- sm[, 1L]
    se <- sm[, 2L]
    stat_v <- sm[, 3L]
    pval <- sm[, 4L]
    df_res <- tryCatch(as.numeric(stats::df.residual(m)), error = function(e) NA_real_)

    if (inherits(m, "glm")) {
      ci <- tryCatch(
        suppressMessages(stats::confint.default(m, level = 0.95)),
        error = function(e) cbind(est - 1.959964 * se, est + 1.959964 * se)
      )
    } else {
      tcrit <- if (!is.na(df_res) && df_res > 0) stats::qt(0.975, df = df_res) else 1.959964
      ci <- cbind(est - tcrit * se, est + tcrit * se)
      rownames(ci) <- rownames(sm)
    }
    ci_l <- ci[rownames(sm), 1L]
    ci_u <- ci[rownames(sm), 2L]

    terms_tbl <- tibble::tibble(
      term = rownames(sm),
      estimate = as.numeric(est),
      or = exp(as.numeric(est)),
      se = as.numeric(se),
      statistic = as.numeric(stat_v),
      df = rep(df_res, nrow(sm)),
      p_value = as.numeric(pval),
      ci_lower = as.numeric(ci_l),
      ci_upper = as.numeric(ci_u)
    )
    fit_stats$n <- tryCatch(as.numeric(stats::nobs(m)), error = function(e) NA_real_)
    fit_stats$r_squared <- sm_obj$r.squared %||% NA_real_
    fit_stats$adj_r_squared <- sm_obj$adj.r.squared %||% NA_real_
    fit_stats$aic <- tryCatch(as.numeric(stats::AIC(m)), error = function(e) NA_real_)
    fit_stats$bic <- tryCatch(as.numeric(stats::BIC(m)), error = function(e) NA_real_)
    fit_stats$df_residual <- df_res
    return(list(terms = terms_tbl, fit = fit_stats, class = class(m)[1L]))
  }

  # 3. htest (t.test, cor.test, wilcox.test, chisq.test)
  if (inherits(m, "htest")) {
    est <- as.numeric(m$estimate)
    if (length(est) == 2L && grepl("Two Sample|Welch", m$method %||% "")) {
      est <- est[1L] - est[2L]
    } else if (length(est) >= 1L) {
      est <- est[1L]
    } else {
      est <- NA_real_
    }
    ci <- m$conf.int
    ci_l <- if (!is.null(ci) && length(ci) >= 2L) as.numeric(ci[1L]) else NA_real_
    ci_u <- if (!is.null(ci) && length(ci) >= 2L) as.numeric(ci[2L]) else NA_real_
    se <- if (!is.null(m$stderr)) as.numeric(m$stderr)[1L] else NA_real_
    stat_v <- if (!is.null(m$statistic)) as.numeric(m$statistic)[1L] else NA_real_
    df_v <- if (!is.null(m$parameter)) as.numeric(m$parameter)[1L] else NA_real_
    pval <- if (!is.null(m$p.value)) as.numeric(m$p.value)[1L] else NA_real_

    if (grepl("correlation", m$method %||% "", ignore.case = TRUE)) {
      fit_stats$r <- est
      if (!is.na(df_v)) fit_stats$n <- df_v + 2
    }
    terms_tbl <- tibble::tibble(
      term = "test",
      estimate = est,
      or = NA_real_,
      se = se,
      statistic = stat_v,
      df = df_v,
      p_value = pval,
      ci_lower = ci_l,
      ci_upper = ci_u
    )
    return(list(terms = terms_tbl, fit = fit_stats, class = "htest"))
  }

  # 4. Tidy data.frame / tibble
  if (is.data.frame(m)) {
    lc <- tolower(names(m))
    pick <- function(aliases) {
      idx <- which(lc %in% aliases)
      if (length(idx) > 0L) m[[idx[1L]]] else NULL
    }
    term_v <- pick(c("term", "parameter", "variable", "predictor", "name"))
    if (is.null(term_v)) term_v <- paste0("row_", seq_len(nrow(m)))
    est_v <- pick(c("estimate", "coef", "b", "beta", "est")) %||% rep(NA_real_, nrow(m))
    se_v <- pick(c("std.error", "se", "std_error", "stderr")) %||% rep(NA_real_, nrow(m))
    stat_v <- pick(c("statistic", "t", "z", "t.value", "z.value")) %||% rep(NA_real_, nrow(m))
    df_v <- pick(c("df", "parameter_df")) %||% rep(NA_real_, nrow(m))
    p_v <- pick(c("p.value", "p_value", "pval", "p")) %||% rep(NA_real_, nrow(m))
    cil_v <- pick(c("conf.low", "ci_lower", "ci_low", "lower")) %||% rep(NA_real_, nrow(m))
    ciu_v <- pick(c("conf.high", "ci_upper", "ci_high", "upper")) %||% rep(NA_real_, nrow(m))
    or_v <- pick(c("or", "odds_ratio")) %||% exp(as.numeric(est_v))
    n_v <- pick(c("n", "nobs"))
    if (!is.null(n_v) && length(n_v) > 0L) fit_stats$n <- as.numeric(n_v[1L])
    r2_v <- pick(c("r_squared", "r.squared", "r2"))
    if (!is.null(r2_v) && length(r2_v) > 0L) fit_stats$r_squared <- as.numeric(r2_v[1L])

    terms_tbl <- tibble::tibble(
      term = as.character(term_v),
      estimate = as.numeric(est_v),
      or = as.numeric(or_v),
      se = as.numeric(se_v),
      statistic = as.numeric(stat_v),
      df = as.numeric(df_v),
      p_value = as.numeric(p_v),
      ci_lower = as.numeric(cil_v),
      ci_upper = as.numeric(ciu_v)
    )
    return(list(terms = terms_tbl, fit = fit_stats, class = "data.frame"))
  }

  rlang::abort(paste0("Unsupported model class for manuscript auditing: `", paste(class(m), collapse = "/"), "`."))
}

# ---------------------------------------------------------------------------
# Prose stripping & claim extraction
# ---------------------------------------------------------------------------

strip_non_prose_lines <- function(lines) {
  n <- length(lines)
  keep <- rep(TRUE, n)

  # Strip YAML frontmatter at top of document
  if (n >= 2L && grepl("^---[[:space:]]*$", lines[1L])) {
    end_yaml <- which(grepl("^(---|\\.\\.\\.)[[:space:]]*$", lines[-1L]))
    if (length(end_yaml) > 0L) {
      keep[seq_len(end_yaml[1L] + 1L)] <- FALSE
    }
  }

  # Strip fenced code blocks (``` or ~~~)
  in_fence <- FALSE
  fence_char <- ""
  fence_len <- 0L
  for (i in seq_len(n)) {
    if (!keep[i]) next
    ln <- lines[i]
    if (!in_fence) {
      m <- regexec("^[[:space:]]*(`{3,}|~{3,})", ln)
      cap <- regmatches(ln, m)[[1L]]
      if (length(cap) >= 2L) {
        in_fence <- TRUE
        fence_char <- substr(cap[2L], 1L, 1L)
        fence_len <- nchar(cap[2L])
        keep[i] <- FALSE
      }
    } else {
      keep[i] <- FALSE
      close_pat <- paste0("^[[:space:]]*", fence_char, "{", fence_len, ",}[[:space:]]*$")
      if (grepl(close_pat, ln)) {
        in_fence <- FALSE
      }
    }
  }

  out <- lines
  out[!keep] <- ""
  out
}

normalize_prose_line <- function(s) {
  # Convert Unicode minus and en-dash before digits to ASCII hyphen
  s <- gsub("\u2212", "-", s, fixed = TRUE)
  s <- gsub("\u2013(?=[[:space:]]*[0-9\\.])", "-", s, perl = TRUE)
  # Strip markdown italics/bold/inline math around metric symbols
  s <- gsub("\\*{1,2}([A-Za-z]+)\\*{1,2}", "\\1", s, perl = TRUE)
  s <- gsub("_([A-Za-z]+)_", "\\1", s, perl = TRUE)
  s <- gsub("\\$([^$]+)\\$", "\\1", s, perl = TRUE)
  s <- gsub("\\\\beta\\b", "beta", s, perl = TRUE)
  s <- gsub("\u03b2", "beta", s, fixed = TRUE)
  s <- gsub("R\u00b2", "R2", s, fixed = TRUE)
  s
}

parse_tag_attrs <- function(attr_str) {
  out <- list(model = NA_character_, term = NA_character_)
  if (is.null(attr_str) || !nzchar(attr_str)) return(out)
  m_mod <- regexec("model[[:space:]]*=[[:space:]]*['\"]?([A-Za-z0-9_.:-]+)['\"]?", attr_str)
  c_mod <- regmatches(attr_str, m_mod)[[1L]]
  if (length(c_mod) >= 2L) out$model <- c_mod[2L]
  m_trm <- regexec("term[[:space:]]*=[[:space:]]*['\"]?([A-Za-z0-9_.:()\\s-]+?)['\"]?(?:[[:space:]]+(?:model|metric)=|$)", attr_str, perl = TRUE)
  c_trm <- regmatches(attr_str, m_trm)[[1L]]
  if (length(c_trm) >= 2L) {
    # Handle quoted or unquoted term cleanly
    m_trm_q <- regexec("term[[:space:]]*=[[:space:]]*['\"]([^'\"]+)['\"]", attr_str)
    cq <- regmatches(attr_str, m_trm_q)[[1L]]
    if (length(cq) >= 2L) {
      out$term <- cq[2L]
    } else {
      m_trm_u <- regexec("term[[:space:]]*=[[:space:]]*([^[:space:]]+)", attr_str)
      cu <- regmatches(attr_str, m_trm_u)[[1L]]
      if (length(cu) >= 2L) out$term <- cu[2L]
    }
  }
  out
}

extract_claims_from_segment <- function(seg, line_num, tag_model = NA_character_, tag_term = NA_character_) {
  norm <- normalize_prose_line(seg)
  claims <- list()

  add_claim <- function(metric, op, raw_val) {
    clean_num <- gsub(",", "", raw_val, fixed = TRUE)
    val <- suppressWarnings(as.numeric(clean_num))
    if (is.na(val)) return()
    claims[[length(claims) + 1L]] <<- list(
      line = as.integer(line_num),
      model = tag_model,
      term = tag_term,
      metric = metric,
      op = op,
      reported_raw = raw_val,
      reported = val,
      decimals = count_decimals(clean_num),
      context = trimws(seg)
    )
  }

  # 1. Confidence intervals: 95% CI [0.20, 0.64] or CI (-0.50, -0.10) or CI: 0.20 to 0.64
  ci_pat <- "(?:(?:[0-9]{2}%[[:space:]]*)?CI[[:space:]]*(?:=|:)?[[:space:]]*(?:[\\[\\(][[:space:]]*(-?(?:[0-9]+\\.[0-9]+|\\.[0-9]+|[0-9]+))[[:space:]]*(?:,|to|[[:space:]]-[[:space:]])[[:space:]]*(-?(?:[0-9]+\\.[0-9]+|\\.[0-9]+|[0-9]+))[[:space:]]*[\\]\\)]|(-?(?:[0-9]+\\.[0-9]+|\\.[0-9]+|[0-9]+))[[:space:]]+to[[:space:]]+(-?(?:[0-9]+\\.[0-9]+|\\.[0-9]+|[0-9]+))))"
  ci_hits <- gregexpr(ci_pat, norm, perl = TRUE)[[1L]]
  if (length(ci_hits) > 0L && ci_hits[1L] != -1L) {
    matches <- regmatches(norm, gregexpr(ci_pat, norm, perl = TRUE))[[1L]]
    for (mt in matches) {
      # Extract two numeric values
      nums <- regmatches(mt, gregexpr("-?(?:[0-9]+\\.[0-9]+|\\.[0-9]+|[0-9]+)", mt, perl = TRUE))[[1L]]
      # Drop leading "95" from "95% CI" if present
      if (grepl("^[0-9]{2}%", trimws(mt)) && length(nums) >= 3L) {
        nums <- nums[-1L]
      }
      if (length(nums) >= 2L) {
        add_claim("ci_lower", "=", nums[1L])
        add_claim("ci_upper", "=", nums[2L])
      }
    }
  }

  # 2. Point estimate: b = ..., beta = ..., B = ..., est = ..., estimate = ..., coef = ...
  est_pat <- "\\b(?:beta|estimate|est|coef|[bB])[[:space:]]*=[[:space:]]*(-?(?:[0-9]+\\.[0-9]+|\\.[0-9]+|[0-9]+))"
  m_est <- gregexpr(est_pat, norm, perl = TRUE)
  if (m_est[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_est)[[1L]]) {
      val <- sub("^[^=]*=[[:space:]]*", "", mt)
      add_claim("estimate", "=", val)
    }
  }

  # 3. Odds ratio: OR = ...
  or_pat <- "\\bOR[[:space:]]*=[[:space:]]*([0-9]+\\.[0-9]+|\\.[0-9]+|[0-9]+)"
  m_or <- gregexpr(or_pat, norm, perl = TRUE)
  if (m_or[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_or)[[1L]]) {
      val <- sub("^[^=]*=[[:space:]]*", "", mt)
      add_claim("or", "=", val)
    }
  }

  # 4. Standard error: SE = ..., S.E. = ...
  se_pat <- "\\bS\\.?E\\.?[[:space:]]*=[[:space:]]*([0-9]+\\.[0-9]+|\\.[0-9]+|[0-9]+)"
  m_se <- gregexpr(se_pat, norm, perl = TRUE)
  if (m_se[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_se)[[1L]]) {
      val <- sub("^[^=]*=[[:space:]]*", "", mt)
      add_claim("se", "=", val)
    }
  }

  # 5. p-values: p = .003, p < .001, p <= 0.05, p > .05
  p_pat <- "\\bp[[:space:]]*(<=|>=|=|<|>)[[:space:]]*(0?\\.[0-9]+|[0-9]+\\.[0-9]+|1|0)\\b"
  m_p <- gregexpr(p_pat, norm, perl = TRUE)
  if (m_p[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_p)[[1L]]) {
      cap <- regmatches(mt, regexec("\\bp[[:space:]]*(<=|>=|=|<|>)[[:space:]]*(0?\\.[0-9]+|[0-9]+\\.[0-9]+|1|0)", mt, perl = TRUE))[[1L]]
      if (length(cap) >= 3L) {
        add_claim("p_value", cap[2L], cap[3L])
      }
    }
  }

  # 6. Test statistic: t(df) = ..., t = ..., z = ...
  stat_pat <- "\\b([tz])[[:space:]]*(?:\\([[:space:]]*([0-9]+(?:\\.[0-9]+)?)[[:space:]]*\\))?[[:space:]]*=[[:space:]]*(-?(?:[0-9]+\\.[0-9]+|\\.[0-9]+|[0-9]+))"
  m_st <- gregexpr(stat_pat, norm, perl = TRUE)
  if (m_st[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_st)[[1L]]) {
      cap <- regmatches(mt, regexec(stat_pat, mt, perl = TRUE))[[1L]]
      if (length(cap) >= 4L) {
        add_claim("statistic", "=", cap[4L])
      }
    }
  }

  # 7. Correlation: r = .34, r(98) = -0.41
  r_pat <- "\\br[[:space:]]*(?:\\([[:space:]]*[0-9]+(?:\\.[0-9]+)?[[:space:]]*\\))?[[:space:]]*=[[:space:]]*(-?(?:0?\\.[0-9]+|1(?:\\.0+)?|0))"
  m_r <- gregexpr(r_pat, norm, perl = TRUE)
  if (m_r[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_r)[[1L]]) {
      val <- sub("^[^=]*=[[:space:]]*", "", mt)
      add_claim("r", "=", val)
    }
  }

  # 8. Sample size: N = 250, n = 1,250
  n_pat <- "\\b[Nn][[:space:]]*=[[:space:]]*([0-9][0-9,]*)\\b"
  m_n <- gregexpr(n_pat, norm, perl = TRUE)
  if (m_n[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_n)[[1L]]) {
      val <- sub("^[^=]*=[[:space:]]*", "", mt)
      add_claim("n", "=", val)
    }
  }

  # 9. R-squared: R^2 = 0.45, R2 = .45
  r2_pat <- "\\bR\\^?\\{?2\\}?[[:space:]]*=[[:space:]]*(0?\\.[0-9]+|[0-9]+\\.[0-9]+)"
  m_r2 <- gregexpr(r2_pat, norm, perl = TRUE)
  if (m_r2[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_r2)[[1L]]) {
      val <- sub("^[^=]*=[[:space:]]*", "", mt)
      add_claim("r_squared", "=", val)
    }
  }

  # 10. AIC / BIC
  aic_pat <- "\\bAIC[[:space:]]*=[[:space:]]*(-?(?:[0-9]+\\.[0-9]+|[0-9]+))"
  m_aic <- gregexpr(aic_pat, norm, perl = TRUE)
  if (m_aic[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_aic)[[1L]]) {
      add_claim("aic", "=", sub("^[^=]*=[[:space:]]*", "", mt))
    }
  }
  bic_pat <- "\\bBIC[[:space:]]*=[[:space:]]*(-?(?:[0-9]+\\.[0-9]+|[0-9]+))"
  m_bic <- gregexpr(bic_pat, norm, perl = TRUE)
  if (m_bic[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_bic)[[1L]]) {
      add_claim("bic", "=", sub("^[^=]*=[[:space:]]*", "", mt))
    }
  }

  claims
}

count_decimals <- function(num_str) {
  s <- trimws(num_str)
  if (!grepl("\\.", s)) return(0L)
  frac <- sub("^[^.]*\\.", "", s)
  nchar(frac)
}

# ---------------------------------------------------------------------------
# Core manuscript auditing logic
# ---------------------------------------------------------------------------

audit_manuscript_lines <- function(lines, source_label, models, tol_extra, warn_rounding, default_model) {
  raw_models <- load_models_input(models)
  catalog <- lapply(raw_models, extract_single_model)
  model_names <- names(catalog)
  if (!is.null(default_model) && !(default_model %in% model_names)) {
    rlang::abort(paste0("`default_model` (`", default_model, "`) not found in `models`."))
  }

  prose <- strip_non_prose_lines(lines)

  # Parse lines into claim groups (respecting inline spans and HTML comments)
  claim_groups <- list()
  pending_tag <- NULL

  span_pat <- "\\[([^\\]]+)\\]\\{\\.statci[[:space:]]+([^}]+)\\}"
  comment_pat <- "<!--+[[:space:]]*statci:[[:space:]]*(.*?)--+>"

  for (i in seq_along(prose)) {
    ln <- prose[i]
    if (!nzchar(trimws(ln))) next

    # 1. Check HTML comment tag: <!-- statci: model=... term=... -->
    cm <- regexec(comment_pat, ln, perl = TRUE)
    ccap <- regmatches(ln, cm)[[1L]]
    line_tag <- pending_tag
    if (length(ccap) >= 2L) {
      attrs <- parse_tag_attrs(ccap[2L])
      remainder <- trimws(sub(comment_pat, "", ln, perl = TRUE))
      if (!nzchar(remainder)) {
        # Tag applies to the next non-empty prose line
        pending_tag <- attrs
        next
      } else {
        line_tag <- attrs
        pending_tag <- NULL
        ln <- remainder
      }
    } else {
      pending_tag <- NULL
    }

    # 2. Extract any Quarto inline spans `[...]{.statci ...}` on this line
    span_hits <- gregexpr(span_pat, ln, perl = TRUE)
    if (span_hits[[1L]][1L] != -1L) {
      spans <- regmatches(ln, span_hits)[[1L]]
      for (sp in spans) {
        sc <- regmatches(sp, regexec(span_pat, sp, perl = TRUE))[[1L]]
        if (length(sc) >= 3L) {
          attrs <- parse_tag_attrs(sc[3L])
          cl <- extract_claims_from_segment(sc[2L], i, attrs$model, attrs$term)
          if (length(cl) > 0L) {
            claim_groups[[length(claim_groups) + 1L]] <- list(
              line = i, text = sc[2L], tag_model = attrs$model, tag_term = attrs$term, claims = cl
            )
          }
        }
      }
      # Remove matched spans before checking remaining text on the line
      ln <- gsub(span_pat, "", ln, perl = TRUE)
    }

    # 3. Extract remaining claims on the line
    if (nzchar(trimws(ln))) {
      tm <- if (!is.null(line_tag)) line_tag$model else NA_character_
      tt <- if (!is.null(line_tag)) line_tag$term else NA_character_
      cl <- extract_claims_from_segment(ln, i, tm, tt)
      if (length(cl) > 0L) {
        claim_groups[[length(claim_groups) + 1L]] <- list(
          line = i, text = ln, tag_model = tm, tag_term = tt, claims = cl
        )
      }
    }
  }

  # Evaluate every claim against the model catalog
  check_rows <- list()

  for (grp in claim_groups) {
    # Resolve target model for the group
    m_name <- grp$tag_model
    if (is.na(m_name) || !nzchar(m_name)) {
      if (!is.null(default_model)) {
        m_name <- default_model
      } else if (length(model_names) == 1L) {
        m_name <- model_names[1L]
      } else {
        # Try matching model name mentioned in text
        mentioned <- model_names[vapply(model_names, function(nm) {
          grepl(paste0("\\b", nm, "\\b"), grp$text)
        }, logical(1L))]
        m_name <- if (length(mentioned) == 1L) mentioned[1L] else model_names[1L]
      }
    }

    if (!(m_name %in% model_names)) {
      for (cl in grp$claims) {
        check_rows[[length(check_rows) + 1L]] <- tibble::tibble(
          line = cl$line, model = m_name, term = cl$term %||% NA_character_,
          metric = cl$metric, op = cl$op, reported_raw = cl$reported_raw,
          reported = cl$reported, actual = NA_real_, diff = NA_real_,
          tolerance = NA_real_, issue_type = "missing_model",
          status = "fail", context = cl$context
        )
      }
      next
    }

    mod <- catalog[[m_name]]
    fit_metrics <- c("n", "r_squared", "aic", "bic")

    # Separate term-level claims to infer term cohesively when untagged
    term_claims <- Filter(function(c) !(c$metric %in% fit_metrics) && !(c$metric == "r" && !is.na(mod$fit$r)), grp$claims)
    resolved_term <- grp$tag_term
    if ((is.na(resolved_term) || !nzchar(resolved_term)) && length(term_claims) > 0L) {
      resolved_term <- infer_group_term(grp$text, term_claims, mod$terms)
    }

    for (cl in grp$claims) {
      if (cl$metric %in% fit_metrics) {
        actual <- mod$fit[[cl$metric]]
        res <- compare_claim_value(cl, actual, tol_extra, warn_rounding)
        check_rows[[length(check_rows) + 1L]] <- tibble::tibble(
          line = cl$line, model = m_name, term = NA_character_,
          metric = cl$metric, op = cl$op, reported_raw = cl$reported_raw,
          reported = cl$reported, actual = actual, diff = res$diff,
          tolerance = res$tol, issue_type = res$issue_type,
          status = res$status, context = cl$context
        )
      } else if (cl$metric == "r" && !is.na(mod$fit$r)) {
        actual <- mod$fit$r
        res <- compare_claim_value(cl, actual, tol_extra, warn_rounding)
        check_rows[[length(check_rows) + 1L]] <- tibble::tibble(
          line = cl$line, model = m_name, term = "correlation",
          metric = "r", op = cl$op, reported_raw = cl$reported_raw,
          reported = cl$reported, actual = actual, diff = res$diff,
          tolerance = res$tol, issue_type = res$issue_type,
          status = res$status, context = cl$context
        )
      } else {
        trm <- if (!is.na(cl$term) && nzchar(cl$term)) cl$term else resolved_term
        idx <- which(mod$terms$term == trm)
        if (length(idx) == 0L) {
          check_rows[[length(check_rows) + 1L]] <- tibble::tibble(
            line = cl$line, model = m_name, term = trm %||% NA_character_,
            metric = cl$metric, op = cl$op, reported_raw = cl$reported_raw,
            reported = cl$reported, actual = NA_real_, diff = NA_real_,
            tolerance = NA_real_, issue_type = "missing_term",
            status = "fail", context = cl$context
          )
        } else {
          col_nm <- if (cl$metric == "r") "estimate" else cl$metric
          actual <- if (col_nm %in% names(mod$terms)) mod$terms[[col_nm]][idx[1L]] else NA_real_
          res <- compare_claim_value(cl, actual, tol_extra, warn_rounding)
          check_rows[[length(check_rows) + 1L]] <- tibble::tibble(
            line = cl$line, model = m_name, term = trm,
            metric = cl$metric, op = cl$op, reported_raw = cl$reported_raw,
            reported = cl$reported, actual = actual, diff = res$diff,
            tolerance = res$tol, issue_type = res$issue_type,
            status = res$status, context = cl$context
          )
        }
      }
    }
  }

  checks <- if (length(check_rows) > 0L) {
    tibble::as_tibble(do.call(rbind, check_rows))
  } else {
    tibble::tibble(
      line = integer(0), model = character(0), term = character(0),
      metric = character(0), op = character(0), reported_raw = character(0),
      reported = numeric(0), actual = numeric(0), diff = numeric(0),
      tolerance = numeric(0), issue_type = character(0),
      status = character(0), context = character(0)
    )
  }

  n_pass <- sum(checks$status == "pass")
  n_warn <- sum(checks$status == "warn")
  n_fail <- sum(checks$status == "fail")
  overall <- combine_status(checks$status)

  structure(
    list(
      checks = checks,
      manuscript = source_label,
      models_checked = model_names,
      summary = list(
        n_claims = nrow(checks),
        n_pass = as.integer(n_pass),
        n_warn = as.integer(n_warn),
        n_fail = as.integer(n_fail),
        n_sign_flips = as.integer(sum(checks$issue_type == "sign_flip")),
        n_rounding = as.integer(sum(checks$issue_type == "rounding_discrepancy"))
      ),
      status = overall
    ),
    class = "statci_manuscript_audit"
  )
}

infer_group_term <- function(text, term_claims, terms_tbl) {
  if (nrow(terms_tbl) == 0L) return(NA_character_)
  non_int <- terms_tbl[terms_tbl$term != "(Intercept)", , drop = FALSE]
  cand <- if (nrow(non_int) > 0L) non_int else terms_tbl
  if (nrow(cand) == 1L) return(cand$term[1L])

  # 1. Exact term name mentioned in prose
  hits <- cand$term[vapply(cand$term, function(tm) {
    grepl(paste0("(?<![A-Za-z0-9_])", gsub("([.^$|()*+?{}\\[\\]\\\\])", "\\\\\\1", tm), "(?![A-Za-z0-9_])"), text, perl = TRUE)
  }, logical(1L))]
  if (length(hits) == 1L) return(hits[1L])

  # 2. Score candidate terms cohesively (using abs() for estimate so sign flips bind to true term!)
  scores <- vapply(seq_len(nrow(cand)), function(r) {
    sc <- 0
    for (cl in term_claims) {
      col_nm <- if (cl$metric == "r") "estimate" else cl$metric
      if (!(col_nm %in% names(cand))) next
      act <- cand[[col_nm]][r]
      if (is.na(act)) next
      if (cl$metric %in% c("estimate", "statistic")) {
        sc <- sc + min(abs(act - cl$reported), abs(abs(act) - abs(cl$reported)) + 1e-4)
      } else if (cl$metric == "p_value" && cl$op != "=") {
        ok <- eval_ineq(act, cl$op, cl$reported)
        sc <- sc + if (ok) 0 else 1
      } else {
        sc <- sc + abs(act - cl$reported)
      }
    }
    sc
  }, numeric(1L))
  cand$term[which.min(scores)]
}

eval_ineq <- function(actual, op, reported) {
  switch(
    op,
    "<" = actual < reported,
    "<=" = actual <= reported,
    ">" = actual > reported,
    ">=" = actual >= reported,
    FALSE
  )
}

compare_claim_value <- function(cl, actual, tol_extra, warn_rounding) {
  if (is.na(actual) || !is.finite(actual)) {
    return(list(diff = NA_real_, tol = NA_real_, issue_type = "unavailable_metric", status = "warn"))
  }

  # Exact sample size comparison
  if (cl$metric == "n") {
    d <- cl$reported - actual
    if (abs(d) <= 1e-9) {
      return(list(diff = 0, tol = 0, issue_type = "match", status = "pass"))
    }
    return(list(diff = d, tol = 0, issue_type = "stale_sample_size", status = "fail"))
  }

  # Inequality p-value checks (p < .001, p > .05, etc.)
  if (cl$metric == "p_value" && cl$op != "=") {
    ok <- eval_ineq(actual, cl$op, cl$reported)
    return(list(
      diff = actual - cl$reported,
      tol = 0,
      issue_type = if (ok) "match" else "inequality_violation",
      status = if (ok) "pass" else "fail"
    ))
  }

  # Standard numeric comparison with rounding half-unit
  dec <- cl$decimals
  half_unit <- 0.5 * 10^(-dec) + tol_extra
  one_half_unit <- 1.5 * 10^(-dec) + tol_extra
  diff_val <- cl$reported - actual
  abs_diff <- abs(diff_val)

  # Explicit sign-flip check (for signed quantities)
  if (cl$metric %in% c("estimate", "statistic", "r", "ci_lower", "ci_upper")) {
    if (cl$reported != 0 && sign(cl$reported) != sign(actual) && abs(actual) > half_unit + 1e-9) {
      return(list(diff = diff_val, tol = half_unit, issue_type = "sign_flip", status = "fail"))
    }
  }

  if (abs_diff <= half_unit + 1e-9) {
    return(list(diff = diff_val, tol = half_unit, issue_type = "match", status = "pass"))
  }

  if (isTRUE(warn_rounding) && dec >= 1L && abs_diff <= one_half_unit + 1e-9) {
    return(list(diff = diff_val, tol = half_unit, issue_type = "rounding_discrepancy", status = "warn"))
  }

  list(diff = diff_val, tol = half_unit, issue_type = "value_mismatch", status = "fail")
}

#' @export
as_tibble.statci_manuscript_audit <- function(x, ...) {
  x$checks
}

#' @export
print.statci_manuscript_audit <- function(x, max_rows = 20L, ...) {
  badge <- format_status_badge(x$status)
  s <- x$summary
  cli::cli_text(
    "{badge} {.strong statci Manuscript Audit} ({basename(x$manuscript)}): ",
    "{s$n_claims} claim(s) | {s$n_pass} pass, {s$n_warn} warn, {s$n_fail} fail"
  )
  issues <- x$checks[x$checks$status != "pass", , drop = FALSE]
  if (nrow(issues) > 0L) {
    show_n <- min(nrow(issues), as.integer(max_rows))
    for (i in seq_len(show_n)) {
      r <- issues[i, ]
      b <- format_status_badge(r$status)
      trm <- if (is.na(r$term)) "" else paste0("[", r$term, "] ")
      cli::cli_text(
        "  {b} line {r$line} ({r$model} {trm}{r$metric}): ",
        "reported `{r$op} {r$reported_raw}` vs actual `{fmt_num(r$actual, 4L)}` ({r$issue_type})"
      )
    }
  }
  invisible(x)
}
