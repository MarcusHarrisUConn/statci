# Manuscript-to-Code Consistency Auditor for Quarto (.qmd), RMarkdown (.Rmd), and LaTeX (.tex)

#' Audit a Quarto, RMarkdown, or LaTeX manuscript against saved R model objects
#'
#' Parses inline prose and Markdown/LaTeX tables in `.qmd`, `.Rmd`, `.md`, or
#' `.tex` manuscripts—covering point estimates, standard errors, p-values,
#' confidence intervals, test statistics (`t`, `z`, `F`, `chi2`, `r`), odds
#' ratios (`OR`), sample sizes (`N`), \eqn{R^2}, and AIC/BIC—and cross-checks
#' them against fitted R model objects (`lm`, `glm`, `lmerMod`, `htest`, or
#' tidy summary data frames). Also performs **internal arithmetic
#' cross-checking** (verifying that reported `t`/`z` matches `b / SE` and that
#' reported `95% CI` bounds are consistent with `b` and `SE` up to rounding
#' intervals), which works even for untagged claims or when `models = NULL`.
#'
#' Supports explicit annotations for zero-ambiguity binding:
#' - HTML comment anchors: `<!-- statci: model=fit1 term=x1 -->`
#' - LaTeX comment anchors: `% statci: model=fit1 term=x1`
#' - Quarto inline spans: `[b = 0.42, SE = 0.11, p = .003]{.statci model="fit1" term="x1"}`
#'
#' @param manuscript Path to a `.qmd`, `.Rmd`, `.md`, or `.tex` file.
#' @param models Optional saved models to cross-check against. One of:
#'   - A named list of fitted model objects (`lm`, `glm`, `lmerMod`, `htest`)
#'     or tidy summary data frames;
#'   - A single fitted model object or tidy summary data frame;
#'   - Path to an `.rds` file containing a model or named list of models;
#'   - Path to a directory containing `.rds` model files (keyed by filename stem);
#'   - `NULL` to run internal arithmetic consistency checks only.
#' @param tol_extra Non-negative numeric extra tolerance added on top of the
#'   automatic rounding half-unit (`0.5 * 10^(-decimals)`). Default `0`.
#' @param warn_rounding Logical; if `TRUE` (default), values off by at most one
#'   unit in the last reported decimal place are flagged as `"warn"`
#'   (`rounding_discrepancy`) rather than `"fail"`.
#' @param default_model Optional default model name used for untagged claims
#'   when multiple models are provided.
#' @param check_internal Logical; if `TRUE` (default), run interval-arithmetic
#'   internal consistency checks (`t` vs `b / SE`, `95% CI` midpoint & width
#'   vs `b` and `SE`) on every claim group and table row.
#'
#' @return An S3 object of class `"statci_manuscript_audit"` containing `checks`
#'   (model comparison [tibble::tibble]), `internal_checks` (internal arithmetic
#'   [tibble::tibble]), `summary`, `manuscript`, `models_checked`, and overall
#'   `status` (`"pass"`, `"warn"`, or `"fail"`).
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
                             models = NULL,
                             tol_extra = 0,
                             warn_rounding = TRUE,
                             default_model = NULL,
                             check_internal = TRUE) {
  validate_existing_file(manuscript, arg = "manuscript")
  lines <- readLines(manuscript, warn = FALSE, encoding = "UTF-8")
  ext <- tolower(tools::file_ext(manuscript))
  fmt <- if (identical(ext, "tex")) "tex" else "markdown"
  audit_manuscript_lines(
    lines = lines,
    source_label = manuscript,
    models = models,
    tol_extra = tol_extra,
    warn_rounding = warn_rounding,
    default_model = default_model,
    format = fmt,
    check_internal = check_internal
  )
}

#' Audit manuscript prose or tables supplied as an in-memory string
#'
#' In-memory counterpart to [audit_manuscript()], supporting Markdown/Quarto
#' and LaTeX (`.tex`) prose, tables, and internal arithmetic verification.
#'
#' @param text Character scalar or vector of manuscript lines.
#' @param format Document format: `"auto"` (detects LaTeX commands
#'   automatically), `"markdown"` (Quarto/RMarkdown), or `"tex"` (LaTeX).
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
                                  models = NULL,
                                  tol_extra = 0,
                                  warn_rounding = TRUE,
                                  default_model = NULL,
                                  format = c("auto", "markdown", "tex"),
                                  check_internal = TRUE) {
  format <- match.arg(format)
  if (!is.character(text)) {
    rlang::abort("`text` must be a character vector.")
  }
  lines <- unlist(strsplit(paste(text, collapse = "\n"), "\n", fixed = TRUE))
  if (length(lines) == 0L) lines <- ""
  if (format == "auto") {
    joined <- paste(lines, collapse = "\n")
    format <- if (grepl("\\\\begin\\{(document|tabular|table)\\}|\\\\section\\{|%\\s*statci:", joined, perl = TRUE)) {
      "tex"
    } else {
      "markdown"
    }
  }
  audit_manuscript_lines(
    lines = lines,
    source_label = "<text>",
    models = models,
    tol_extra = tol_extra,
    warn_rounding = warn_rounding,
    default_model = default_model,
    format = format,
    check_internal = check_internal
  )
}

# ---------------------------------------------------------------------------
# Model catalog loader & uniform extractor
# ---------------------------------------------------------------------------

load_models_input <- function(models) {
  if (is.null(models)) {
    return(list())
  }
  if (is.character(models) && length(models) == 1L) {
    assert_clean_scalar_string(models, "models")
    if (dir.exists(models)) {
      validate_existing_dir(models, arg = "models")
      rds_files <- sort(list.files(models, pattern = "\\.rds$", ignore.case = TRUE, full.names = TRUE))
      if (length(rds_files) == 0L) {
        rlang::abort(paste0("No `.rds` files found in directory `", models, "`."))
      }
      escaped <- rds_files[!vapply(rds_files, is_safe_contained_file, logical(1L), root_dir = models)]
      if (length(escaped) > 0L) {
        rlang::abort(paste0(
          "Refusing to load `.rds` file(s) outside model directory via symlink traversal: ",
          paste(basename(escaped), collapse = ", ")
        ))
      }
      objs <- lapply(rds_files, readRDS)
      names(objs) <- tools::file_path_sans_ext(basename(rds_files))
      return(objs)
    }
    validate_existing_file(models, arg = "models")
    obj <- readRDS(models)
    if (is.list(obj) && !inherits(obj, c("lm", "glm", "htest", "data.frame", "lmerMod", "glmerMod"))) {
      if (length(obj) == 0L) {
        return(list())
      }
      if (is.null(names(obj))) {
        names(obj) <- paste0("model_", seq_along(obj))
      }
      return(obj)
    }
    nm <- tools::file_path_sans_ext(basename(models))
    return(stats::setNames(list(obj), nm))
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

  rlang::abort("`models` must be NULL, a model object, a named list of models, an `.rds` file, or a directory of `.rds` files.")
}

extract_single_model <- function(m) {
  fit_stats <- list(
    n = NA_real_, r_squared = NA_real_, adj_r_squared = NA_real_,
    aic = NA_real_, bic = NA_real_, df_residual = NA_real_, r = NA_real_,
    f_stat = NA_real_, chisq = NA_real_
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
    if (!is.null(sm_obj$fstatistic) && length(sm_obj$fstatistic) >= 1L) {
      fit_stats$f_stat <- as.numeric(sm_obj$fstatistic[1L])
    }
    fit_stats$aic <- tryCatch(as.numeric(stats::AIC(m)), error = function(e) NA_real_)
    fit_stats$bic <- tryCatch(as.numeric(stats::BIC(m)), error = function(e) NA_real_)
    fit_stats$df_residual <- df_res
    return(list(terms = terms_tbl, fit = fit_stats, class = class(m)[1L]))
  }

  # 3. htest (t.test, cor.test, wilcox.test, chisq.test, var.test)
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

    meth <- m$method %||% ""
    if (grepl("correlation", meth, ignore.case = TRUE)) {
      fit_stats$r <- est
      if (!is.na(df_v)) fit_stats$n <- df_v + 2
    }
    if (grepl("Chi-squared", meth, ignore.case = TRUE) ||
        identical(names(m$statistic), "X-squared")) {
      fit_stats$chisq <- stat_v
      if (!is.null(m$observed)) fit_stats$n <- sum(m$observed)
    }
    if (grepl("F test", meth, ignore.case = TRUE) ||
        identical(names(m$statistic), "F")) {
      fit_stats$f_stat <- stat_v
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
    f_v <- pick(c("f_stat", "f.value", "f"))
    if (!is.null(f_v) && length(f_v) > 0L) fit_stats$f_stat <- as.numeric(f_v[1L])
    chi_v <- pick(c("chisq", "chi2", "x2"))
    if (!is.null(chi_v) && length(chi_v) > 0L) fit_stats$chisq <- as.numeric(chi_v[1L])

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
# Prose stripping (Markdown & LaTeX) + Table Parsers
# ---------------------------------------------------------------------------

strip_non_prose_lines <- function(lines, format = "markdown") {
  n <- length(lines)
  keep <- rep(TRUE, n)

  if (format == "tex") {
    in_verb <- FALSE
    for (i in seq_len(n)) {
      ln <- lines[i]
      if (!in_verb && grepl("\\\\begin\\{(verbatim|lstlisting|minted)\\}", ln)) {
        keep[i] <- FALSE
        if (!grepl("\\\\end\\{(verbatim|lstlisting|minted)\\}", ln)) {
          in_verb <- TRUE
        }
        next
      }
      if (in_verb) {
        keep[i] <- FALSE
        if (grepl("\\\\end\\{(verbatim|lstlisting|minted)\\}", ln)) {
          in_verb <- FALSE
        }
        next
      }
    }
    out <- lines
    out[!keep] <- ""
    # Strip LaTeX % comments EXCEPT `% statci:` anchor comments (keep those for tag parser!)
    for (i in seq_len(n)) {
      if (!nzchar(out[i])) next
      if (grepl("^[[:space:]]*%[[:space:]]*statci:", out[i])) next
      # Remove unescaped `% ...` trailing comments
      out[i] <- sub("(?<!\\\\)%.*$", "", out[i], perl = TRUE)
    }
    return(out)
  }

  # Strip YAML frontmatter at top of Markdown/Quarto document
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
  # Guard against pathological oversized lines (ReDoS/DoS defense)
  if (nchar(s, type = "bytes") > 20000L) {
    s <- substr(s, 1L, 20000L)
  }
  # Convert Unicode minus (\u2212) to ASCII hyphen
  s <- gsub("\u2212", "-", s, fixed = TRUE)
  # Distinguish en-dash (\u2013) used as CI range separator vs leading minus sign
  s <- gsub("(?<=[0-9])\u2013(?=[[:space:]]{0,3}-?[0-9.])", " to ", s, perl = TRUE)
  s <- gsub("\u2013(?=[[:space:]]{0,3}[0-9.])", "-", s, perl = TRUE)
  # Unwrap common LaTeX inline styling & math macros safely (linear-time bounded)
  s <- gsub("\\\\(?:textit|textbf|emph|mathrm|text|mathbf)\\{([^{}]{1,80})\\}", "\\1", s, perl = TRUE)
  s <- gsub("\\\\leq\\b", "<=", s, perl = TRUE)
  s <- gsub("\\\\geq\\b", ">=", s, perl = TRUE)
  s <- gsub("\\\\[,;!]", " ", s, perl = TRUE)
  s <- gsub("\\\\chi\\^\\{?2\\}?", "chi2", s, perl = TRUE)
  s <- gsub("\u03c7\u00b2", "chi2", s, fixed = TRUE)
  s <- gsub("\u03c7\\^2", "chi2", s, perl = TRUE)
  # Strip bounded markdown italics/bold/inline math around metric symbols (linear-time)
  s <- gsub("\\*{1,2}([A-Za-z]{1,20})\\*{1,2}", "\\1", s, perl = TRUE)
  s <- gsub("_([A-Za-z]{1,20})_", "\\1", s, perl = TRUE)
  s <- gsub("\\$([^$]{1,200})\\$", "\\1", s, perl = TRUE)
  s <- gsub("\\\\beta\\b", "beta", s, perl = TRUE)
  s <- gsub("\u03b2", "beta", s, fixed = TRUE)
  s <- gsub("R\u00b2", "R2", s, fixed = TRUE)
  # Normalize LaTeX/Unicode scientific notation `1.2 \times 10^{-4}` or `1.2 x 10^-4` -> `1.2e-4`
  s <- gsub(
    "(-?(?:[0-9]{1,12}\\.[0-9]{1,12}|\\.[0-9]{1,12}|[0-9]{1,12}))[[:space:]]{0,4}(?:\\\\times|\u00d7|[xX])[[:space:]]{0,4}10\\^\\{?([+-]?[0-9]{1,4})\\}?",
    "\\1e\\2",
    s,
    perl = TRUE
  )
  s
}

parse_tag_attrs <- function(attr_str) {
  out <- list(model = NA_character_, term = NA_character_)
  if (is.null(attr_str) || !nzchar(attr_str)) return(out)
  if (nchar(attr_str, type = "bytes") > 1000L) {
    attr_str <- substr(attr_str, 1L, 1000L)
  }
  m_mod <- regexec("model[[:space:]]{0,4}=[[:space:]]{0,4}['\"]?([A-Za-z0-9_.:-]{1,128})['\"]?", attr_str, perl = TRUE)
  c_mod <- regmatches(attr_str, m_mod)[[1L]]
  if (length(c_mod) >= 2L) out$model <- c_mod[2L]

  # Strictly linear-time quoted vs unquoted term extraction
  m_trm_q <- regexec("term[[:space:]]{0,4}=[[:space:]]{0,4}['\"]([^'\"]{1,128})['\"]", attr_str, perl = TRUE)
  cq <- regmatches(attr_str, m_trm_q)[[1L]]
  if (length(cq) >= 2L) {
    out$term <- cq[2L]
  } else {
    m_trm_u <- regexec("term[[:space:]]{0,4}=[[:space:]]{0,4}([^[:space:]'\"]{1,128})", attr_str, perl = TRUE)
    cu <- regmatches(attr_str, m_trm_u)[[1L]]
    if (length(cu) >= 2L) out$term <- cu[2L]
  }
  out
}

# Map a table column header string to a canonical statci metric name
map_table_header_metric <- function(hdr) {
  h <- tolower(trimws(gsub("[`*_$\\\\]", "", hdr)))
  h <- gsub("[[:space:]]+", " ", h)
  if (h %in% c("term", "variable", "predictor", "parameter", "effect", "covariate", "factor")) return("term")
  if (h %in% c("b", "beta", "est", "est.", "estimate", "coef", "coef.", "coefficient", "slope")) return("estimate")
  if (h %in% c("or", "odds ratio", "exp(b)")) return("or")
  if (h %in% c("se", "s.e.", "std. error", "std.error", "stderr", "standard error")) return("se")
  if (h %in% c("t", "z", "t value", "z value", "t-value", "z-value", "stat", "statistic", "t/z")) return("statistic")
  if (h %in% c("p", "p-value", "p.value", "p value", "pval", "pr(>|t|)", "pr(>|z|)")) return("p_value")
  if (grepl("^(9[059]%\\s*)?ci$|confidence interval", h)) return("ci_pair")
  if (h %in% c("ci low", "ci_low", "ci_lower", "conf.low", "lower", "lower ci", "2.5%", "ll")) return("ci_lower")
  if (h %in% c("ci high", "ci_high", "ci_upper", "conf.high", "upper", "upper ci", "97.5%", "ul")) return("ci_upper")
  NA_character_
}

parse_table_cell_claims <- function(cells, col_metrics, line_num, tag_model, raw_line, col_ci_levels = NULL) {
  term_idx <- which(col_metrics == "term")
  row_term <- if (length(term_idx) >= 1L && term_idx[1L] <= length(cells)) {
    trm <- trimws(gsub("^[`*_$]+|[`*_$]+$", "", cells[term_idx[1L]]))
    trm <- gsub("\\\\_", "_", trm, fixed = TRUE)
    if (nzchar(trm)) trm else NA_character_
  } else {
    NA_character_
  }
  if (is.na(row_term) || !nzchar(row_term)) return(NULL)

  num_signed <- "[+-]?(?:[0-9]{1,16}\\.[0-9]{1,16}|\\.[0-9]{1,16}|[0-9]{1,16})(?:[eE][+-]?[0-9]{1,4})?"
  claims <- list()
  add_tbl_claim <- function(metric, op, raw_val, ci_level = NA_real_) {
    clean_num <- gsub(",", "", trimws(raw_val), fixed = TRUE)
    val <- suppressWarnings(as.numeric(clean_num))
    if (is.na(val) || !is.finite(val)) return()
    claims[[length(claims) + 1L]] <<- list(
      line = as.integer(line_num),
      model = tag_model,
      term = row_term,
      metric = metric,
      op = op,
      reported_raw = trimws(raw_val),
      reported = val,
      df = NA_real_,
      ci_level = ci_level,
      unit = rounding_unit(clean_num),
      has_fraction = grepl("[.eE]", clean_num),
      context = substr(trimws(raw_line), 1L, 300L)
    )
  }

  for (j in seq_along(col_metrics)) {
    m <- col_metrics[j]
    if (is.na(m) || m == "term" || j > length(cells)) next
    cell <- normalize_prose_line(trimws(cells[j]))
    if (!nzchar(cell) || cell %in% c("-", "--", "\u2014", "NA", ".")) next
    lvl_j <- if (!is.null(col_ci_levels) && j <= length(col_ci_levels)) col_ci_levels[j] else 0.95

    if (m == "ci_pair") {
      pair_pat <- paste0("[\\[\\(]?[[:space:]]{0,3}(", num_signed, ")[[:space:]]{0,3}(?:,|to|[[:space:]]-[[:space:]])[[:space:]]{0,3}(", num_signed, ")[[:space:]]{0,3}[\\]\\)]?")
      cap <- regmatches(cell, regexec(pair_pat, cell, perl = TRUE))[[1L]]
      if (length(cap) >= 3L) {
        add_tbl_claim("ci_lower", "=", cap[2L], ci_level = lvl_j)
        add_tbl_claim("ci_upper", "=", cap[3L], ci_level = lvl_j)
      }
    } else if (m == "p_value") {
      p_cell_pat <- paste0("^(<=|>=|=|<|>)?[[:space:]]{0,3}(", num_signed, ")")
      cap <- regmatches(cell, regexec(p_cell_pat, cell, perl = TRUE))[[1L]]
      if (length(cap) >= 3L) {
        op <- if (nzchar(cap[2L])) cap[2L] else "="
        add_tbl_claim("p_value", op, cap[3L])
      }
    } else {
      cap <- regmatches(cell, regexec(paste0("^(", num_signed, ")"), cell, perl = TRUE))[[1L]]
      if (length(cap) >= 2L) {
        add_tbl_claim(m, "=", cap[2L], ci_level = if (m %in% c("ci_lower", "ci_upper")) lvl_j else NA_real_)
      }
    }
  }

  if (length(claims) == 0L) return(NULL)
  list(line = as.integer(line_num), text = raw_line, tag_model = tag_model, tag_term = row_term, claims = claims)
}

extract_claims_from_segment <- function(seg, line_num, tag_model = NA_character_, tag_term = NA_character_) {
  norm <- normalize_prose_line(seg)
  claims <- list()

  # Strictly linear-time bounded numeric patterns (supports decimals, leading-dot, and scientific notation)
  num_signed <- "[+-]?(?:[0-9]{1,16}\\.[0-9]{1,16}|\\.[0-9]{1,16}|[0-9]{1,16})(?:[eE][+-]?[0-9]{1,4})?"
  num_pos <- "(?:[0-9]{1,16}\\.[0-9]{1,16}|\\.[0-9]{1,16}|[0-9]{1,16})(?:[eE][+-]?[0-9]{1,4})?"

  add_claim <- function(metric, op, raw_val, extra_df = NA_real_, ci_level = NA_real_) {
    clean_num <- gsub(",", "", trimws(raw_val), fixed = TRUE)
    val <- suppressWarnings(as.numeric(clean_num))
    if (is.na(val) || !is.finite(val)) return()
    claims[[length(claims) + 1L]] <<- list(
      line = as.integer(line_num),
      model = tag_model,
      term = tag_term,
      metric = metric,
      op = op,
      reported_raw = trimws(raw_val),
      reported = val,
      df = extra_df,
      ci_level = ci_level,
      unit = rounding_unit(clean_num),
      has_fraction = grepl("[.eE]", clean_num),
      context = substr(trimws(seg), 1L, 300L)
    )
  }

  # 1. Confidence intervals: 90%/95%/99% CI [0.20, 0.64], CI (-0.50, -0.10), 95% CI: 0.20 to 0.64
  ci_brack_pat <- paste0(
    "(?:([0-9]{2})%[[:space:]]{0,3})?CI[[:space:]]{0,3}(?:=|:)?[[:space:]]{0,3}[\\[\\(][[:space:]]{0,3}(",
    num_signed, ")[[:space:]]{0,3}(?:,|to|[[:space:]]-[[:space:]])[[:space:]]{0,3}(",
    num_signed, ")[[:space:]]{0,3}[\\]\\)]"
  )
  ci_to_pat <- paste0(
    "(?:([0-9]{2})%[[:space:]]{0,3})?CI[[:space:]]{0,3}(?:=|:)?[[:space:]]{0,3}(",
    num_signed, ")[[:space:]]{1,3}to[[:space:]]{1,3}(", num_signed, ")"
  )

  for (pat in c(ci_brack_pat, ci_to_pat)) {
    m_ci <- gregexpr(pat, norm, perl = TRUE)
    if (m_ci[[1L]][1L] != -1L) {
      for (mt in regmatches(norm, m_ci)[[1L]]) {
        cap <- regmatches(mt, regexec(pat, mt, perl = TRUE))[[1L]]
        if (length(cap) >= 4L) {
          ci_lvl <- if (nzchar(cap[2L])) {
            lv <- suppressWarnings(as.numeric(cap[2L]) / 100)
            if (is.finite(lv) && lv > 0.5 && lv < 1) lv else 0.95
          } else {
            0.95
          }
          add_claim("ci_lower", "=", cap[3L], ci_level = ci_lvl)
          add_claim("ci_upper", "=", cap[4L], ci_level = ci_lvl)
        }
      }
    }
  }

  # 2. Point estimate: b = ..., beta = ..., B = ..., est = ..., estimate = ..., coef = ...
  # Guard against resampling prose like "B = 1000 bootstrap samples"
  est_pat <- paste0("\\b(beta|estimate|est|coef|[bB])[[:space:]]{0,4}=[[:space:]]{0,4}(", num_signed, ")")
  m_est <- gregexpr(est_pat, norm, perl = TRUE)
  if (m_est[[1L]][1L] != -1L) {
    starts <- as.integer(m_est[[1L]])
    lens <- attr(m_est[[1L]], "match.length")
    mts <- regmatches(norm, m_est)[[1L]]
    for (idx_e in seq_along(mts)) {
      mt <- mts[idx_e]
      cap <- regmatches(mt, regexec(est_pat, mt, perl = TRUE))[[1L]]
      if (length(cap) >= 3L) {
        sym_lbl <- cap[2L]
        val_str <- cap[3L]
        if (identical(sym_lbl, "B") && !grepl("[.eE]", val_str)) {
          v_int <- suppressWarnings(as.numeric(val_str))
          after_txt <- substr(norm, starts[idx_e] + lens[idx_e], min(nchar(norm), starts[idx_e] + lens[idx_e] + 45L))
          if (is.finite(v_int) && v_int >= 10 &&
              grepl("bootstrap|boot|permut|simulat|iterat|replicat|resampl|sample|draw|monte\\s*carlo",
                    after_txt, ignore.case = TRUE, perl = TRUE)) {
            next
          }
        }
        add_claim("estimate", "=", val_str)
      }
    }
  }

  # 3. Odds ratio: OR = ...
  or_pat <- paste0("\\bOR[[:space:]]{0,4}=[[:space:]]{0,4}(", num_pos, ")")
  m_or <- gregexpr(or_pat, norm, perl = TRUE)
  if (m_or[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_or)[[1L]]) {
      val <- sub("^[^=]*=[[:space:]]*", "", mt)
      add_claim("or", "=", val)
    }
  }

  # 4. Standard error: SE = ..., S.E. = ...
  se_pat <- paste0("\\bS\\.?E\\.?[[:space:]]{0,4}=[[:space:]]{0,4}(", num_pos, ")")
  m_se <- gregexpr(se_pat, norm, perl = TRUE)
  if (m_se[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_se)[[1L]]) {
      val <- sub("^[^=]*=[[:space:]]*", "", mt)
      add_claim("se", "=", val)
    }
  }

  # 5. p-values: p = .003, p < .001, p = 1.2e-4, p <= 0.05, p > .05
  p_pat <- paste0("\\bp[[:space:]]{0,4}(<=|>=|=|<|>)[[:space:]]{0,4}(", num_pos, ")\\b")
  m_p <- gregexpr(p_pat, norm, perl = TRUE)
  if (m_p[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_p)[[1L]]) {
      cap <- regmatches(mt, regexec(p_pat, mt, perl = TRUE))[[1L]]
      if (length(cap) >= 3L) {
        add_claim("p_value", cap[2L], cap[3L])
      }
    }
  }

  # 6. Test statistic: t(df) = ..., t = ..., z = ...
  stat_pat <- paste0(
    "\\b([tz])[[:space:]]{0,3}(?:\\([[:space:]]{0,3}([0-9]{1,8}(?:\\.[0-9]{1,4})?)[[:space:]]{0,3}\\))?[[:space:]]{0,4}=[[:space:]]{0,4}(",
    num_signed, ")"
  )
  m_st <- gregexpr(stat_pat, norm, perl = TRUE)
  if (m_st[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_st)[[1L]]) {
      cap <- regmatches(mt, regexec(stat_pat, mt, perl = TRUE))[[1L]]
      if (length(cap) >= 4L) {
        df_num <- suppressWarnings(as.numeric(cap[3L]))
        add_claim("statistic", "=", cap[4L], extra_df = df_num)
      }
    }
  }

  # 7. APA F-statistic: F(df1, df2) = 12.45
  f_pat <- paste0(
    "\\bF[[:space:]]{0,3}\\([[:space:]]{0,3}[0-9]{1,6}(?:\\.[0-9]{1,3})?[[:space:]]{0,3},[[:space:]]{0,3}[0-9]{1,8}(?:\\.[0-9]{1,3})?[[:space:]]{0,3}\\)[[:space:]]{0,4}=[[:space:]]{0,4}(",
    num_pos, ")"
  )
  m_f <- gregexpr(f_pat, norm, perl = TRUE)
  if (m_f[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_f)[[1L]]) {
      add_claim("f_stat", "=", sub("^[^=]*=[[:space:]]*", "", mt))
    }
  }

  # 8. APA Chi-squared: chi2(df) = 8.42, X2(df) = 8.42, chi^2(df) = ...
  chi_pat <- paste0(
    "\\b(?:chi\\^?2|X\\^?2)[[:space:]]{0,3}(?:\\([[:space:]]{0,3}[0-9]{1,6}(?:\\.[0-9]{1,3})?(?:,[[:space:]]{0,3}[Nn][[:space:]]{0,2}=[[:space:]]{0,2}[0-9,]{1,10})?[[:space:]]{0,3}\\))?[[:space:]]{0,4}=[[:space:]]{0,4}(",
    num_pos, ")"
  )
  m_chi <- gregexpr(chi_pat, norm, perl = TRUE)
  if (m_chi[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_chi)[[1L]]) {
      add_claim("chisq", "=", sub("^[^=]*=[[:space:]]*", "", mt))
    }
  }

  # 9. Correlation: r = .34, r = -.42, r(98) = -0.41
  r_pat <- paste0(
    "\\br[[:space:]]{0,3}(?:\\([[:space:]]{0,3}[0-9]{1,8}(?:\\.[0-9]{1,4})?[[:space:]]{0,3}\\))?[[:space:]]{0,4}=[[:space:]]{0,4}(",
    num_signed, ")"
  )
  m_r <- gregexpr(r_pat, norm, perl = TRUE)
  if (m_r[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_r)[[1L]]) {
      val <- sub("^[^=]*=[[:space:]]*", "", mt)
      v_num <- suppressWarnings(as.numeric(val))
      if (!is.na(v_num) && abs(v_num) <= 1.0001) {
        add_claim("r", "=", val)
      }
    }
  }

  # 10. Sample size: N = 250, n = 1,250
  n_pat <- "\\b[Nn][[:space:]]{0,4}=[[:space:]]{0,4}([0-9][0-9,]{0,14})\\b"
  m_n <- gregexpr(n_pat, norm, perl = TRUE)
  if (m_n[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_n)[[1L]]) {
      val <- sub("^[^=]*=[[:space:]]*", "", mt)
      add_claim("n", "=", val)
    }
  }

  # 11. R-squared: R^2 = 0.45, R2 = .45
  r2_pat <- paste0("\\bR\\^?\\{?2\\}?[[:space:]]{0,4}=[[:space:]]{0,4}(", num_pos, ")")
  m_r2 <- gregexpr(r2_pat, norm, perl = TRUE)
  if (m_r2[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_r2)[[1L]]) {
      val <- sub("^[^=]*=[[:space:]]*", "", mt)
      add_claim("r_squared", "=", val)
    }
  }

  # 12. AIC / BIC
  aic_pat <- paste0("\\bAIC[[:space:]]{0,4}=[[:space:]]{0,4}(", num_signed, ")")
  m_aic <- gregexpr(aic_pat, norm, perl = TRUE)
  if (m_aic[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_aic)[[1L]]) {
      add_claim("aic", "=", sub("^[^=]*=[[:space:]]*", "", mt))
    }
  }
  bic_pat <- paste0("\\bBIC[[:space:]]{0,4}=[[:space:]]{0,4}(", num_signed, ")")
  m_bic <- gregexpr(bic_pat, norm, perl = TRUE)
  if (m_bic[[1L]][1L] != -1L) {
    for (mt in regmatches(norm, m_bic)[[1L]]) {
      add_claim("bic", "=", sub("^[^=]*=[[:space:]]*", "", mt))
    }
  }

  claims
}

#' Compute the place-value unit (1 ulp in decimal) of a reported numeric string
#' @noRd
rounding_unit <- function(num_str) {
  s <- trimws(num_str)
  s <- sub("^[+-]", "", s)
  if (grepl("[eE]", s)) {
    parts <- strsplit(s, "[eE]")[[1L]]
    mant <- parts[1L]
    exp_val <- suppressWarnings(as.integer(parts[2L]))
    if (is.na(exp_val)) exp_val <- 0L
    d_mant <- if (grepl("\\.", mant)) nchar(sub("^[^.]*\\.", "", mant)) else 0L
    return(10^(exp_val - d_mant))
  }
  d <- if (grepl("\\.", s)) nchar(sub("^[^.]*\\.", "", s)) else 0L
  10^(-d)
}

# ---------------------------------------------------------------------------
# Internal Arithmetic Consistency Engine (Interval Arithmetic)
# ---------------------------------------------------------------------------

evaluate_internal_arithmetic <- function(claim_groups, tol_extra = 0) {
  rows <- list()
  add_ic <- function(line, term, check_type, reported_expr, expected_range, status, detail) {
    rows[[length(rows) + 1L]] <<- tibble::tibble(
      line = as.integer(line),
      term = term %||% NA_character_,
      check_type = check_type,
      reported_expr = reported_expr,
      expected_range = expected_range,
      status = status,
      detail = detail
    )
  }

  for (grp in claim_groups) {
    cl_list <- grp$claims
    metrics <- vapply(cl_list, `[[`, character(1L), "metric")
    b_idx <- which(metrics == "estimate")
    se_idx <- which(metrics == "se")
    t_idx <- which(metrics == "statistic")
    cil_idx <- which(metrics == "ci_lower")
    ciu_idx <- which(metrics == "ci_upper")

    # (a) Wald ratio check: t == b / SE (accounting for rounding intervals!)
    if (length(b_idx) == 1L && length(se_idx) == 1L && length(t_idx) == 1L) {
      cb <- cl_list[[b_idx]]
      cs <- cl_list[[se_idx]]
      ct <- cl_list[[t_idx]]
      db <- 0.5 * (cb$unit %||% 0.01) + tol_extra
      ds <- 0.5 * (cs$unit %||% 0.01) + tol_extra
      dt <- 0.5 * (ct$unit %||% 0.01) + tol_extra
      if (cs$reported > ds + 1e-12) {
        b_rng <- c(cb$reported - db, cb$reported + db)
        s_rng <- c(max(1e-12, cs$reported - ds), cs$reported + ds)
        corners <- as.vector(outer(b_rng, s_rng, "/"))
        t_lo <- min(corners) - dt - 1e-9
        t_hi <- max(corners) + dt + 1e-9
        ok <- ct$reported >= t_lo && ct$reported <= t_hi
        # Allow minor warning band if off by <= 2*dt
        near_ok <- ct$reported >= (t_lo - 2 * dt) && ct$reported <= (t_hi + 2 * dt)
        st <- if (ok) "pass" else if (near_ok) "warn" else "fail"
        add_ic(
          line = grp$line,
          term = grp$tag_term,
          check_type = "internal_t_ratio",
          reported_expr = paste0("b=", cb$reported_raw, ", SE=", cs$reported_raw, ", t=", ct$reported_raw),
          expected_range = paste0("[", fmt_num(min(corners), 2L), ", ", fmt_num(max(corners), 2L), "]"),
          status = st,
          detail = if (ok) "t consistent with b / SE within rounding" else paste0(
            "Reported t = ", ct$reported_raw, " inconsistent with b / SE (",
            fmt_num(cb$reported / cs$reported, 2L), ")"
          )
        )
      }
    }

    # (b) Confidence interval internal checks: ordering, midpoint symmetry, and SE half-width
    if (length(cil_idx) == 1L && length(ciu_idx) == 1L) {
      cl <- cl_list[[cil_idx]]
      cu <- cl_list[[ciu_idx]]
      dl <- 0.5 * (cl$unit %||% 0.01) + tol_extra
      du <- 0.5 * (cu$unit %||% 0.01) + tol_extra

      # Bound ordering
      if (cl$reported > cu$reported + 1e-9) {
        add_ic(
          line = grp$line,
          term = grp$tag_term,
          check_type = "internal_ci_order",
          reported_expr = paste0("CI [", cl$reported_raw, ", ", cu$reported_raw, "]"),
          expected_range = "lower <= upper",
          status = "fail",
          detail = "Confidence interval lower bound exceeds upper bound."
        )
      } else if (length(b_idx) == 1L) {
        cb <- cl_list[[b_idx]]
        db <- 0.5 * (cb$unit %||% 0.01) + tol_extra
        mid <- 0.5 * (cl$reported + cu$reported)
        mid_tol <- db + 0.5 * (dl + du) + 1e-9
        # Only check midpoint symmetry when b is not obviously an exponentiated OR
        if (abs(mid - cb$reported) > mid_tol) {
          # Allow warn band if off by small rounding jitter (<= 2x tol)
          st <- if (abs(mid - cb$reported) <= 2 * mid_tol) "warn" else "fail"
          add_ic(
            line = grp$line,
            term = grp$tag_term,
            check_type = "internal_ci_midpoint",
            reported_expr = paste0("b=", cb$reported_raw, ", CI [", cl$reported_raw, ", ", cu$reported_raw, "]"),
            expected_range = paste0("midpoint=", fmt_num(mid, 3L), " (+/- ", fmt_num(mid_tol, 3L), ")"),
            status = st,
            detail = paste0("CI midpoint (", fmt_num(mid, 3L), ") disagrees with reported b (", cb$reported_raw, ").")
          )
        } else {
          add_ic(
            line = grp$line,
            term = grp$tag_term,
            check_type = "internal_ci_midpoint",
            reported_expr = paste0("b=", cb$reported_raw, ", CI [", cl$reported_raw, ", ", cu$reported_raw, "]"),
            expected_range = paste0("midpoint=", fmt_num(mid, 3L)),
            status = "pass",
            detail = "CI midpoint consistent with reported b within rounding"
          )
        }

        # Half-width vs SE check respecting reported CI confidence level (e.g., 90%, 95%, 99%)
        if (length(se_idx) == 1L) {
          cs <- cl_list[[se_idx]]
          ds <- 0.5 * (cs$unit %||% 0.01) + tol_extra
          if (cs$reported > ds + 1e-12) {
            hw <- 0.5 * (cu$reported - cl$reported)
            hw_tol <- 0.5 * (dl + du) + 1e-9
            ci_lvl <- cl$ci_level %||% 0.95
            if (is.na(ci_lvl) || !is.finite(ci_lvl) || ci_lvl <= 0.5 || ci_lvl >= 1) ci_lvl <- 0.95
            p_tail <- (1 + ci_lvl) / 2
            rep_df <- if (length(t_idx) == 1L) (cl_list[[t_idx]]$df %||% NA_real_) else NA_real_
            if (!is.na(rep_df) && is.finite(rep_df) && rep_df >= 1) {
              crit_lo <- stats::qt(p_tail, df = rep_df) - 0.02
              crit_hi <- stats::qt(p_tail, df = rep_df) + 0.02
            } else {
              crit_lo <- stats::qnorm(p_tail) - 0.02
              crit_hi <- stats::qt(p_tail, df = 4) + 0.02
            }
            exp_lo <- crit_lo * max(0, cs$reported - ds) - hw_tol
            exp_hi <- crit_hi * (cs$reported + ds) + hw_tol
            ok_hw <- hw >= exp_lo && hw <= exp_hi
            add_ic(
              line = grp$line,
              term = grp$tag_term,
              check_type = "internal_ci_width",
              reported_expr = paste0("SE=", cs$reported_raw, ", ", round(100 * ci_lvl), "% CI [", cl$reported_raw, ", ", cu$reported_raw, "]"),
              expected_range = paste0("half-width in [", fmt_num(exp_lo, 3L), ", ", fmt_num(exp_hi, 3L), "]"),
              status = if (ok_hw) "pass" else "fail",
              detail = if (ok_hw) "CI half-width consistent with SE" else paste0(
                "CI half-width (", fmt_num(hw, 3L), ") inconsistent with SE (", cs$reported_raw, ")."
              )
            )
          }
        }
      }
    }

    # (c) Probability & fit-statistic domain bounds (p in [0, 1], R^2 in [-0.5, 1])
    for (cp in cl_list[which(metrics == "p_value")]) {
      if (cp$reported < -1e-9 || cp$reported > 1 + 1e-9) {
        add_ic(
          line = grp$line,
          term = grp$tag_term,
          check_type = "internal_p_bounds",
          reported_expr = paste0("p ", cp$op, " ", cp$reported_raw),
          expected_range = "[0, 1]",
          status = "fail",
          detail = paste0("Impossible p-value (", cp$reported_raw, ") outside [0, 1].")
        )
      }
    }
    for (cr in cl_list[which(metrics == "r_squared")]) {
      if (cr$reported > 1 + 0.5 * (cr$unit %||% 0.01) + tol_extra + 1e-9 || cr$reported < -0.5) {
        add_ic(
          line = grp$line,
          term = grp$tag_term,
          check_type = "internal_r2_bounds",
          reported_expr = paste0("R^2 = ", cr$reported_raw),
          expected_range = "[0, 1]",
          status = "fail",
          detail = paste0("Impossible R^2 (", cr$reported_raw, ") outside [0, 1].")
        )
      }
    }
  }

  if (length(rows) == 0L) {
    return(tibble::tibble(
      line = integer(0), term = character(0), check_type = character(0),
      reported_expr = character(0), expected_range = character(0),
      status = character(0), detail = character(0)
    ))
  }
  tibble::as_tibble(do.call(rbind, rows))
}

# ---------------------------------------------------------------------------
# Core manuscript auditing logic (Prose + Markdown Tables + LaTeX Tables)
# ---------------------------------------------------------------------------

audit_manuscript_lines <- function(lines, source_label, models, tol_extra, warn_rounding,
                                   default_model, format = "markdown", check_internal = TRUE) {
  if (!is.numeric(tol_extra) || length(tol_extra) != 1L || !is.finite(tol_extra) || tol_extra < 0) {
    rlang::abort("`tol_extra` must be a finite non-negative numeric scalar.")
  }
  if (!is.logical(warn_rounding) || length(warn_rounding) != 1L || is.na(warn_rounding)) {
    rlang::abort("`warn_rounding` must be a non-NA logical scalar.")
  }
  if (!is.logical(check_internal) || length(check_internal) != 1L || is.na(check_internal)) {
    rlang::abort("`check_internal` must be a non-NA logical scalar.")
  }
  raw_models <- load_models_input(models)
  catalog <- lapply(raw_models, extract_single_model)
  model_names <- names(catalog)
  if (!is.null(default_model) && length(model_names) > 0L && !(default_model %in% model_names)) {
    rlang::abort(paste0("`default_model` (`", default_model, "`) not found in `models`."))
  }

  prose <- strip_non_prose_lines(lines, format = format)

  claim_groups <- list()
  pending_tag <- NULL

  span_pat <- "\\[([^\\]]{1,500})\\]\\{\\.statci[[:space:]]{1,4}([^}]{1,250})\\}"
  html_comment_pat <- "<!--+[[:space:]]{0,4}statci:[[:space:]]{0,4}([^<>]{0,250}?)--+>"
  tex_comment_pat <- "^[[:space:]]*%[[:space:]]{0,4}statci:[[:space:]]{0,4}(.{1,250})$"

  # Track Markdown / LaTeX table parsing state
  in_md_table <- FALSE
  md_col_metrics <- NULL
  md_col_ci_levels <- NULL
  md_table_model <- NA_character_

  in_tex_tabular <- FALSE
  tex_col_metrics <- NULL
  tex_col_ci_levels <- NULL
  tex_table_model <- NA_character_

  extract_header_ci_levels <- function(hdrs) {
    vapply(hdrs, function(h) {
      cap <- regmatches(h, regexec("([0-9]{2})%", h, perl = TRUE))[[1L]]
      if (length(cap) >= 2L) {
        lv <- suppressWarnings(as.numeric(cap[2L]) / 100)
        if (is.finite(lv) && lv > 0.5 && lv < 1) return(lv)
      }
      0.95
    }, numeric(1L))
  }

  i <- 1L
  n_lines <- length(prose)
  while (i <= n_lines) {
    ln <- prose[i]
    trimmed <- trimws(ln)

    if (!nzchar(trimmed)) {
      in_md_table <- FALSE
      md_col_metrics <- NULL
      i <- i + 1L
      next
    }

    # 1. Check HTML or LaTeX comment anchor tag
    is_tex_tag <- format == "tex" && grepl(tex_comment_pat, ln, perl = TRUE)
    if (is_tex_tag) {
      cap <- regmatches(ln, regexec(tex_comment_pat, ln, perl = TRUE))[[1L]]
      pending_tag <- parse_tag_attrs(cap[2L])
      i <- i + 1L
      next
    }
    cm <- regexec(html_comment_pat, ln, perl = TRUE)
    ccap <- regmatches(ln, cm)[[1L]]
    line_tag <- pending_tag
    if (length(ccap) >= 2L) {
      attrs <- parse_tag_attrs(ccap[2L])
      remainder <- trimws(sub(html_comment_pat, "", ln, perl = TRUE))
      if (!nzchar(remainder)) {
        pending_tag <- attrs
        i <- i + 1L
        next
      } else {
        line_tag <- attrs
        pending_tag <- NULL
        ln <- remainder
        trimmed <- trimws(ln)
      }
    } else {
      pending_tag <- NULL
    }

    # 2. LaTeX tabular table detection (`\begin{tabular}` ... `\end{tabular}`)
    if (format == "tex") {
      if (grepl("\\\\begin\\{tabular\\}", ln)) {
        in_tex_tabular <- TRUE
        tex_col_metrics <- NULL
        tex_table_model <- if (!is.null(line_tag)) line_tag$model else NA_character_
        i <- i + 1L
        next
      }
      if (in_tex_tabular) {
        if (grepl("\\\\end\\{tabular\\}", ln)) {
          in_tex_tabular <- FALSE
          tex_col_metrics <- NULL
          i <- i + 1L
          next
        }
        if (grepl("&", ln, fixed = TRUE) && !grepl("^\\\\(hline|toprule|midrule|bottomrule|cline)", trimmed)) {
          row_clean <- sub("\\\\\\\\.*$", "", trimmed)
          cells <- trimws(strsplit(row_clean, "&", fixed = TRUE)[[1L]])
          if (is.null(tex_col_metrics)) {
            mapped <- vapply(cells, map_table_header_metric, character(1L))
            if (any(!is.na(mapped))) {
              tex_col_metrics <- mapped
              tex_col_ci_levels <- extract_header_ci_levels(cells)
            }
          } else {
            grp <- parse_table_cell_claims(cells, tex_col_metrics, i, tex_table_model, trimmed, col_ci_levels = tex_col_ci_levels)
            if (!is.null(grp)) claim_groups[[length(claim_groups) + 1L]] <- grp
          }
        }
        i <- i + 1L
        next
      }
    }

    # 3. Markdown / Quarto / GFM pipe table detection (with or without outer `|` borders)
    if (format != "tex" && grepl("|", trimmed, fixed = TRUE)) {
      is_gfm_sep <- function(s) {
        if (!grepl("|", s, fixed = TRUE)) return(FALSE)
        if (!grepl("^[[:space:]:|-]+$", s)) return(FALSE)
        s_clean <- sub("^\\|", "", sub("\\|$", "", s))
        parts <- trimws(strsplit(s_clean, "|", fixed = TRUE)[[1L]])
        length(parts) >= 2L && all(grepl("^:?-+:?$", parts))
      }
      if (!in_md_table && i + 1L <= n_lines && is_gfm_sep(trimws(prose[i + 1L]))) {
        raw_hdr <- sub("^\\|", "", sub("\\|$", "", trimmed))
        hdr_cells <- trimws(strsplit(raw_hdr, "|", fixed = TRUE)[[1L]])
        mapped <- vapply(hdr_cells, map_table_header_metric, character(1L))
        if (any(!is.na(mapped)) && "term" %in% mapped) {
          in_md_table <- TRUE
          md_col_metrics <- mapped
          md_col_ci_levels <- extract_header_ci_levels(hdr_cells)
          md_table_model <- if (!is.null(line_tag)) line_tag$model else NA_character_
          i <- i + 2L # skip header + separator line
          next
        }
      } else if (in_md_table) {
        raw_row <- sub("^\\|", "", sub("\\|$", "", trimmed))
        cells <- trimws(strsplit(raw_row, "|", fixed = TRUE)[[1L]])
        grp <- parse_table_cell_claims(cells, md_col_metrics, i, md_table_model, trimmed, col_ci_levels = md_col_ci_levels)
        if (!is.null(grp)) claim_groups[[length(claim_groups) + 1L]] <- grp
        i <- i + 1L
        next
      }
    } else {
      in_md_table <- FALSE
      md_col_metrics <- NULL
      md_col_ci_levels <- NULL
    }

    # 4. Extract any Quarto inline spans `[...]{.statci ...}` on this line
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
      ln <- gsub(span_pat, "", ln, perl = TRUE)
    }

    # 5. Extract remaining prose claims on the line
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

    i <- i + 1L
  }

  # Evaluate internal arithmetic consistency across all claim groups
  internal_checks <- if (isTRUE(check_internal)) {
    evaluate_internal_arithmetic(claim_groups, tol_extra = tol_extra)
  } else {
    tibble::tibble(
      line = integer(0), term = character(0), check_type = character(0),
      reported_expr = character(0), expected_range = character(0),
      status = character(0), detail = character(0)
    )
  }

  # Evaluate every claim against the model catalog (when models are supplied)
  check_rows <- list()

  if (length(model_names) > 0L) {
    fit_metrics <- c("n", "r_squared", "aic", "bic", "f_stat", "chisq")

    for (grp in claim_groups) {
      m_name <- grp$tag_model
      if (is.na(m_name) || !nzchar(m_name)) {
        if (!is.null(default_model)) {
          m_name <- default_model
        } else if (length(model_names) == 1L) {
          m_name <- model_names[1L]
        } else {
          mentioned <- model_names[vapply(model_names, function(nm) {
            nm_esc <- gsub("([.^$*+?()\\[\\]{}|\\\\])", "\\\\\\1", nm, perl = TRUE)
            grepl(paste0("(?<![A-Za-z0-9_])", nm_esc, "(?![A-Za-z0-9_])"), grp$text, perl = TRUE)
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
  n_ic_fail <- sum(internal_checks$status == "fail")
  n_ic_warn <- sum(internal_checks$status == "warn")
  overall <- combine_status(c(checks$status, internal_checks$status))

  structure(
    list(
      checks = checks,
      internal_checks = internal_checks,
      manuscript = source_label,
      format = format,
      models_checked = model_names,
      summary = list(
        n_claims = nrow(checks),
        n_pass = as.integer(n_pass),
        n_warn = as.integer(n_warn),
        n_fail = as.integer(n_fail),
        n_sign_flips = as.integer(sum(checks$issue_type == "sign_flip")),
        n_rounding = as.integer(sum(checks$issue_type == "rounding_discrepancy")),
        n_internal_checks = nrow(internal_checks),
        n_internal_warn = as.integer(n_ic_warn),
        n_internal_fail = as.integer(n_ic_fail)
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

  hits <- cand$term[vapply(cand$term, function(tm) {
    grepl(paste0("(?<![A-Za-z0-9_])", gsub("([.^$|()*+?{}\\[\\]\\\\])", "\\\\\\1", tm), "(?![A-Za-z0-9_])"), text, perl = TRUE)
  }, logical(1L))]
  if (length(hits) == 1L) return(hits[1L])

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

  if (cl$metric == "n") {
    d <- cl$reported - actual
    if (abs(d) <= 1e-9) {
      return(list(diff = 0, tol = 0, issue_type = "match", status = "pass"))
    }
    return(list(diff = d, tol = 0, issue_type = "stale_sample_size", status = "fail"))
  }

  if (cl$metric == "p_value" && cl$op != "=") {
    ok <- eval_ineq(actual, cl$op, cl$reported)
    return(list(
      diff = actual - cl$reported,
      tol = 0,
      issue_type = if (ok) "match" else "inequality_violation",
      status = if (ok) "pass" else "fail"
    ))
  }

  u <- cl$unit %||% 1
  eps <- 1e-11 * max(1, abs(actual))
  half_unit <- 0.5 * u + tol_extra
  one_half_unit <- 1.5 * u + tol_extra
  diff_val <- cl$reported - actual
  abs_diff <- abs(diff_val)

  if (cl$metric %in% c("estimate", "statistic", "r", "ci_lower", "ci_upper")) {
    if (cl$reported != 0 && sign(cl$reported) != sign(actual) && abs(actual) > half_unit + eps) {
      return(list(diff = diff_val, tol = half_unit, issue_type = "sign_flip", status = "fail"))
    }
  }

  if (abs_diff <= half_unit + eps) {
    return(list(diff = diff_val, tol = half_unit, issue_type = "match", status = "pass"))
  }

  if (isTRUE(warn_rounding) && isTRUE(cl$has_fraction) && abs_diff <= one_half_unit + eps) {
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
  ic_msg <- if (s$n_internal_checks > 0L) {
    paste0(" | internal: ", s$n_internal_checks, " (", s$n_internal_fail, " fail)")
  } else {
    ""
  }
  cli::cli_text(
    "{badge} {.strong statci Manuscript Audit} ({basename(x$manuscript)}): ",
    "{s$n_claims} claim(s) | {s$n_pass} pass, {s$n_warn} warn, {s$n_fail} fail{ic_msg}"
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
  ic_bad <- x$internal_checks[x$internal_checks$status != "pass", , drop = FALSE]
  if (nrow(ic_bad) > 0L) {
    show_ic <- min(nrow(ic_bad), as.integer(max_rows))
    for (i in seq_len(show_ic)) {
      r <- ic_bad[i, ]
      b <- format_status_badge(r$status)
      cli::cli_text("  {b} line {r$line} ({r$check_type}): {r$detail}")
    }
  }
  invisible(x)
}
