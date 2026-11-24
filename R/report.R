# Unified Audit Report, CI Gate, and Standalone HTML/Markdown/JSON Renderers

#' Assemble a unified `statci` audit report
#'
#' Combines results from any subset of `statci` verification suites—AST linting
#' ([stat_lint()] / [stat_lint_dir()]), estimator calibration
#' ([audit_estimator()]), Simulation-Based Calibration ([audit_sbc()]),
#' parallel RNG invariance ([check_parallel_rng()]), manuscript auditing
#' ([audit_manuscript()]), and adversarial sensitivity ([audit_sensitivity()])—
#' into a single pass/warn/fail audit report object.
#'
#' @param lint Optional `"statci_lint"` object.
#' @param estimator Optional `"statci_estimator_audit"` object.
#' @param sbc Optional standalone `"statci_sbc"` object.
#' @param parallel_rng Optional standalone `"statci_parallel_rng"` object.
#' @param manuscript Optional `"statci_manuscript_audit"` object.
#' @param sensitivity Optional `"statci_sensitivity"` object.
#' @param power_curve Optional `"statci_power_curve"` object.
#' @param multiverse Optional `"statci_multiverse"` object.
#' @param title Report title string.
#' @param metadata Optional named list of custom metadata (e.g., git SHA,
#'   project name).
#'
#' @return An S3 object of class `"statci_report"` containing `title`,
#'   `sections` (summary tibble), `components`, `metadata`, and overall
#'   `status` (`"pass"`, `"warn"`, or `"fail"`).
#' @export
#'
#' @examples
#' lint_res <- stat_lint_text("set.seed(1)\nx <- rnorm(20)\n")
#' rep <- statci_report(lint = lint_res)
#' rep
statci_report <- function(lint = NULL,
                          estimator = NULL,
                          sbc = NULL,
                          parallel_rng = NULL,
                          manuscript = NULL,
                          sensitivity = NULL,
                          power_curve = NULL,
                          multiverse = NULL,
                          title = "statci Statistical CI/CD Audit Report",
                          metadata = list()) {
  validate_opt <- function(obj, cls, arg) {
    if (!is.null(obj) && !inherits(obj, cls)) {
      rlang::abort(paste0("`", arg, "` must be a `", cls, "` object or NULL."))
    }
  }
  validate_opt(lint, "statci_lint", "lint")
  validate_opt(estimator, "statci_estimator_audit", "estimator")
  validate_opt(sbc, "statci_sbc", "sbc")
  validate_opt(parallel_rng, "statci_parallel_rng", "parallel_rng")
  validate_opt(manuscript, "statci_manuscript_audit", "manuscript")
  validate_opt(sensitivity, "statci_sensitivity", "sensitivity")
  validate_opt(power_curve, "statci_power_curve", "power_curve")
  validate_opt(multiverse, "statci_multiverse", "multiverse")

  sec_rows <- list()
  add_sec <- function(name, checks, passed, warnings, failures, status, detail) {
    sec_rows[[length(sec_rows) + 1L]] <<- tibble::tibble(
      section = name,
      checks = as.integer(checks),
      passed = as.integer(passed),
      warnings = as.integer(warnings),
      failures = as.integer(failures),
      status = status,
      detail = detail
    )
  }

  if (!is.null(lint)) {
    s <- lint$summary
    add_sec(
      "AST Statistical Linter",
      checks = s$n_rules,
      passed = max(0L, s$n_rules - length(unique(lint$findings$rule))),
      warnings = s$n_warning,
      failures = s$n_error,
      status = lint$status,
      detail = paste0(s$n_files, " file(s), ", s$n_findings, " finding(s) (",
                      s$n_error, " error, ", s$n_warning, " warning, ", s$n_info, " info)")
    )
  }

  if (!is.null(estimator)) {
    st_vec <- c(
      estimator$convergence$status,
      if (!is.null(estimator$recovery)) estimator$recovery$status,
      if (!is.null(estimator$type1)) estimator$type1$status,
      if (!is.null(estimator$sbc)) estimator$sbc$status,
      if (!is.null(estimator$parallel_rng)) estimator$parallel_rng$status
    )
    add_sec(
      "Estimator Calibration & Fuzzing",
      checks = length(st_vec),
      passed = sum(st_vec == "pass"),
      warnings = sum(st_vec == "warn"),
      failures = sum(st_vec == "fail"),
      status = estimator$status,
      detail = paste0(
        estimator$convergence$n_valid, "/", estimator$convergence$n_total,
        " valid reps; fail_rate = ", fmt_pct(estimator$convergence$fail_rate),
        ", singular_rate = ", fmt_pct(estimator$convergence$singular_rate)
      )
    )
  }

  if (!is.null(sbc)) {
    st_vec <- sbc$summary$status
    add_sec(
      "Simulation-Based Calibration (SBC)",
      checks = length(st_vec),
      passed = sum(st_vec == "pass"),
      warnings = sum(st_vec == "warn"),
      failures = sum(st_vec == "fail"),
      status = sbc$status,
      detail = paste0(sbc$n_valid, "/", sbc$n_sim, " reps, L = ", sbc$n_draws, " draws, ", sbc$n_bins, " bins")
    )
  }

  if (!is.null(parallel_rng)) {
    st_vec <- parallel_rng$comparisons$status
    add_sec(
      "Parallel RNG Invariance",
      checks = length(st_vec),
      passed = sum(st_vec == "pass"),
      warnings = 0L,
      failures = sum(st_vec == "fail"),
      status = parallel_rng$status,
      detail = paste0("cores = ", paste(parallel_rng$cores_tested, collapse = " vs "),
                      ", identical = ", parallel_rng$identical_all)
    )
  }

  if (!is.null(manuscript)) {
    s <- manuscript$summary
    add_sec(
      "Manuscript-to-Code Consistency",
      checks = s$n_claims,
      passed = s$n_pass,
      warnings = s$n_warn,
      failures = s$n_fail,
      status = manuscript$status,
      detail = paste0(s$n_claims, " claim(s) audited against ", length(manuscript$models_checked),
                      " model(s); ", s$n_sign_flips, " sign flip(s)")
    )
  }

  if (!is.null(sensitivity)) {
    st_vec <- sensitivity$results$status
    add_sec(
      "Adversarial Sensitivity Battery",
      checks = length(st_vec),
      passed = sum(st_vec == "pass"),
      warnings = sum(st_vec == "warn"),
      failures = sum(st_vec == "fail"),
      status = sensitivity$status,
      detail = paste0(length(sensitivity$baseline), " parameter(s), ", nrow(sensitivity$results), " perturbation check(s)")
    )
  }

  if (!is.null(power_curve)) {
    st_vec <- c(
      power_curve$monotonicity$status,
      if (isTRUE(power_curve$null_check$evaluated)) power_curve$null_check$status,
      if (isTRUE(power_curve$target_check$evaluated)) power_curve$target_check$status
    )
    add_sec(
      "Power-Curve Audit",
      checks = length(st_vec),
      passed = sum(st_vec == "pass"),
      warnings = sum(st_vec == "warn"),
      failures = sum(st_vec == "fail"),
      status = power_curve$status,
      detail = paste0(nrow(power_curve$curve), " grid point(s), MDE = ", fmt_num(power_curve$mde, 3L))
    )
  }

  if (!is.null(multiverse)) {
    ms <- multiverse$summary
    add_sec(
      "Multiverse Specification Curve",
      checks = ms$n_specs,
      passed = if (multiverse$status == "pass") ms$n_valid else 0L,
      warnings = if (multiverse$status == "warn") 1L else 0L,
      failures = if (multiverse$status == "fail") 1L else 0L,
      status = multiverse$status,
      detail = paste0(
        ms$n_valid, "/", ms$n_specs, " specs; median = ", fmt_num(ms$median_est, 3L),
        ", IQR = ", fmt_num(ms$iqr_est, 3L),
        ", sign consistency = ", fmt_pct(ms$sign_consistency)
      )
    )
  }

  sections <- if (length(sec_rows) > 0L) {
    tibble::as_tibble(do.call(rbind, sec_rows))
  } else {
    tibble::tibble(
      section = character(0), checks = integer(0), passed = integer(0),
      warnings = integer(0), failures = integer(0), status = character(0),
      detail = character(0)
    )
  }

  overall <- combine_status(sections$status)
  pkg_ver <- tryCatch(as.character(utils::packageVersion("statci")), error = function(e) "0.1.0")
  meta <- utils::modifyList(
    list(
      created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      r_version = R.version.string,
      statci_version = pkg_ver,
      platform = R.version$platform
    ),
    metadata
  )

  structure(
    list(
      title = as.character(title)[1L],
      sections = sections,
      components = list(
        lint = lint,
        estimator = estimator,
        sbc = sbc,
        parallel_rng = parallel_rng,
        manuscript = manuscript,
        sensitivity = sensitivity,
        power_curve = power_curve,
        multiverse = multiverse
      ),
      metadata = meta,
      status = overall
    ),
    class = "statci_report"
  )
}

#' Enforce CI quality gates on a `statci_report`
#'
#' Signals an error (causing a non-zero exit status in CI pipelines) when the
#' audit report fails to meet the requested gate level.
#'
#' @param report A `"statci_report"` (or any `statci_*` audit object with a
#'   `$status` field).
#' @param fail_on Gate threshold: `"fail"` (default; errors only on `"fail"`)
#'   or `"warn"` (errors on either `"warn"` or `"fail"`).
#'
#' @return Invisibly returns `TRUE` when the gate passes; otherwise aborts with
#'   an informative error message.
#' @export
#'
#' @examples
#' rep <- statci_report(lint = stat_lint_text("set.seed(1)\nx <- rnorm(5)\n"))
#' statci_check_gate(rep)
statci_check_gate <- function(report, fail_on = c("fail", "warn")) {
  fail_on <- match.arg(fail_on)
  st <- report$status
  if (is.null(st)) {
    rlang::abort("`report` must be a `statci_report` or `statci_*` audit object with a `$status` field.")
  }
  breached <- (fail_on == "fail" && st == "fail") ||
    (fail_on == "warn" && st %in% c("warn", "fail"))

  if (breached) {
    detail <- ""
    if (inherits(report, "statci_report") && nrow(report$sections) > 0L) {
      bad <- report$sections[report$sections$status %in% c(if (fail_on == "warn") "warn", "fail"), , drop = FALSE]
      detail <- paste0(
        "\nFailing sections:\n",
        paste0("  - ", bad$section, " [", toupper(bad$status), "]: ", bad$detail, collapse = "\n")
      )
    }
    rlang::abort(paste0("statci CI gate failed (overall status: ", toupper(st), ", fail_on = '", fail_on, "').", detail))
  }
  invisible(TRUE)
}

#' Render a `statci_report` to standalone HTML, GitHub-Flavored Markdown, or JSON
#'
#' @param report A `"statci_report"` object created by [statci_report()].
#' @param output_file Optional output file path. When `NULL`, returns the
#'   rendered document as a character scalar without writing to disk.
#' @param format Output format: `"html"` (standalone HTML5 dashboard),
#'   `"markdown"` (GitHub-Flavored Markdown suitable for `$GITHUB_STEP_SUMMARY`),
#'   or `"json"` (machine-readable JSON; requires `jsonlite`).
#' @param title Optional override for the report title.
#' @param open Logical; if `TRUE`, `interactive()`, and `output_file` is
#'   provided, opens the rendered report in the default browser.
#'
#' @return If `output_file` is `NULL`, returns the rendered character scalar;
#'   otherwise writes the file and returns `output_file` invisibly.
#' @export
#'
#' @examples
#' rep <- statci_report(lint = stat_lint_text("x <- rnorm(10)\n"))
#' md <- render_statci_report(rep, format = "markdown")
#' cat(md)
render_statci_report <- function(report,
                                 output_file = NULL,
                                 format = c("html", "markdown", "json"),
                                 title = NULL,
                                 open = FALSE) {
  if (!inherits(report, "statci_report")) {
    rlang::abort("`report` must be a `statci_report` object created with `statci_report()`.")
  }
  format <- match.arg(format)
  rep_title <- title %||% report$title

  rendered <- switch(
    format,
    html = build_html_report(report, rep_title),
    markdown = build_markdown_report(report, rep_title),
    json = build_json_report(report, rep_title)
  )

  if (!is.null(output_file)) {
    dir_out <- dirname(output_file)
    if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
    writeLines(rendered, con = output_file, useBytes = TRUE)
    if (isTRUE(open) && interactive()) {
      utils::browseURL(output_file)
    }
    return(invisible(output_file))
  }

  rendered
}

#' @export
as_tibble.statci_report <- function(x, ...) {
  x$sections
}

#' @export
print.statci_report <- function(x, ...) {
  badge <- format_status_badge(x$status)
  cli::cli_text("{badge} {.strong {x$title}}")
  if (nrow(x$sections) == 0L) {
    cli::cli_text("  (No verification sections included)")
    return(invisible(x))
  }
  for (i in seq_len(nrow(x$sections))) {
    r <- x$sections[i, ]
    b <- format_status_badge(r$status)
    cli::cli_text("  {b} {.strong {r$section}}: {r$detail}")
  }
  invisible(x)
}

# ---------------------------------------------------------------------------
# Markdown Renderer (GFM)
# ---------------------------------------------------------------------------

md_badge <- function(status) {
  switch(
    tolower(status),
    pass = "**PASS**",
    warn = "**WARN**",
    fail = "**FAIL**",
    toupper(status)
  )
}

df_to_md_table <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return("_No rows._")
  hdr <- paste0("| ", paste(md_escape_cell(names(df)), collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |")
  rows <- vapply(seq_len(nrow(df)), function(i) {
    paste0("| ", paste(md_escape_cell(unlist(df[i, ], use.names = FALSE)), collapse = " | "), " |")
  }, character(1L))
  paste(c(hdr, sep, rows), collapse = "\n")
}

build_markdown_report <- function(report, title) {
  safe_title <- md_escape_cell(title)
  out <- c(
    paste0("# ", safe_title),
    "",
    paste0("**Overall Status:** ", md_badge(report$status),
           " | **Generated:** ", md_escape_cell(report$metadata$created_at),
           " | **statci:** v", md_escape_cell(report$metadata$statci_version)),
    ""
  )

  if (nrow(report$sections) > 0L) {
    sec_df <- data.frame(
      Section = report$sections$section,
      Status = vapply(report$sections$status, md_badge, character(1L)),
      Checks = report$sections$checks,
      Passed = report$sections$passed,
      Warnings = report$sections$warnings,
      Failures = report$sections$failures,
      Summary = report$sections$detail,
      stringsAsFactors = FALSE
    )
    out <- c(out, "## Executive Summary", "", df_to_md_table(sec_df), "")
  }

  comp <- report$components

  if (!is.null(comp$lint)) {
    out <- c(out, "## 1. AST Statistical Linter", "")
    if (nrow(comp$lint$findings) == 0L) {
      out <- c(out, "All checked files passed with zero statistical lint findings.", "")
    } else {
      f <- comp$lint$findings
      tbl <- data.frame(
        File = basename(f$file),
        Line = f$line,
        Chunk = ifelse(is.na(f$chunk), "\u2014", f$chunk),
        Severity = toupper(f$severity),
        Rule = paste0("`", f$rule, "`"),
        Message = f$message,
        stringsAsFactors = FALSE
      )
      out <- c(out, df_to_md_table(tbl), "")
    }
  }

  if (!is.null(comp$estimator)) {
    est <- comp$estimator
    out <- c(out, "## 2. Estimator Calibration & Fuzzing", "")
    cv <- est$convergence
    out <- c(
      out,
      paste0("- **Convergence & Singularity:** ", md_badge(cv$status),
             " (", cv$n_valid, "/", cv$n_total, " valid, fail rate = ", fmt_pct(cv$fail_rate),
             ", singular rate = ", fmt_pct(cv$singular_rate), ")"),
      ""
    )
    if (!is.null(est$recovery) && nrow(est$recovery) > 0L) {
      r <- est$recovery
      tbl <- data.frame(
        Parameter = r$parameter,
        Truth = fmt_num(r$truth, 3L),
        Bias = paste0(fmt_num(r$bias, 4L), " (z=", fmt_num(r$bias_z, 2L), ")"),
        RMSE = fmt_num(r$rmse, 4L),
        `SE Ratio` = fmt_num(r$se_ratio, 3L),
        Coverage = paste0(fmt_pct(r$coverage), " [", fmt_pct(r$coverage_ci_low), ", ", fmt_pct(r$coverage_ci_high), "]"),
        Status = vapply(r$status, md_badge, character(1L)),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      out <- c(out, "### Parameter Recovery", "", df_to_md_table(tbl), "")
    }
    if (!is.null(est$type1) && nrow(est$type1) > 0L) {
      t1 <- est$type1
      tbl <- data.frame(
        Parameter = t1$parameter,
        Nominal = fmt_pct(t1$nominal_alpha),
        Empirical = fmt_pct(t1$empirical_alpha),
        `95% Exact CI` = paste0("[", fmt_pct(t1$ci_low), ", ", fmt_pct(t1$ci_high), "]"),
        `Binom p` = fmt_num(t1$binom_p, 4L),
        Status = vapply(t1$status, md_badge, character(1L)),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      out <- c(out, "### Type-I Error Calibration", "", df_to_md_table(tbl), "")
    }
  }

  sbc_obj <- comp$sbc %||% if (!is.null(comp$estimator)) comp$estimator$sbc else NULL
  if (!is.null(sbc_obj)) {
    s <- sbc_obj$summary
    tbl <- data.frame(
      Parameter = s$parameter,
      `Chi2 p` = fmt_num(s$chisq_p, 4L),
      `KS p` = fmt_num(s$ks_p, 4L),
      `50% Cov` = fmt_pct(s$cov_50),
      `90% Cov` = fmt_pct(s$cov_90),
      Status = vapply(s$status, md_badge, character(1L)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    out <- c(out, "## Simulation-Based Calibration (SBC)", "", df_to_md_table(tbl), "")
  }

  par_obj <- comp$parallel_rng %||% if (!is.null(comp$estimator)) comp$estimator$parallel_rng else NULL
  if (!is.null(par_obj)) {
    p <- par_obj$comparisons
    tbl <- data.frame(
      `Ref Cores` = p$ref_cores,
      `Test Cores` = p$test_cores,
      Identical = p$identical,
      `Max |Diff|` = fmt_num(p$max_abs_diff, 6L),
      Status = vapply(p$status, md_badge, character(1L)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    out <- c(out, "## Parallel RNG Invariance", "", df_to_md_table(tbl), "")
  }

  if (!is.null(comp$manuscript)) {
    m <- comp$manuscript
    out <- c(out, "## 3. Manuscript-to-Code Consistency", "")
    if (nrow(m$checks) == 0L && (is.null(m$internal_checks) || nrow(m$internal_checks) == 0L)) {
      out <- c(out, "No numerical statistical claims detected in manuscript prose.", "")
    } else {
      if (nrow(m$checks) > 0L) {
        c_df <- m$checks
        tbl <- data.frame(
          Line = c_df$line,
          Model = c_df$model,
          Term = ifelse(is.na(c_df$term), "\u2014", c_df$term),
          Metric = c_df$metric,
          Reported = paste(c_df$op, c_df$reported_raw),
          Actual = fmt_num(c_df$actual, 4L),
          Issue = c_df$issue_type,
          Status = vapply(c_df$status, md_badge, character(1L)),
          stringsAsFactors = FALSE
        )
        out <- c(out, df_to_md_table(tbl), "")
      }
      if (!is.null(m$internal_checks) && nrow(m$internal_checks) > 0L) {
        ic <- m$internal_checks
        ic_tbl <- data.frame(
          Line = ic$line,
          Check = ic$check_type,
          Reported = ic$reported_expr,
          Expected = ic$expected_range,
          Detail = ic$detail,
          Status = vapply(ic$status, md_badge, character(1L)),
          stringsAsFactors = FALSE
        )
        out <- c(out, "### Internal Arithmetic Checks", "", df_to_md_table(ic_tbl), "")
      }
    }
  }

  if (!is.null(comp$sensitivity)) {
    s <- comp$sensitivity
    out <- c(out, "## 4. Adversarial Sensitivity Battery", "")
    r <- s$results
    tbl <- data.frame(
      Category = r$category,
      Scenario = r$scenario,
      Parameter = r$parameter,
      Baseline = fmt_num(r$baseline, 4L),
      Perturbed = fmt_num(r$perturbed, 4L),
      `% Change` = fmt_pct(r$pct_change),
      `Sign Flip` = ifelse(is.na(r$sign_flip), "\u2014", ifelse(r$sign_flip, "YES", "no")),
      Status = vapply(r$status, md_badge, character(1L)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    out <- c(out, df_to_md_table(tbl), "")
  }

  if (!is.null(comp$power_curve)) {
    pc <- comp$power_curve
    c_df <- pc$curve
    tbl <- data.frame(
      Effect = fmt_num(c_df$effect, 3L),
      `Valid Reps` = c_df$n_valid,
      Power = fmt_pct(c_df$power),
      `95% CI` = paste0("[", fmt_pct(c_df$ci_low), ", ", fmt_pct(c_df$ci_high), "]"),
      MCSE = fmt_num(c_df$mcse, 3L),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    out <- c(out, "## 5. Power-Curve Audit", "", df_to_md_table(tbl), "")
  }

  if (!is.null(comp$multiverse)) {
    mv <- comp$multiverse
    ms <- mv$summary
    safe_param <- gsub("`", "'", md_escape_cell(ms$parameter), fixed = TRUE)
    out <- c(
      out,
      "## 6. Multiverse Specification Curve",
      "",
      paste0(
        "- **Parameter:** `", safe_param, "` (", ms$n_valid, "/", ms$n_specs, " specs) | ",
        "**Median:** ", fmt_num(ms$median_est, 4L), " | **IQR:** ", fmt_num(ms$iqr_est, 4L),
        " | **Sign Consistency:** ", fmt_pct(ms$sign_consistency),
        " | **Significant:** ", fmt_pct(ms$prop_significant)
      ),
      ""
    )
    if (nrow(mv$fork_sensitivity) > 0L) {
      fk <- mv$fork_sensitivity
      tbl <- data.frame(
        Decision = fk$decision,
        Levels = fk$n_levels,
        `Median Range` = fmt_num(fk$median_range, 4L),
        `Variance Explained` = fmt_pct(fk$eta_squared),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      out <- c(out, df_to_md_table(tbl), "")
    }
  }

  paste(out, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Standalone HTML5 Renderer (Safely Escaped via htmltools)
# ---------------------------------------------------------------------------

html_esc <- function(x) {
  html_escape_strict(x)
}

html_pill <- function(status) {
  st <- tolower(as.character(status))
  cls <- switch(
    st,
    pass = "pill-pass",
    warn = "pill-warn",
    fail = "pill-fail",
    error = "pill-fail",
    warning = "pill-warn",
    info = "pill-info",
    "pill-info"
  )
  paste0('<span class="pill ', cls, '">', html_esc(toupper(st)), "</span>")
}

df_to_html_table <- function(df, raw_cols = character(0)) {
  if (is.null(df) || nrow(df) == 0L) {
    return('<p class="muted">No items to report.</p>')
  }
  ths <- paste0("<th>", html_esc(names(df)), "</th>", collapse = "")
  trs <- vapply(seq_len(nrow(df)), function(i) {
    tds <- vapply(seq_along(df), function(j) {
      nm <- names(df)[j]
      val <- df[[j]][i]
      cell <- if (nm %in% raw_cols) as.character(val) else html_esc(val)
      paste0("<td>", cell, "</td>")
    }, character(1L))
    paste0("<tr>", paste(tds, collapse = ""), "</tr>")
  }, character(1L))
  paste0(
    '<div class="table-wrap"><table><thead><tr>', ths,
    "</tr></thead><tbody>", paste(trs, collapse = ""), "</tbody></table></div>"
  )
}

build_html_report <- function(report, title) {
  comp <- report$components
  sec <- report$sections

  css <- paste(
    ":root{--bg:#f8fafc;--card:#ffffff;--ink:#0f172a;--muted:#475569;--border:#e2e8f0;",
    "--pass:#15803d;--pass-bg:#dcfce7;--warn:#b45309;--warn-bg:#fef3c7;",
    "--fail:#b91c1c;--fail-bg:#fee2e2;--info:#1d4ed8;--info-bg:#dbeafe;}",
    "*{box-sizing:border-box;}",
    "body{margin:0;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;",
    "background:var(--bg);color:var(--ink);line-height:1.5;}",
    "header{background:linear-gradient(135deg,#0f172a 0%,#1e293b 100%);color:#fff;padding:28px 32px;}",
    "header h1{margin:0 0 8px 0;font-size:1.55rem;font-weight:700;letter-spacing:-0.01em;}",
    ".meta{color:#cbd5e1;font-size:0.86rem;display:flex;flex-wrap:wrap;gap:16px;align-items:center;}",
    "main{max-width:1180px;margin:24px auto 56px;padding:0 20px;}",
    ".kpi-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(210px,1fr));gap:14px;margin-bottom:24px;}",
    ".kpi{background:var(--card);border:1px solid var(--border);border-radius:10px;padding:16px 18px;",
    "box-shadow:0 1px 2px rgba(15,23,42,0.04);}",
    ".kpi .label{font-size:0.78rem;text-transform:uppercase;letter-spacing:0.04em;color:var(--muted);}",
    ".kpi .val{font-size:1.45rem;font-weight:700;margin-top:4px;}",
    "section{background:var(--card);border:1px solid var(--border);border-radius:10px;padding:20px 22px;",
    "margin-bottom:20px;box-shadow:0 1px 2px rgba(15,23,42,0.04);}",
    "section h2{margin:0 0 12px 0;font-size:1.15rem;display:flex;align-items:center;gap:10px;}",
    "section h3{margin:16px 0 8px 0;font-size:0.98rem;color:#334155;}",
    ".table-wrap{overflow-x:auto;}",
    "table{width:100%;border-collapse:collapse;font-size:0.88rem;}",
    "th,td{text-align:left;padding:9px 10px;border-bottom:1px solid var(--border);vertical-align:top;}",
    "th{background:#f1f5f9;font-weight:600;color:#334155;white-space:nowrap;}",
    "tr:hover td{background:#f8fafc;}",
    "code{font-family:ui-monospace,SFMono-Regular,Menlo,Monaco,Consolas,monospace;font-size:0.83rem;",
    "background:#f1f5f9;padding:1px 5px;border-radius:4px;}",
    ".pill{display:inline-block;padding:2px 9px;border-radius:999px;font-size:0.74rem;font-weight:700;letter-spacing:0.03em;}",
    ".pill-pass{background:var(--pass-bg);color:var(--pass);}",
    ".pill-warn{background:var(--warn-bg);color:var(--warn);}",
    ".pill-fail{background:var(--fail-bg);color:var(--fail);}",
    ".pill-info{background:var(--info-bg);color:var(--info);}",
    ".muted{color:var(--muted);font-size:0.9rem;}",
    "footer{text-align:center;color:var(--muted);font-size:0.8rem;padding:18px;}"
  )

  total_checks <- sum(sec$checks)
  total_fail <- sum(sec$failures)
  total_warn <- sum(sec$warnings)

  kpis <- paste0(
    '<div class="kpi-grid">',
    '<div class="kpi"><div class="label">Overall Gate</div><div class="val">', html_pill(report$status), "</div></div>",
    '<div class="kpi"><div class="label">Audit Sections</div><div class="val">', nrow(sec), "</div></div>",
    '<div class="kpi"><div class="label">Total Checks</div><div class="val">', total_checks, "</div></div>",
    '<div class="kpi"><div class="label">Warnings / Failures</div><div class="val">', total_warn, " / ", total_fail, "</div></div>",
    "</div>"
  )

  body_parts <- c(kpis)

  # Executive Summary Table
  if (nrow(sec) > 0L) {
    sec_df <- data.frame(
      Section = sec$section,
      Status = vapply(sec$status, html_pill, character(1L)),
      Checks = sec$checks,
      Passed = sec$passed,
      Warnings = sec$warnings,
      Failures = sec$failures,
      Summary = sec$detail,
      stringsAsFactors = FALSE
    )
    body_parts <- c(
      body_parts,
      paste0("<section><h2>Executive Summary</h2>", df_to_html_table(sec_df, raw_cols = "Status"), "</section>")
    )
  }

  # 1. Lint section
  if (!is.null(comp$lint)) {
    f <- comp$lint$findings
    inner <- if (nrow(f) == 0L) {
      '<p class="muted">Zero statistical lint findings across all scanned files.</p>'
    } else {
      tbl <- data.frame(
        Location = paste0(basename(f$file), ":", f$line, ":", f$col),
        Chunk = ifelse(is.na(f$chunk), "\u2014", f$chunk),
        Severity = vapply(f$severity, html_pill, character(1L)),
        Rule = paste0("<code>", html_esc(f$rule), "</code>"),
        Message = f$message,
        Snippet = paste0("<code>", html_esc(f$code_snippet), "</code>"),
        stringsAsFactors = FALSE
      )
      df_to_html_table(tbl, raw_cols = c("Severity", "Rule", "Snippet"))
    }
    body_parts <- c(
      body_parts,
      paste0("<section><h2>AST Statistical Linter ", html_pill(comp$lint$status), "</h2>", inner, "</section>")
    )
  }

  # 2. Estimator Calibration section
  if (!is.null(comp$estimator)) {
    est <- comp$estimator
    cv <- est$convergence
    parts <- c(
      paste0(
        '<p class="muted"><strong>Convergence &amp; Singularity:</strong> ', html_pill(cv$status),
        " &nbsp; ", cv$n_valid, "/", cv$n_total, " valid reps | fail rate = ", fmt_pct(cv$fail_rate),
        " | singular rate = ", fmt_pct(cv$singular_rate), "</p>"
      )
    )
    if (!is.null(est$recovery) && nrow(est$recovery) > 0L) {
      r <- est$recovery
      tbl <- data.frame(
        Parameter = r$parameter,
        Truth = fmt_num(r$truth, 3L),
        Bias = paste0(fmt_num(r$bias, 4L), " (z=", fmt_num(r$bias_z, 2L), ")"),
        RMSE = fmt_num(r$rmse, 4L),
        `SE Ratio` = fmt_num(r$se_ratio, 3L),
        Coverage = paste0(fmt_pct(r$coverage), " [", fmt_pct(r$coverage_ci_low), ", ", fmt_pct(r$coverage_ci_high), "]"),
        Status = vapply(r$status, html_pill, character(1L)),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      parts <- c(parts, "<h3>Parameter Recovery</h3>", df_to_html_table(tbl, raw_cols = "Status"))
    }
    if (!is.null(est$type1) && nrow(est$type1) > 0L) {
      t1 <- est$type1
      tbl <- data.frame(
        Parameter = t1$parameter,
        Nominal = fmt_pct(t1$nominal_alpha),
        Empirical = fmt_pct(t1$empirical_alpha),
        `95% Clopper-Pearson CI` = paste0("[", fmt_pct(t1$ci_low), ", ", fmt_pct(t1$ci_high), "]"),
        `Binomial p` = fmt_num(t1$binom_p, 4L),
        Status = vapply(t1$status, html_pill, character(1L)),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      parts <- c(parts, "<h3>Type-I Error Calibration</h3>", df_to_html_table(tbl, raw_cols = "Status"))
    }
    body_parts <- c(
      body_parts,
      paste0("<section><h2>Estimator Calibration &amp; Fuzzing ", html_pill(est$status), "</h2>", paste(parts, collapse = ""), "</section>")
    )
  }

  # SBC section
  sbc_obj <- comp$sbc %||% if (!is.null(comp$estimator)) comp$estimator$sbc else NULL
  if (!is.null(sbc_obj)) {
    s <- sbc_obj$summary
    tbl <- data.frame(
      Parameter = s$parameter,
      `Valid Reps` = s$n_valid,
      `Chi-Square p` = fmt_num(s$chisq_p, 4L),
      `KS p` = fmt_num(s$ks_p, 4L),
      `50% Coverage` = fmt_pct(s$cov_50),
      `90% Coverage` = fmt_pct(s$cov_90),
      Status = vapply(s$status, html_pill, character(1L)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    body_parts <- c(
      body_parts,
      paste0("<section><h2>Simulation-Based Calibration (SBC) ", html_pill(sbc_obj$status), "</h2>",
             df_to_html_table(tbl, raw_cols = "Status"), "</section>")
    )
  }

  # Parallel RNG section
  par_obj <- comp$parallel_rng %||% if (!is.null(comp$estimator)) comp$estimator$parallel_rng else NULL
  if (!is.null(par_obj)) {
    p <- par_obj$comparisons
    tbl <- data.frame(
      `Reference Cores` = p$ref_cores,
      `Test Cores` = p$test_cores,
      Identical = ifelse(p$identical, "Yes", "No"),
      `Max |Diff|` = fmt_num(p$max_abs_diff, 6L),
      Status = vapply(p$status, html_pill, character(1L)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    body_parts <- c(
      body_parts,
      paste0("<section><h2>Parallel RNG Invariance ", html_pill(par_obj$status), "</h2>",
             df_to_html_table(tbl, raw_cols = "Status"), "</section>")
    )
  }

  # 3. Manuscript section
  if (!is.null(comp$manuscript)) {
    m <- comp$manuscript
    has_checks <- nrow(m$checks) > 0L
    has_ic <- !is.null(m$internal_checks) && nrow(m$internal_checks) > 0L
    inner <- if (!has_checks && !has_ic) {
      '<p class="muted">No numerical statistical claims detected in manuscript prose.</p>'
    } else {
      parts <- character(0)
      if (has_checks) {
        c_df <- m$checks
        tbl <- data.frame(
          Line = c_df$line,
          Model = c_df$model,
          Term = ifelse(is.na(c_df$term), "\u2014", c_df$term),
          Metric = c_df$metric,
          Reported = paste(c_df$op, c_df$reported_raw),
          Actual = fmt_num(c_df$actual, 4L),
          Issue = paste0("<code>", html_esc(c_df$issue_type), "</code>"),
          Status = vapply(c_df$status, html_pill, character(1L)),
          stringsAsFactors = FALSE
        )
        parts <- c(parts, df_to_html_table(tbl, raw_cols = c("Issue", "Status")))
      }
      if (has_ic) {
        ic <- m$internal_checks
        ic_tbl <- data.frame(
          Line = ic$line,
          Check = paste0("<code>", html_esc(ic$check_type), "</code>"),
          Reported = ic$reported_expr,
          Expected = ic$expected_range,
          Detail = ic$detail,
          Status = vapply(ic$status, html_pill, character(1L)),
          stringsAsFactors = FALSE
        )
        parts <- c(parts, "<h3>Internal Arithmetic Checks</h3>",
                   df_to_html_table(ic_tbl, raw_cols = c("Check", "Status")))
      }
      paste(parts, collapse = "")
    }
    body_parts <- c(
      body_parts,
      paste0("<section><h2>Manuscript-to-Code Consistency ", html_pill(m$status), "</h2>", inner, "</section>")
    )
  }

  # 4. Sensitivity section
  if (!is.null(comp$sensitivity)) {
    s <- comp$sensitivity
    r <- s$results
    tbl <- data.frame(
      Category = r$category,
      Scenario = r$scenario,
      Parameter = r$parameter,
      Baseline = fmt_num(r$baseline, 4L),
      Perturbed = fmt_num(r$perturbed, 4L),
      `% Change` = fmt_pct(r$pct_change),
      `Sign Flip` = ifelse(is.na(r$sign_flip), "\u2014", ifelse(r$sign_flip, "YES", "no")),
      Detail = r$detail,
      Status = vapply(r$status, html_pill, character(1L)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    body_parts <- c(
      body_parts,
      paste0("<section><h2>Adversarial Sensitivity Battery ", html_pill(s$status), "</h2>",
             df_to_html_table(tbl, raw_cols = "Status"), "</section>")
    )
  }

  # 5. Power-Curve section
  if (!is.null(comp$power_curve)) {
    pc <- comp$power_curve
    c_df <- pc$curve
    tbl <- data.frame(
      Effect = fmt_num(c_df$effect, 3L),
      `Valid Reps` = c_df$n_valid,
      Power = fmt_pct(c_df$power),
      `95% Clopper-Pearson CI` = paste0("[", fmt_pct(c_df$ci_low), ", ", fmt_pct(c_df$ci_high), "]"),
      MCSE = fmt_num(c_df$mcse, 3L),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    body_parts <- c(
      body_parts,
      paste0("<section><h2>Power-Curve Audit ", html_pill(pc$status), "</h2>",
             df_to_html_table(tbl), "</section>")
    )
  }

  # 6. Multiverse section
  if (!is.null(comp$multiverse)) {
    mv <- comp$multiverse
    ms <- mv$summary
    summary_p <- paste0(
      '<p class="muted"><strong>Parameter:</strong> <code>', html_esc(ms$parameter), "</code> (",
      ms$n_valid, "/", ms$n_specs, " specs) | <strong>Median:</strong> ", fmt_num(ms$median_est, 4L),
      " | <strong>IQR:</strong> ", fmt_num(ms$iqr_est, 4L),
      " | <strong>Sign Consistency:</strong> ", fmt_pct(ms$sign_consistency),
      " | <strong>Significant:</strong> ", fmt_pct(ms$prop_significant), "</p>"
    )
    fk <- mv$fork_sensitivity
    tbl <- data.frame(
      Decision = fk$decision,
      Levels = fk$n_levels,
      `Median Range` = fmt_num(fk$median_range, 4L),
      `Variance Explained` = fmt_pct(fk$eta_squared),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    body_parts <- c(
      body_parts,
      paste0("<section><h2>Multiverse Specification Curve ", html_pill(mv$status), "</h2>",
             summary_p, df_to_html_table(tbl), "</section>")
    )
  }

  paste0(
    "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"UTF-8\">",
    '<meta name="viewport" content="width=device-width,initial-scale=1">',
    '<meta http-equiv="Content-Security-Policy" content="default-src \'none\'; style-src \'unsafe-inline\';">',
    "<title>", html_esc(title), "</title><style>", css, "</style></head><body>",
    "<header><h1>", html_esc(title), "</h1>",
    '<div class="meta">',
    "<span>Status: ", html_pill(report$status), "</span>",
    "<span>Generated: ", html_esc(report$metadata$created_at), "</span>",
    "<span>", html_esc(report$metadata$r_version), "</span>",
    "<span>statci v", html_esc(report$metadata$statci_version), "</span>",
    "</div></header><main>",
    paste(body_parts, collapse = "\n"),
    "</main><footer>Generated by <strong>statci</strong> &mdash; Automated Statistical CI/CD for R</footer>",
    "</body></html>"
  )
}

# ---------------------------------------------------------------------------
# JSON Renderer
# ---------------------------------------------------------------------------

build_json_report <- function(report, title) {
  rlang::check_installed("jsonlite", reason = "to render statci reports as JSON.")
  payload <- list(
    title = title,
    status = report$status,
    metadata = report$metadata,
    sections = report$sections,
    lint = if (!is.null(report$components$lint)) report$components$lint$findings else NULL,
    estimator_recovery = if (!is.null(report$components$estimator)) report$components$estimator$recovery else NULL,
    estimator_type1 = if (!is.null(report$components$estimator)) report$components$estimator$type1 else NULL,
    estimator_convergence = if (!is.null(report$components$estimator)) report$components$estimator$convergence else NULL,
    manuscript = if (!is.null(report$components$manuscript)) report$components$manuscript$checks else NULL,
    manuscript_internal = if (!is.null(report$components$manuscript)) report$components$manuscript$internal_checks else NULL,
    sensitivity = if (!is.null(report$components$sensitivity)) report$components$sensitivity$results else NULL,
    power_curve = if (!is.null(report$components$power_curve)) report$components$power_curve$curve else NULL,
    multiverse = if (!is.null(report$components$multiverse)) report$components$multiverse$specifications else NULL
  )
  as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, na = "null"))
}
