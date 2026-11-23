# Public AST-based statistical linter engine

#' Lint an R script or Quarto/RMarkdown document for statistical hazards
#'
#' Parses `.R`, `.qmd`, or `.Rmd` source code into an abstract syntax tree (AST)
#' and evaluates registered statistical and methodological lint rules.
#'
#' @param path Path to an `.R`, `.r`, `.qmd`, or `.Rmd` file.
#' @param rules Optional subset of rules to run: a character vector of rule IDs,
#'   a list of `statci_rule` objects, or `NULL` (default) to run all registered
#'   rules.
#' @param exclude_rules Optional character vector of rule IDs to skip.
#' @param skip_eval_false Logical; for `.qmd`/`.Rmd` files, skip chunks with
#'   `eval = FALSE` or `#| eval: false`. Default `FALSE`.
#'
#' @return An S3 object of class `"statci_lint"` containing `findings` (a
#'   [tibble::tibble]), `files`, `rules_run`, `summary`, and `status`
#'   (`"pass"`, `"warn"`, or `"fail"`).
#' @export
#'
#' @examples
#' res <- stat_lint_text("x <- rnorm(100)\n")
#' res
stat_lint <- function(path,
                      rules = NULL,
                      exclude_rules = NULL,
                      skip_eval_false = FALSE) {
  validate_existing_file(path, arg = "path")
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  ext <- tolower(tools::file_ext(path))
  is_rmd <- ext %in% c("qmd", "rmd")
  lint_lines_internal(
    lines = lines,
    file = path,
    is_rmd = is_rmd,
    rules = rules,
    exclude_rules = exclude_rules,
    skip_eval_false = skip_eval_false
  )
}

#' Lint R or Quarto/RMarkdown source code supplied as a string
#'
#' Convenience wrapper around [stat_lint()] for in-memory code strings, unit
#' tests, and programmatic checks.
#'
#' @param text Character scalar or vector of source lines.
#' @param format Source format: `"r"` for R code or `"qmd"` / `"rmd"` for
#'   Quarto/RMarkdown documents.
#' @param rules Optional character vector of rule IDs or list of `statci_rule`
#'   objects. Default `NULL` runs all registered rules.
#' @param exclude_rules Optional character vector of rule IDs to skip.
#' @param skip_eval_false Logical; for `"qmd"`/`"rmd"`, skip `eval = FALSE`
#'   chunks. Default `FALSE`.
#'
#' @return An S3 object of class `"statci_lint"`.
#' @export
#'
#' @examples
#' stat_lint_text("set.seed(123)\nx <- rnorm(100)\n")
#' stat_lint_text("x <- rnorm(100)\n")
stat_lint_text <- function(text,
                           format = c("r", "qmd", "rmd"),
                           rules = NULL,
                           exclude_rules = NULL,
                           skip_eval_false = FALSE) {
  format <- match.arg(format)
  if (!is.character(text)) {
    rlang::abort("`text` must be a character vector.")
  }
  lines <- unlist(strsplit(paste(text, collapse = "\n"), "\n", fixed = TRUE))
  if (length(lines) == 0L) lines <- ""
  lint_lines_internal(
    lines = lines,
    file = paste0("<text:", format, ">"),
    is_rmd = format %in% c("qmd", "rmd"),
    rules = rules,
    exclude_rules = exclude_rules,
    skip_eval_false = skip_eval_false
  )
}

#' Lint all R, Quarto, and RMarkdown files in one or more directories
#'
#' Recursively discovers `.R`, `.r`, `.qmd`, and `.Rmd` files under `path` and
#' combines their diagnostics into a single `"statci_lint"` object.
#'
#' @param path Character vector of directory path(s) to scan.
#' @param pattern Regular expression matching files to lint. Defaults to
#'   `"\\.(R|r|qmd|Rmd|rmd)$"`.
#' @param recursive Logical; scan subdirectories recursively? Default `TRUE`.
#' @param exclude_pattern Optional regex of paths to skip (e.g., `"renv/|packrat/"`).
#' @param rules Optional subset of rules to run.
#' @param exclude_rules Optional character vector of rule IDs to skip.
#' @param skip_eval_false Logical; skip `eval = FALSE` chunks in `.qmd`/`.Rmd`.
#'
#' @return An S3 object of class `"statci_lint"`.
#' @export
#'
#' @examples
#' tmp <- tempfile("statci_dir_")
#' dir.create(tmp)
#' writeLines("set.seed(1)\nx <- rnorm(10)", file.path(tmp, "ok.R"))
#' stat_lint_dir(tmp)
#' unlink(tmp, recursive = TRUE)
stat_lint_dir <- function(path = ".",
                          pattern = "\\.(R|r|qmd|Rmd|rmd)$",
                          recursive = TRUE,
                          exclude_pattern = "(^|/)(\\.git|renv|packrat|\\.Rproj\\.user)/",
                          rules = NULL,
                          exclude_rules = NULL,
                          skip_eval_false = FALSE) {
  if (!is.character(path) || length(path) < 1L) {
    rlang::abort("`path` must be a non-empty character vector of directories.")
  }
  for (p in path) validate_existing_dir(p, arg = "path")
  files <- unique(unlist(lapply(path, function(p) {
    found <- list.files(p, pattern = pattern, recursive = recursive, full.names = TRUE)
    found[vapply(found, is_safe_contained_file, logical(1L), root_dir = p)]
  })))
  if (!is.null(exclude_pattern) && nzchar(exclude_pattern) && length(files) > 0L) {
    files <- files[!grepl(exclude_pattern, files)]
  }
  files <- sort(files)

  active_rules <- resolve_lint_rules(rules, exclude_rules)
  all_rows <- list()
  for (f in files) {
    res <- stat_lint(
      f,
      rules = active_rules,
      exclude_rules = NULL,
      skip_eval_false = skip_eval_false
    )
    if (nrow(res$findings) > 0L) {
      all_rows[[length(all_rows) + 1L]] <- res$findings
    }
  }

  findings <- if (length(all_rows) > 0L) {
    do.call(rbind, all_rows)
  } else {
    empty_findings_tibble()
  }
  build_lint_object(
    findings = findings,
    files = files,
    rules_run = vapply(active_rules, `[[`, character(1L), "id")
  )
}

resolve_lint_rules <- function(rules = NULL, exclude_rules = NULL) {
  ensure_default_rules()
  if (is.null(rules)) {
    ids <- list_stat_rules()$id
    selected <- lapply(ids, function(id) .statci_rule_registry[[id]])
  } else if (is.character(rules)) {
    missing_ids <- setdiff(rules, ls(envir = .statci_rule_registry))
    if (length(missing_ids) > 0L) {
      rlang::abort(paste0("Unknown lint rule(s): ", paste(missing_ids, collapse = ", ")))
    }
    selected <- lapply(rules, function(id) .statci_rule_registry[[id]])
  } else if (inherits(rules, "statci_rule")) {
    selected <- list(rules)
  } else if (is.list(rules) && all(vapply(rules, inherits, logical(1L), "statci_rule"))) {
    selected <- rules
  } else {
    rlang::abort("`rules` must be NULL, a character vector of rule IDs, a `statci_rule`, or a list of `statci_rule` objects.")
  }

  if (!is.null(exclude_rules) && length(exclude_rules) > 0L) {
    keep <- !vapply(selected, function(r) r$id %in% exclude_rules, logical(1L))
    selected <- selected[keep]
  }
  selected
}

empty_findings_tibble <- function() {
  tibble::tibble(
    file = character(0),
    line = integer(0),
    col = integer(0),
    chunk = character(0),
    rule = character(0),
    severity = character(0),
    category = character(0),
    message = character(0),
    recommendation = character(0),
    code_snippet = character(0)
  )
}

lint_lines_internal <- function(lines, file, is_rmd, rules, exclude_rules, skip_eval_false) {
  active_rules <- resolve_lint_rules(rules, exclude_rules)
  ctx <- build_parse_context(
    lines = lines,
    file = file,
    is_rmd = is_rmd,
    skip_eval_false = skip_eval_false
  )

  rows <- list()

  # Report any syntax parse errors first
  for (pe in ctx$parse_errors) {
    if (!is_suppressed(ctx$suppressions, pe$line, "parse_error")) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        file = file,
        line = as.integer(pe$line),
        col = as.integer(pe$col),
        chunk = as.character(pe$chunk %||% NA_character_),
        rule = "parse_error",
        severity = "error",
        category = "syntax",
        message = as.character(pe$message),
        recommendation = "Fix the R syntax error so full AST auditing can run.",
        code_snippet = ctx_snippet(ctx, pe$line)
      )
    }
  }

  if (length(ctx$units) > 0L) {
    for (r in active_rules) {
      raw_hits <- r$check(ctx)
      if (is.null(raw_hits) || length(raw_hits) == 0L) next
      if (is.data.frame(raw_hits)) {
        raw_hits <- lapply(seq_len(nrow(raw_hits)), function(i) as.list(raw_hits[i, , drop = FALSE]))
      }
      for (h in raw_hits) {
        ln <- as.integer(h$line %||% 1L)
        if (is_suppressed(ctx$suppressions, ln, r$id)) next
        cl <- as.integer(h$col %||% 1L)
        ch <- if (!is.null(h$chunk)) {
          as.character(h$chunk)
        } else if (!is.na(ln) && ln >= 1L && ln <= length(ctx$line_to_chunk)) {
          ctx$line_to_chunk[ln]
        } else {
          NA_character_
        }
        snip <- h$code_snippet %||% ctx_snippet(ctx, ln)
        rows[[length(rows) + 1L]] <- tibble::tibble(
          file = file,
          line = ln,
          col = cl,
          chunk = ch,
          rule = r$id,
          severity = r$severity,
          category = r$category,
          message = as.character(h$message),
          recommendation = r$recommendation,
          code_snippet = as.character(snip)
        )
      }
    }
  }

  findings <- if (length(rows) > 0L) {
    tbl <- do.call(rbind, rows)
    ord <- order(tbl$file, tbl$line, tbl$col, tbl$rule)
    tbl[ord, , drop = FALSE]
  } else {
    empty_findings_tibble()
  }

  build_lint_object(
    findings = findings,
    files = file,
    rules_run = vapply(active_rules, `[[`, character(1L), "id")
  )
}

build_lint_object <- function(findings, files, rules_run) {
  n_err <- sum(findings$severity == "error")
  n_warn <- sum(findings$severity == "warning")
  n_info <- sum(findings$severity == "info")
  status <- lint_severity_to_status(findings$severity)

  structure(
    list(
      findings = tibble::as_tibble(findings),
      files = files,
      rules_run = rules_run,
      summary = list(
        n_files = length(files),
        n_rules = length(rules_run),
        n_findings = nrow(findings),
        n_error = as.integer(n_err),
        n_warning = as.integer(n_warn),
        n_info = as.integer(n_info)
      ),
      status = status
    ),
    class = "statci_lint"
  )
}

#' @export
as_tibble.statci_lint <- function(x, ...) {
  x$findings
}

#' @export
print.statci_lint <- function(x, max_rows = 20L, ...) {
  badge <- format_status_badge(x$status)
  s <- x$summary
  cli::cli_text(
    "{badge} {.strong statci lint}: {s$n_files} file(s), {s$n_rules} rule(s) | ",
    "{s$n_error} error(s), {s$n_warning} warning(s), {s$n_info} info"
  )
  if (nrow(x$findings) > 0L) {
    show_n <- min(nrow(x$findings), as.integer(max_rows))
    for (i in seq_len(show_n)) {
      row <- x$findings[i, ]
      sev_tag <- switch(
        row$severity,
        error = cli::col_red("ERROR"),
        warning = cli::col_yellow("WARN "),
        info = cli::col_blue("INFO "),
        toupper(row$severity)
      )
      loc <- paste0(basename(row$file), ":", row$line, ":", row$col)
      if (!is.na(row$chunk) && nzchar(row$chunk)) {
        loc <- paste0(loc, " [", row$chunk, "]")
      }
      cli::cli_text("  {sev_tag} {loc} ({row$rule}): {row$message}")
    }
    if (nrow(x$findings) > show_n) {
      cli::cli_text("  ... and {nrow(x$findings) - show_n} more finding(s). Use `as_tibble()` to view all.")
    }
  }
  invisible(x)
}
