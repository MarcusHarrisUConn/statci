# Git-Diff-Aware Incremental PR Linting

#' Parse unified diff lines into per-file added/modified 1-based line numbers
#' @param diff_lines Character vector of unified diff lines.
#' @return Named list mapping relative file path to integer vector of changed
#'   1-based line numbers in the target file.
#' @noRd
parse_unified_diff <- function(diff_lines) {
  changed <- list()
  cur_file <- NULL
  cur_new_line <- NA_integer_
  in_hunk <- FALSE

  for (ln in diff_lines) {
    # New target file header: `+++ b/R/foo.R` or `+++ R/foo.R`
    if (startsWith(ln, "+++ ")) {
      raw_path <- trimws(substring(ln, 5L))
      # Strip optional tab-separated timestamp from plain `diff -u`
      raw_path <- sub("\t.*$", "", raw_path)
      if (identical(raw_path, "/dev/null")) {
        cur_file <- NULL
        in_hunk <- FALSE
        next
      }
      if (startsWith(raw_path, "b/")) {
        raw_path <- substring(raw_path, 3L)
      }
      cur_file <- raw_path
      in_hunk <- FALSE
      if (is.null(changed[[cur_file]])) {
        changed[[cur_file]] <- integer(0)
      }
      next
    }

    if (startsWith(ln, "--- ") || startsWith(ln, "diff --git ")) {
      in_hunk <- FALSE
      next
    }

    # Hunk header: `@@ -a,b +c,d @@`
    if (startsWith(ln, "@@ ") && !is.null(cur_file)) {
      m <- regexec("^@@ -[0-9]+(?:,[0-9]+)? \\+([0-9]+)(?:,([0-9]+))? @@", ln)
      cap <- regmatches(ln, m)[[1L]]
      if (length(cap) >= 2L) {
        start_new <- suppressWarnings(as.integer(cap[2L]))
        count_new <- if (length(cap) >= 3L && nzchar(cap[3L])) suppressWarnings(as.integer(cap[3L])) else 1L
        if (!is.na(start_new) && !is.na(count_new) && count_new > 0L) {
          cur_new_line <- start_new
          in_hunk <- TRUE
        } else {
          in_hunk <- FALSE
        }
      }
      next
    }

    if (!in_hunk || is.null(cur_file) || is.na(cur_new_line)) next

    if (startsWith(ln, "+")) {
      changed[[cur_file]] <- c(changed[[cur_file]], cur_new_line)
      cur_new_line <- cur_new_line + 1L
    } else if (startsWith(ln, " ")) {
      cur_new_line <- cur_new_line + 1L
    } else if (startsWith(ln, "-") || startsWith(ln, "\\")) {
      # Deleted line or "\ No newline at end of file" — does not advance new line counter
      next
    } else {
      in_hunk <- FALSE
    }
  }

  # Deduplicate, sort, and drop files with zero added lines
  out <- list()
  for (nm in names(changed)) {
    lns <- sort(unique(changed[[nm]]))
    if (length(lns) > 0L) out[[nm]] <- lns
  }
  out
}

#' Incrementally lint only added or modified lines in a Git diff / pull request
#'
#' Enables adopting `statci` in continuous integration on large legacy
#' repositories without failing on untouched historical code. Parses either a
#' supplied unified diff (`diff_text`) or invokes safe read-only `git diff`
#' against a strictly validated `base_ref`, runs full-file AST analysis on each
#' modified `.R`/`.qmd`/`.Rmd` file (preserving cross-line and cross-chunk
#' context such as upstream `set.seed()` calls), and reports **only findings
#' landing on added or modified lines**.
#'
#' All Git refs are strictly validated against option injection (leading `-`,
#' control bytes, shell characters), and all diff file paths are strictly
#' confined inside `path` (blocking `..` path traversal and symlink escapes).
#'
#' @param path Root repository or project directory (default `"."`).
#' @param base_ref Git base reference (e.g., `"HEAD"`, `"main"`, `"HEAD~1"`,
#'   or commit SHA) used when `diff_text` is `NULL` and `staged = FALSE`.
#' @param head_ref Optional Git head reference for two-ref diffs (e.g.,
#'   `base_ref = "main"`, `head_ref = "HEAD"`). When `NULL` (default), diffs
#'   `base_ref` against the current working tree.
#' @param diff_text Optional character scalar or vector containing unified diff
#'   output (`git diff -U0` or `diff -u`). When provided, `git` is not invoked.
#' @param staged Logical; if `TRUE` (and `diff_text` is `NULL`), runs
#'   `git diff --cached` against staged index changes. Default `FALSE`.
#' @param rules Optional subset of lint rules (forwarded to [stat_lint()]).
#' @param exclude_rules Optional character vector of rule IDs to skip.
#' @param skip_eval_false Logical; for `.qmd`/`.Rmd` files, skip `eval = FALSE`
#'   chunks. Default `FALSE`.
#' @param pattern Regular expression matching lintable files (defaults to
#'   `"\\.(R|r|qmd|Rmd|rmd)$"`).
#'
#' @return An S3 object of class `"statci_lint"` (compatible with
#'   [statci_report()], [render_statci_report()], [export_statci_sarif()], and
#'   [statci_check_gate()]) with an additional `$diff_meta` list recording
#'   `changed_files`, `changed_lines`, and `n_suppressed_untouched`.
#' @export
#'
#' @examples
#' tmp <- tempfile("statci_diff_")
#' dir.create(tmp)
#' script <- file.path(tmp, "legacy.R")
#' writeLines(c(
#'   "legacy_x <- rnorm(10)",
#'   "set.seed(42)",
#'   "new_val <- mean(rnorm(5), na.rm = T)"
#' ), script)
#' diff_u <- paste(
#'   "--- a/legacy.R",
#'   "+++ b/legacy.R",
#'   "@@ -2,0 +3,1 @@",
#'   "+new_val <- mean(rnorm(5), na.rm = T)",
#'   sep = "\n"
#' )
#' # Line 1 (legacy unseeded rnorm) is untouched and ignored; only line 3 is flagged!
#' res <- stat_lint_diff(path = tmp, diff_text = diff_u)
#' res
#' unlink(tmp, recursive = TRUE)
stat_lint_diff <- function(path = ".",
                           base_ref = "HEAD",
                           head_ref = NULL,
                           diff_text = NULL,
                           staged = FALSE,
                           rules = NULL,
                           exclude_rules = NULL,
                           skip_eval_false = FALSE,
                           pattern = "\\.(R|r|qmd|Rmd|rmd)$") {
  validate_existing_dir(path, arg = "path")
  root_norm <- normalizePath(path, winslash = "/", mustWork = TRUE)

  if (!is.null(diff_text)) {
    if (!is.character(diff_text)) {
      rlang::abort("`diff_text` must be a character vector.")
    }
    diff_lines <- unlist(strsplit(paste(diff_text, collapse = "\n"), "\n", fixed = TRUE))
  } else {
    git_bin <- Sys.which("git")
    if (!nzchar(git_bin)) {
      rlang::abort("`git` executable not found on PATH; supply `diff_text` directly instead.")
    }
    git_args <- c("-C", shQuote(root_norm), "diff", "--unified=0", "--no-color", "--no-ext-diff")
    if (isTRUE(staged)) {
      git_args <- c(git_args, "--cached")
    } else {
      base_clean <- validate_git_ref(base_ref, arg = "base_ref")
      if (!is.null(head_ref)) {
        head_clean <- validate_git_ref(head_ref, arg = "head_ref")
        git_args <- c(git_args, shQuote(base_clean), shQuote(head_clean))
      } else {
        git_args <- c(git_args, shQuote(base_clean))
      }
    }
    git_args <- c(git_args, "--")
    diff_out <- suppressWarnings(system2(git_bin, args = git_args, stdout = TRUE, stderr = TRUE))
    exit_code <- attr(diff_out, "status") %||% 0L
    if (!identical(as.integer(exit_code), 0L)) {
      rlang::abort(paste0("`git diff` failed: ", paste(diff_out, collapse = "\n")))
    }
    diff_lines <- diff_out
  }

  changed_map <- parse_unified_diff(diff_lines)
  active_rules <- resolve_lint_rules(rules, exclude_rules)
  rule_ids <- vapply(active_rules, `[[`, character(1L), "id")

  # Validate path confinement & filter to lintable existing files
  linted_files <- character(0)
  all_rows <- list()
  n_suppressed_untouched <- 0L
  safe_changed_map <- list()

  for (rel_f in names(changed_map)) {
    if (!grepl(pattern, rel_f)) next
    # Strictly enforce relative path safety (reject `..`, absolute paths, control bytes)
    rel_clean <- validate_safe_rel_path(rel_f, arg = "diff file path")
    abs_f <- file.path(root_norm, rel_clean)
    if (!is_safe_contained_file(abs_f, root_norm)) next

    target_lines <- changed_map[[rel_f]]
    safe_changed_map[[rel_clean]] <- target_lines
    linted_files <- c(linted_files, abs_f)

    full_res <- stat_lint(
      abs_f,
      rules = active_rules,
      exclude_rules = NULL,
      skip_eval_false = skip_eval_false
    )
    if (nrow(full_res$findings) > 0L) {
      keep <- full_res$findings$line %in% target_lines
      n_suppressed_untouched <- n_suppressed_untouched + sum(!keep)
      if (any(keep)) {
        all_rows[[length(all_rows) + 1L]] <- full_res$findings[keep, , drop = FALSE]
      }
    }
  }

  findings <- if (length(all_rows) > 0L) {
    do.call(rbind, all_rows)
  } else {
    empty_findings_tibble()
  }

  obj <- build_lint_object(
    findings = findings,
    files = linted_files,
    rules_run = rule_ids
  )
  obj$diff_meta <- list(
    changed_files = names(safe_changed_map),
    changed_lines = safe_changed_map,
    n_suppressed_untouched = as.integer(n_suppressed_untouched),
    base_ref = if (is.null(diff_text)) base_ref else "<diff_text>"
  )
  obj
}
