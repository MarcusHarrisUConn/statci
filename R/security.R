# Centralized security, sanitization, escaping, and path-validation helpers

#' Strip unsafe ASCII control characters from strings
#' @param x Character vector.
#' @param allow_newline Logical; preserve `\n` and `\t` if TRUE.
#' @return Sanitized character vector.
#' @noRd
strip_control_chars <- function(x, allow_newline = FALSE) {
  x <- as.character(x)
  pat <- if (isTRUE(allow_newline)) {
    "[\\x00-\\x08\\x0b\\x0c\\x0e-\\x1f\\x7f]"
  } else {
    "[\\x00-\\x1f\\x7f]"
  }
  gsub(pat, "", x, perl = TRUE)
}

#' Strictly escape text for safe embedding in HTML body or attribute contexts
#'
#' Strips non-printable control characters and escapes `&`, `<`, `>`, `"`, and
#' `'` using `htmltools::htmlEscape(..., attribute = TRUE)`.
#'
#' @param x Vector coercible to character.
#' @return HTML-escaped character vector.
#' @noRd
html_escape_strict <- function(x) {
  clean <- strip_control_chars(as.character(x), allow_newline = TRUE)
  htmltools::htmlEscape(clean, attribute = TRUE)
}

#' Escape a value safely for GitHub-Flavored Markdown table cells
#'
#' Neutralizes pipe separators, newlines, control characters, and raw HTML
#' angle brackets so untrusted inputs cannot inject raw HTML into GitHub
#' Actions step summaries or PR comments.
#'
#' @param x Vector coercible to character.
#' @return Escaped character vector.
#' @noRd
md_escape_cell <- function(x) {
  s <- strip_control_chars(as.character(x), allow_newline = TRUE)
  s <- gsub("[\r\n]+", " ", s, perl = TRUE)
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  s <- gsub("\\|", "\\\\|", s, perl = TRUE)
  s
}

#' Reject strings containing control characters or embedded newlines
#' @noRd
assert_clean_scalar_string <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    rlang::abort(paste0("`", arg, "` must be a non-empty character scalar."))
  }
  if (grepl("[\\x00-\\x1f\\x7f]", x, perl = TRUE)) {
    rlang::abort(paste0("`", arg, "` contains disallowed control characters or newlines."))
  }
  invisible(x)
}

#' Validate a project-relative path for safe workflow/CI configuration
#'
#' Blocks path traversal (`..`), absolute paths, home-tilde expansion, newlines,
#' quotes, and shell/R/YAML metacharacters.
#'
#' @param x Character scalar relative path.
#' @param arg Argument name for error messages.
#' @param must_be_r_script Logical; require `.R` or `.r` extension.
#' @return Validated character scalar (with normalized forward slashes).
#' @noRd
validate_safe_rel_path <- function(x, arg, must_be_r_script = FALSE) {
  assert_clean_scalar_string(x, arg)
  if (grepl("^(/|~|[A-Za-z]:)", x)) {
    rlang::abort(paste0("`", arg, "` must be a project-relative path, not an absolute or home path: `", x, "`."))
  }
  if (grepl("\\\\", x)) {
    rlang::abort(paste0("`", arg, "` must use forward slashes (`/`) without backslashes: `", x, "`."))
  }
  segs <- strsplit(x, "/", fixed = TRUE)[[1L]]
  if (any(segs == "..")) {
    rlang::abort(paste0("`", arg, "` must not contain parent-directory traversal (`..`): `", x, "`."))
  }
  if (!grepl("^[A-Za-z0-9._/@+-]+$", x)) {
    rlang::abort(paste0(
      "`", arg, "` contains disallowed characters (quotes, spaces, or shell/YAML metacharacters): `", x, "`."
    ))
  }
  if (isTRUE(must_be_r_script) && !grepl("\\.[Rr]$", x)) {
    rlang::abort(paste0("`", arg, "` must point to an R script ending in `.R` or `.r`: `", x, "`."))
  }
  x
}

#' Validate that a relative file destination resolves inside a target root dir
#' @noRd
validate_contained_dest <- function(root_dir, rel_file, arg = "file") {
  validate_safe_rel_path(rel_file, arg = arg)
  root_norm <- normalizePath(root_dir, winslash = "/", mustWork = TRUE)
  dest_raw <- file.path(root_norm, rel_file)
  dest_norm <- normalizePath(dest_raw, winslash = "/", mustWork = FALSE)
  prefix <- paste0(sub("/+$", "", root_norm), "/")
  if (!startsWith(dest_norm, prefix)) {
    rlang::abort(paste0("`", arg, "` resolves outside project directory `", root_dir, "`."))
  }
  dest_raw
}

#' Validate R version specifier for GitHub Actions setup-r
#' @noRd
validate_r_version <- function(r_version) {
  assert_clean_scalar_string(r_version, "r_version")
  valid <- grepl("^(release|devel|oldrel(-[1-4])?|[0-9]+\\.[0-9]+(\\.[0-9]+)?)$", r_version)
  if (!valid) {
    rlang::abort(paste0(
      "`r_version` must be one of `'release'`, `'devel'`, `'oldrel'`, `'oldrel-1'`..`'oldrel-4'`, ",
      "or a numeric version like `'4.4.2'`; got `", r_version, "`."
    ))
  }
  r_version
}

#' Validate Git branch names for GitHub Actions workflow triggers
#' @noRd
validate_branch_names <- function(branches) {
  if (!is.character(branches) || length(branches) < 1L) {
    rlang::abort("`branches` must be a non-empty character vector.")
  }
  for (b in branches) {
    assert_clean_scalar_string(b, "branches")
    if (grepl("^[-/]|/$|\\.\\.", b) || !grepl("^[A-Za-z0-9._/-]+$", b)) {
      rlang::abort(paste0("Invalid Git branch name in `branches`: `", b, "`."))
    }
  }
  branches
}

#' Safely quote a validated scalar string for YAML
#' @noRd
safe_yaml_scalar <- function(x, arg = "value") {
  assert_clean_scalar_string(x, arg)
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

#' Validate an existing regular file path (rejecting control chars, directories, and dangling symlinks)
#' @noRd
validate_existing_file <- function(path, arg = "path") {
  assert_clean_scalar_string(path, arg)
  link_target <- tryCatch(Sys.readlink(path), error = function(e) "")
  if (!is.na(link_target) && nzchar(link_target) && !file.exists(path)) {
    rlang::abort(paste0("`", arg, "` is a broken/dangling symbolic link: `", path, "`."))
  }
  if (dir.exists(path)) {
    rlang::abort(paste0("`", arg, "` is a directory, expected a file: `", path, "`."))
  }
  if (!file.exists(path)) {
    rlang::abort(paste0("File not found for `", arg, "`: `", path, "`."))
  }
  invisible(path)
}

#' Validate an existing directory path (rejecting control chars and broken symlinks)
#' @noRd
validate_existing_dir <- function(path, arg = "path") {
  assert_clean_scalar_string(path, arg)
  link_target <- tryCatch(Sys.readlink(path), error = function(e) "")
  if (!is.na(link_target) && nzchar(link_target) && !dir.exists(path)) {
    rlang::abort(paste0("`", arg, "` is a broken symbolic link: `", path, "`."))
  }
  if (!dir.exists(path)) {
    rlang::abort(paste0("Directory not found for `", arg, "`: `", path, "`."))
  }
  invisible(path)
}

#' Verify that a candidate file inside a scanned directory does not escape via symlink
#' @noRd
is_safe_contained_file <- function(file_path, root_dir) {
  if (!file.exists(file_path) || dir.exists(file_path)) return(FALSE)
  root_real <- normalizePath(root_dir, winslash = "/", mustWork = TRUE)
  file_real <- tryCatch(normalizePath(file_path, winslash = "/", mustWork = TRUE), error = function(e) NA_character_)
  if (is.na(file_real)) return(FALSE)
  prefix <- paste0(sub("/+$", "", root_real), "/")
  startsWith(file_real, prefix)
}
