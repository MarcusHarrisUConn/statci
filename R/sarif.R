# SARIF 2.1.0 Export for GitHub Code Scanning and PR Inline Annotations

#' Export `statci` AST lint results to SARIF 2.1.0 JSON
#'
#' Serializes a `"statci_lint"` object (or the lint component of a
#' `"statci_report"`) into a valid Static Analysis Results Interchange Format
#' (SARIF) 2.1.0 JSON log compatible with GitHub Code Scanning
#' (`github/codeql-action/upload-sarif@v3`) for inline pull-request annotations.
#'
#' @param lint A `"statci_lint"` object from [stat_lint()] / [stat_lint_dir()]
#'   or a `"statci_report"` containing a lint component.
#' @param output_file Optional path to write the `.sarif` JSON file. If `NULL`
#'   (default), returns the serialized SARIF 2.1.0 JSON string.
#' @param base_uri Optional base project directory path stripped from file
#'   paths so artifact URIs are clean repository-relative paths.
#' @param tool_version Optional tool version string (defaults to the installed
#'   `statci` package version).
#'
#' @return If `output_file` is `NULL`, returns a character scalar of SARIF
#'   2.1.0 JSON; otherwise writes the file and returns `output_file` invisibly.
#' @export
#'
#' @examples
#' lint_res <- stat_lint_text("x <- rnorm(10)\n")
#' sarif_json <- export_statci_sarif(lint_res)
#' parsed <- jsonlite::fromJSON(sarif_json)
#' parsed$version
export_statci_sarif <- function(lint,
                                output_file = NULL,
                                base_uri = NULL,
                                tool_version = NULL) {
  if (inherits(lint, "statci_report")) {
    if (is.null(lint$components$lint)) {
      rlang::abort("`statci_report` does not contain a `lint` component to export as SARIF.")
    }
    lint <- lint$components$lint
  }
  if (!inherits(lint, "statci_lint")) {
    rlang::abort("`lint` must be a `statci_lint` or `statci_report` object.")
  }

  pkg_ver <- tool_version %||% tryCatch(as.character(utils::packageVersion("statci")), error = function(e) "0.5.0")
  sev_to_sarif <- function(s) {
    switch(tolower(as.character(s)), error = "error", warning = "warning", info = "note", "warning")
  }

  # Build SARIF driver rule descriptors for all rules run (plus parse_error if hit)
  reg_tbl <- list_stat_rules()
  rule_ids <- unique(c(lint$rules_run, lint$findings$rule))
  sarif_rules <- lapply(rule_ids, function(rid) {
    idx <- which(reg_tbl$id == rid)
    if (length(idx) == 1L) {
      r <- reg_tbl[idx, ]
      list(
        id = r$id,
        name = r$id,
        shortDescription = list(text = r$title),
        fullDescription = list(text = if (nzchar(r$description)) r$description else r$title),
        help = list(text = if (nzchar(r$recommendation)) r$recommendation else r$title),
        defaultConfiguration = list(level = sev_to_sarif(r$severity)),
        properties = list(tags = list("statistical-ci", r$category))
      )
    } else {
      list(
        id = rid,
        name = rid,
        shortDescription = list(text = paste0("statci rule: ", rid)),
        defaultConfiguration = list(level = "error"),
        properties = list(tags = list("statistical-ci"))
      )
    }
  })

  # Normalize artifact URIs cleanly relative to base_uri and RFC-3986 percent-encode
  base_prefix <- NULL
  if (!is.null(base_uri) && nzchar(base_uri)) {
    base_norm <- normalizePath(base_uri, winslash = "/", mustWork = FALSE)
    base_prefix <- paste0(sub("/+$", "", base_norm), "/")
  }
  cwd_norm <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = FALSE), error = function(e) NULL)
  cwd_prefix <- if (!is.null(cwd_norm) && nzchar(cwd_norm)) paste0(sub("/+$", "", cwd_norm), "/") else NULL

  encode_uri_path <- function(rel_path) {
    segs <- strsplit(rel_path, "/", fixed = TRUE)[[1L]]
    enc_segs <- vapply(segs, function(seg) {
      # Encode each UTF-8 byte not in RFC 3986 unreserved set [A-Za-z0-9._~-]
      bytes <- charToRaw(enc2utf8(seg))
      if (length(bytes) == 0L) return("")
      out_chars <- vapply(bytes, function(b) {
        bi <- as.integer(b)
        is_unres <- (bi >= 65L && bi <= 90L) || (bi >= 97L && bi <= 122L) ||
          (bi >= 48L && bi <= 57L) || bi %in% c(45L, 46L, 95L, 126L)
        if (is_unres) rawToChar(b) else sprintf("%%%02X", bi)
      }, character(1L))
      paste0(out_chars, collapse = "")
    }, character(1L))
    paste(enc_segs, collapse = "/")
  }

  normalize_uri <- function(f) {
    f_clean <- strip_control_chars(as.character(f), allow_newline = FALSE)
    if (startsWith(f_clean, "<text")) return("inline.R")
    f_norm <- gsub("\\\\", "/", f_clean)
    f_real <- tryCatch(normalizePath(f_clean, winslash = "/", mustWork = FALSE), error = function(e) f_norm)
    if (!is.null(base_prefix) && startsWith(f_real, base_prefix)) {
      f_norm <- substring(f_real, nchar(base_prefix) + 1L)
    } else if (!is.null(base_prefix) && startsWith(f_norm, base_prefix)) {
      f_norm <- substring(f_norm, nchar(base_prefix) + 1L)
    } else if (grepl("^(/|[A-Za-z]:/)", f_norm)) {
      # Never leak host absolute filesystem paths when base_uri is omitted or doesn't match
      if (!is.null(cwd_prefix) && startsWith(f_real, cwd_prefix)) {
        f_norm <- substring(f_real, nchar(cwd_prefix) + 1L)
      } else {
        f_norm <- basename(f_norm)
      }
    }
    f_norm <- sub("^\\./", "", f_norm)
    encode_uri_path(f_norm)
  }

  f_tbl <- lint$findings
  sarif_results <- lapply(seq_len(nrow(f_tbl)), function(i) {
    row <- f_tbl[i, ]
    uri <- normalize_uri(row$file)
    ln_raw <- suppressWarnings(as.integer(row$line %||% 1L))[1L]
    ln <- if (is.na(ln_raw) || ln_raw < 1L) 1L else ln_raw
    cl_raw <- suppressWarnings(as.integer(row$col %||% 1L))[1L]
    cl <- if (is.na(cl_raw) || cl_raw < 1L) 1L else cl_raw
    msg <- strip_control_chars(row$message, allow_newline = FALSE)
    if (!is.na(row$chunk) && nzchar(row$chunk)) {
      msg <- paste0("[chunk: ", strip_control_chars(row$chunk, allow_newline = FALSE), "] ", msg)
    }
    fp <- paste0(row$rule, "|", uri, "|", ln, "|", cl)
    list(
      ruleId = row$rule,
      level = sev_to_sarif(row$severity),
      message = list(text = msg),
      locations = list(
        list(
          physicalLocation = list(
            artifactLocation = list(uri = uri),
            region = list(startLine = ln, startColumn = cl)
          )
        )
      ),
      partialFingerprints = list(primaryLocationLineHash = fp)
    )
  })

  sarif_doc <- list(
    `$schema` = "https://json.schemastore.org/sarif-2.1.0.json",
    version = "2.1.0",
    runs = list(
      list(
        tool = list(
          driver = list(
            name = "statci",
            informationUri = "https://marcusharrisphd.com/statci/",
            version = pkg_ver,
            rules = sarif_rules
          )
        ),
        results = sarif_results
      )
    )
  )

  json_str <- as.character(jsonlite::toJSON(sarif_doc, auto_unbox = TRUE, pretty = TRUE))

  if (!is.null(output_file)) {
    dir_out <- dirname(output_file)
    if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
    writeLines(json_str, con = output_file, useBytes = TRUE)
    return(invisible(output_file))
  }

  json_str
}
