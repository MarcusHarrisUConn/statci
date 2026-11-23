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

  pkg_ver <- tool_version %||% tryCatch(as.character(utils::packageVersion("statci")), error = function(e) "0.3.0")
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

  # Normalize artifact URIs cleanly relative to base_uri
  base_prefix <- NULL
  if (!is.null(base_uri) && nzchar(base_uri)) {
    base_norm <- normalizePath(base_uri, winslash = "/", mustWork = FALSE)
    base_prefix <- paste0(sub("/+$", "", base_norm), "/")
  }
  normalize_uri <- function(f) {
    f_clean <- strip_control_chars(as.character(f), allow_newline = FALSE)
    if (startsWith(f_clean, "<text")) return("inline.R")
    f_norm <- gsub("\\\\", "/", f_clean)
    if (!is.null(base_prefix)) {
      f_real <- tryCatch(normalizePath(f_clean, winslash = "/", mustWork = FALSE), error = function(e) f_norm)
      if (startsWith(f_real, base_prefix)) {
        f_norm <- substring(f_real, nchar(base_prefix) + 1L)
      }
    }
    sub("^\\./", "", f_norm)
  }

  f_tbl <- lint$findings
  sarif_results <- lapply(seq_len(nrow(f_tbl)), function(i) {
    row <- f_tbl[i, ]
    uri <- normalize_uri(row$file)
    ln <- max(1L, as.integer(row$line %||% 1L))
    cl <- max(1L, as.integer(row$col %||% 1L))
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
