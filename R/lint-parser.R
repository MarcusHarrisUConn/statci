# AST parsing, Quarto/Rmd chunk extraction, and XML query helpers

#' Extract R code chunks from Quarto (.qmd) or RMarkdown (.Rmd) lines
#'
#' Produces a blank-line-preserving projection of all R code chunks so that
#' 1-based line numbers in parsed R expressions match the original document
#' line numbers exactly while preserving cross-chunk ordering.
#'
#' @param lines Character vector of source lines.
#' @param skip_eval_false Logical; if `TRUE`, omit chunks with `eval = FALSE`
#'   or `#| eval: false` from linting.
#' @return A list with `chunks`, `projected_lines`, and `line_to_chunk`.
#' @noRd
extract_r_chunks <- function(lines, skip_eval_false = FALSE) {
  n <- length(lines)
  projected <- rep("", n)
  line_to_chunk <- rep(NA_character_, n)
  chunks <- list()

  in_chunk <- FALSE
  fence_len <- 0L
  chunk_idx <- 0L
  cur_label <- NA_character_
  cur_start <- NA_integer_
  cur_code_start <- NA_integer_
  cur_eval <- TRUE

  for (i in seq_len(n)) {
    ln <- lines[i]
    if (!in_chunk) {
      m <- regexec("^[[:space:]]*(`{3,})[[:space:]]*\\{[[:space:]]*[rR]([[:space:],=][^}]*)?\\}[[:space:]]*$", ln)
      caps <- regmatches(ln, m)[[1L]]
      if (length(caps) >= 2L) {
        in_chunk <- TRUE
        fence_len <- nchar(caps[2L])
        chunk_idx <- chunk_idx + 1L
        header_opts <- if (length(caps) >= 3L) trimws(caps[3L]) else ""
        # Extract label from header: `{r my-label, echo=FALSE}`
        cur_label <- NA_character_
        cur_eval <- TRUE
        if (nzchar(header_opts)) {
          # Strip leading comma if any
          header_clean <- sub("^,[[:space:]]*", "", header_opts)
          first_part <- trimws(strsplit(header_clean, ",", fixed = TRUE)[[1L]][1L])
          if (nzchar(first_part) && !grepl("=", first_part, fixed = TRUE)) {
            cur_label <- first_part
          }
          if (grepl("\\beval[[:space:]]*=[[:space:]]*(FALSE|F|0)\\b", header_opts)) {
            cur_eval <- FALSE
          }
        }
        cur_start <- i
        cur_code_start <- i + 1L
      }
    } else {
      # Check closing fence
      close_pat <- paste0("^[[:space:]]*`{", fence_len, ",}[[:space:]]*$")
      if (grepl(close_pat, ln)) {
        code_end <- i - 1L
        chunk_lines <- if (code_end >= cur_code_start) lines[cur_code_start:code_end] else character(0)

        # Inspect Quarto YAML pipe comments (#| label: ..., #| eval: false)
        for (cl in chunk_lines) {
          if (!grepl("^[[:space:]]*#\\|", cl)) break
          if (grepl("^[[:space:]]*#\\|[[:space:]]*label[[:space:]]*:", cl)) {
            lbl <- trimws(sub("^[[:space:]]*#\\|[[:space:]]*label[[:space:]]*:", "", cl))
            lbl <- gsub("^['\"]|['\"]$", "", lbl)
            if (nzchar(lbl)) cur_label <- lbl
          }
          if (grepl("^[[:space:]]*#\\|[[:space:]]*eval[[:space:]]*:[[:space:]]*(false|FALSE|no)", cl)) {
            cur_eval <- FALSE
          }
        }

        if (is.na(cur_label) || !nzchar(cur_label)) {
          cur_label <- paste0("chunk-", chunk_idx)
        }

        include_chunk <- !(skip_eval_false && !cur_eval)
        if (include_chunk && code_end >= cur_code_start) {
          idx_range <- cur_code_start:code_end
          projected[idx_range] <- lines[idx_range]
          line_to_chunk[idx_range] <- cur_label
        }

        chunks[[length(chunks) + 1L]] <- list(
          index = chunk_idx,
          label = cur_label,
          start_line = cur_start,
          end_line = i,
          code_start_line = cur_code_start,
          code_end_line = code_end,
          eval = cur_eval,
          included = include_chunk,
          lines = chunk_lines
        )
        in_chunk <- FALSE
      }
    }
  }

  list(
    chunks = chunks,
    projected_lines = projected,
    line_to_chunk = line_to_chunk
  )
}

#' Parse inline suppression directives (`# statci-ignore`)
#' @param lines Character vector of source lines.
#' @return Named list mapping character line numbers to `"*"` or rule ID vectors.
#' @noRd
parse_suppressions <- function(lines) {
  sup <- list()
  add_sup <- function(ln_num, rules_vec) {
    key <- as.character(ln_num)
    if (is.null(sup[[key]])) {
      sup[[key]] <<- rules_vec
    } else if ("*" %in% sup[[key]] || "*" %in% rules_vec) {
      sup[[key]] <<- "*"
    } else {
      sup[[key]] <<- unique(c(sup[[key]], rules_vec))
    }
  }

  for (i in seq_along(lines)) {
    ln <- lines[i]
    if (!grepl("statci-ignore", ln, fixed = TRUE)) next

    # Match `# statci-ignore-next-line` or `# statci-ignore`
    m_next <- regexec("#[[:space:]]*statci-ignore-next-line(?:[[:space:]]*:[[:space:]]*([A-Za-z0-9_,[:space:]-]+))?", ln)
    cap_next <- regmatches(ln, m_next)[[1L]]
    if (length(cap_next) >= 1L && nzchar(cap_next[1L])) {
      target_line <- i + 1L
      if (length(cap_next) >= 2L && nzchar(trimws(cap_next[2L]))) {
        rids <- trimws(strsplit(cap_next[2L], ",", fixed = TRUE)[[1L]])
        rids <- rids[nzchar(rids)]
        add_sup(target_line, rids)
      } else {
        add_sup(target_line, "*")
      }
      next
    }

    m_cur <- regexec("#[[:space:]]*statci-ignore(?:[[:space:]]*:[[:space:]]*([A-Za-z0-9_,[:space:]-]+))?", ln)
    cap_cur <- regmatches(ln, m_cur)[[1L]]
    if (length(cap_cur) >= 1L && nzchar(cap_cur[1L])) {
      if (length(cap_cur) >= 2L && nzchar(trimws(cap_cur[2L]))) {
        rids <- trimws(strsplit(cap_cur[2L], ",", fixed = TRUE)[[1L]])
        rids <- rids[nzchar(rids)]
        add_sup(i, rids)
      } else {
        add_sup(i, "*")
      }
    }
  }
  sup
}

#' Check whether a finding at `line` for `rule_id` is suppressed
#' @noRd
is_suppressed <- function(suppressions, line, rule_id) {
  key <- as.character(line)
  entry <- suppressions[[key]]
  if (is.null(entry)) {
    return(FALSE)
  }
  "*" %in% entry || rule_id %in% entry
}

#' Parse a vector of code lines into an xml2 document
#' @noRd
parse_lines_to_xml <- function(code_lines) {
  text_joined <- paste(code_lines, collapse = "\n")
  if (!nzchar(trimws(text_joined))) {
    return(list(xml = NULL, error = NULL))
  }
  res <- tryCatch(
    {
      exprs <- parse(text = code_lines, keep.source = TRUE)
      pd <- utils::getParseData(exprs)
      if (is.null(pd) || nrow(pd) == 0L) {
        return(list(xml = NULL, error = NULL))
      }
      xml_str <- xmlparsedata::xml_parse_data(exprs)
      doc <- xml2::read_xml(xml_str)
      list(xml = doc, error = NULL)
    },
    error = function(e) {
      list(xml = NULL, error = conditionMessage(e))
    }
  )
  res
}

#' Build a full parse context for a file or text buffer
#' @param lines Character vector of source lines.
#' @param file File path or `"<text>"`.
#' @param is_rmd Logical; TRUE for `.qmd` / `.Rmd`.
#' @param skip_eval_false Logical; whether to skip `eval = FALSE` chunks.
#' @return A list representing the parsed context passed to lint rules.
#' @noRd
build_parse_context <- function(lines, file = "<text>", is_rmd = FALSE, skip_eval_false = FALSE) {
  if (is.null(lines)) lines <- character(0)
  suppressions <- parse_suppressions(lines)

  if (is_rmd) {
    chunk_info <- extract_r_chunks(lines, skip_eval_false = skip_eval_false)
    line_to_chunk <- chunk_info$line_to_chunk
    chunks <- chunk_info$chunks
    code_lines <- chunk_info$projected_lines
  } else {
    line_to_chunk <- rep(NA_character_, length(lines))
    chunks <- list()
    code_lines <- lines
  }

  units <- list()
  parse_errors <- list()

  parsed <- parse_lines_to_xml(code_lines)
  if (is.null(parsed$error)) {
    if (!is.null(parsed$xml)) {
      units[[1L]] <- list(xml = parsed$xml, line_offset = 0L)
    }
  } else if (is_rmd && length(chunks) > 0L) {
    # Fallback: parse chunks individually so one malformed chunk does not block others
    for (ch in chunks) {
      if (!isTRUE(ch$included) || length(ch$lines) == 0L) next
      ch_parsed <- parse_lines_to_xml(ch$lines)
      if (is.null(ch_parsed$error)) {
        if (!is.null(ch_parsed$xml)) {
          units[[length(units) + 1L]] <- list(
            xml = ch_parsed$xml,
            line_offset = ch$code_start_line - 1L
          )
        }
      } else {
        parse_errors[[length(parse_errors) + 1L]] <- list(
          line = ch$code_start_line,
          col = 1L,
          chunk = ch$label,
          message = paste0("Parse error in chunk `", ch$label, "`: ", ch_parsed$error)
        )
      }
    }
  } else {
    # Extract line number from parse error message if available
    err_line <- 1L
    m <- regexec(":([0-9]+):([0-9]+):", parsed$error)
    caps <- regmatches(parsed$error, m)[[1L]]
    if (length(caps) >= 2L) {
      err_line <- suppressWarnings(as.integer(caps[2L]))
      if (is.na(err_line)) err_line <- 1L
    }
    parse_errors[[1L]] <- list(
      line = err_line,
      col = 1L,
      chunk = NA_character_,
      message = paste0("R parse error: ", parsed$error)
    )
  }

  list(
    file = file,
    is_rmd = is_rmd,
    lines = lines,
    line_to_chunk = line_to_chunk,
    chunks = chunks,
    suppressions = suppressions,
    units = units,
    parse_errors = parse_errors
  )
}

# ---------------------------------------------------------------------------
# AST XML query helpers
# ---------------------------------------------------------------------------

#' 1-based line number of an XML AST node
#' @noRd
ast_line <- function(node, offset = 0L) {
  as.integer(xml2::xml_attr(node, "line1")) + as.integer(offset)
}

#' 1-based end line number of an XML AST node
#' @noRd
ast_line2 <- function(node, offset = 0L) {
  as.integer(xml2::xml_attr(node, "line2")) + as.integer(offset)
}

#' 1-based column number of an XML AST node
#' @noRd
ast_col <- function(node) {
  as.integer(xml2::xml_attr(node, "col1"))
}

#' Extract clean single-line snippet for a given line in ctx
#' @noRd
ctx_snippet <- function(ctx, line) {
  if (is.na(line) || line < 1L || line > length(ctx$lines)) {
    return("")
  }
  trimws(ctx$lines[line])
}

#' Find call `<expr>` nodes matching optional function names
#' @param xml xml2 document or node.
#' @param fn_names Optional character vector of function names to match.
#' @return xml_nodeset of call `<expr>` nodes.
#' @noRd
ast_calls <- function(xml, fn_names = NULL) {
  if (is.null(fn_names)) {
    return(xml2::xml_find_all(xml, "descendant-or-self::expr[expr[1]/SYMBOL_FUNCTION_CALL and OP-LEFT-PAREN]"))
  }
  # Escape quotes defensively (function names are R identifiers)
  conds <- paste0("text() = '", gsub("'", "", fn_names, fixed = TRUE), "'")
  xpath <- paste0(
    "descendant-or-self::expr[expr[1]/SYMBOL_FUNCTION_CALL[",
    paste(conds, collapse = " or "),
    "] and OP-LEFT-PAREN]"
  )
  xml2::xml_find_all(xml, xpath)
}

#' Get function name from a call `<expr>` node
#' @noRd
ast_call_name <- function(call_node) {
  fn <- xml2::xml_find_first(call_node, "./expr[1]/SYMBOL_FUNCTION_CALL")
  if (inherits(fn, "xml_missing")) return(NA_character_)
  xml2::xml_text(fn)
}

#' Get package namespace prefix from a call `<expr>` node, or NA
#' @noRd
ast_call_pkg <- function(call_node) {
  pkg <- xml2::xml_find_first(call_node, "./expr[1]/SYMBOL_PACKAGE")
  if (inherits(pkg, "xml_missing")) return(NA_character_)
  xml2::xml_text(pkg)
}

#' Parse top-level arguments of a call `<expr>` node
#' @return List of lists, each with `name` (character or NA), `expr` (xml_node or NULL), `text` (character).
#' @noRd
ast_call_args <- function(call_node) {
  children <- xml2::xml_children(call_node)
  if (length(children) < 3L) return(list())
  names_ch <- xml2::xml_name(children)
  lp <- which(names_ch == "OP-LEFT-PAREN")[1L]
  rp <- tail_pos(which(names_ch == "OP-RIGHT-PAREN"))
  if (is.na(lp) || is.na(rp) || rp <= lp + 1L) return(list())

  inner <- children[(lp + 1L):(rp - 1L)]
  inner_names <- names_ch[(lp + 1L):(rp - 1L)]

  args <- list()
  i <- 1L
  m <- length(inner)
  while (i <= m) {
    nm <- inner_names[i]
    if (nm == "OP-COMMA") {
      i <- i + 1L
      next
    }
    if (nm == "SYMBOL_SUB") {
      arg_name <- xml2::xml_text(inner[[i]])
      # Next should be EQ_SUB, followed optionally by expr
      if (i + 2L <= m && inner_names[i + 1L] == "EQ_SUB" && inner_names[i + 2L] != "OP-COMMA") {
        val_node <- inner[[i + 2L]]
        args[[length(args) + 1L]] <- list(
          name = arg_name,
          expr = val_node,
          text = xml2::xml_text(val_node)
        )
        i <- i + 3L
      } else {
        args[[length(args) + 1L]] <- list(
          name = arg_name,
          expr = NULL,
          text = ""
        )
        i <- i + 2L
      }
    } else {
      val_node <- inner[[i]]
      args[[length(args) + 1L]] <- list(
        name = NA_character_,
        expr = val_node,
        text = xml2::xml_text(val_node)
      )
      i <- i + 1L
    }
  }
  args
}

#' Helper: last element of integer vector or NA
#' @noRd
tail_pos <- function(x) {
  if (length(x) == 0L) NA_integer_ else x[length(x)]
}

#' Check whether an AST node is nested inside a function(...) definition
#' @noRd
ast_is_inside_function <- function(node) {
  anc <- xml2::xml_find_first(node, "ancestor::expr[FUNCTION]")
  !inherits(anc, "xml_missing")
}

#' Check whether an AST node is nested inside a call to one of `fn_names`
#' @noRd
ast_is_inside_call <- function(node, fn_names) {
  conds <- paste0("text() = '", gsub("'", "", fn_names, fixed = TRUE), "'")
  xpath <- paste0(
    "ancestor::expr[expr[1]/SYMBOL_FUNCTION_CALL[",
    paste(conds, collapse = " or "),
    "] and OP-LEFT-PAREN]"
  )
  anc <- xml2::xml_find_first(node, xpath)
  !inherits(anc, "xml_missing")
}

#' Return all `<SYMBOL>` texts within a node
#' @noRd
ast_symbols <- function(node) {
  if (is.null(node)) return(character(0))
  xml2::xml_text(xml2::xml_find_all(node, ".//SYMBOL"))
}

#' Return all `<SYMBOL_FUNCTION_CALL>` texts within a node
#' @noRd
ast_fn_calls_in <- function(node) {
  if (is.null(node)) return(character(0))
  xml2::xml_text(xml2::xml_find_all(node, ".//SYMBOL_FUNCTION_CALL"))
}

#' Check if an expression is piped from a left-hand expression (`x |> fn(...)` or `x %>% fn(...)`)
#' @return The piped-in LHS xml_node, or NULL.
#' @noRd
ast_piped_lhs <- function(call_node) {
  parent <- xml2::xml_parent(call_node)
  if (inherits(parent, "xml_missing")) return(NULL)
  ch <- xml2::xml_children(parent)
  if (length(ch) != 3L) return(NULL)
  mid_name <- xml2::xml_name(ch[[2L]])
  mid_text <- xml2::xml_text(ch[[2L]])
  is_pipe <- mid_name == "PIPE" || (mid_name == "SPECIAL" && mid_text %in% c("%>%", "%T>%", "%<>%"))
  if (is_pipe && identical(xml2::xml_path(ch[[3L]]), xml2::xml_path(call_node))) {
    return(ch[[1L]])
  }
  NULL
}

#' Extract variable assignment target for an expression (handles `<-`, `=`, `->` and pipes)
#' @return Character variable name or NA_character_.
#' @noRd
ast_assigned_var <- function(node) {
  cur <- node
  # Walk up through pipe chains to find assignment
  for (step in seq_len(10L)) {
    parent <- xml2::xml_parent(cur)
    if (inherits(parent, "xml_missing")) return(NA_character_)
    ch <- xml2::xml_children(parent)
    if (length(ch) == 3L) {
      op <- xml2::xml_name(ch[[2L]])
      op_txt <- xml2::xml_text(ch[[2L]])
      if (op %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) {
        lhs_sym <- xml2::xml_find_first(ch[[1L]], "./SYMBOL")
        if (!inherits(lhs_sym, "xml_missing")) {
          return(xml2::xml_text(lhs_sym))
        }
        # Handle df$col <- ...
        lhs_text <- xml2::xml_text(ch[[1L]])
        if (nzchar(lhs_text)) return(lhs_text)
        return(NA_character_)
      }
      if (op == "RIGHT_ASSIGN") {
        rhs_sym <- xml2::xml_find_first(ch[[3L]], "./SYMBOL")
        if (!inherits(rhs_sym, "xml_missing")) {
          return(xml2::xml_text(rhs_sym))
        }
        return(NA_character_)
      }
      if (op == "PIPE" || (op == "SPECIAL" && op_txt %in% c("%>%", "%<>%"))) {
        cur <- parent
        next
      }
    }
    break
  }
  NA_character_
}

#' Extract all variable assignments in an XML AST unit
#' @return List of records: lhs, lhs_root, rhs_node, line, col, rhs_text, rhs_calls, rhs_symbols.
#' @noRd
ast_assignments <- function(xml, offset = 0L) {
  nodes <- xml2::xml_find_all(
    xml,
    ".//*[self::expr or self::equal_assign][LEFT_ASSIGN or EQ_ASSIGN or RIGHT_ASSIGN]"
  )
  out <- vector("list", length(nodes))
  k <- 0L
  for (nd in nodes) {
    ch <- xml2::xml_children(nd)
    if (length(ch) != 3L) next
    op <- xml2::xml_name(ch[[2L]])
    if (op %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) {
      lhs_node <- ch[[1L]]
      rhs_node <- ch[[3L]]
    } else if (op == "RIGHT_ASSIGN") {
      lhs_node <- ch[[3L]]
      rhs_node <- ch[[1L]]
    } else {
      next
    }
    lhs_txt <- xml2::xml_text(lhs_node)
    # Extract base object symbol if lhs is `df$col` or `df[["col"]]`
    lhs_syms <- ast_symbols(lhs_node)
    lhs_root <- if (length(lhs_syms) >= 1L) lhs_syms[1L] else lhs_txt
    k <- k + 1L
    out[[k]] <- list(
      lhs = lhs_txt,
      lhs_root = lhs_root,
      lhs_node = lhs_node,
      rhs_node = rhs_node,
      line = ast_line(nd, offset),
      col = ast_col(nd),
      rhs_text = xml2::xml_text(rhs_node),
      rhs_calls = ast_fn_calls_in(rhs_node),
      rhs_symbols = ast_symbols(rhs_node)
    )
  }
  out[seq_len(k)]
}

#' Create a normalized finding record inside a rule check
#' @noRd
make_finding <- function(ctx, line, col = 1L, message, snippet = NULL) {
  ln <- as.integer(line)
  ch <- if (!is.na(ln) && ln >= 1L && ln <= length(ctx$line_to_chunk)) {
    ctx$line_to_chunk[ln]
  } else {
    NA_character_
  }
  list(
    line = ln,
    col = as.integer(col),
    chunk = ch,
    message = as.character(message),
    code_snippet = snippet %||% ctx_snippet(ctx, ln)
  )
}
