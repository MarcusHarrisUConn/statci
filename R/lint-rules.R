# Statistical lint rule S3 constructor, registry, and 13 built-in AST rules

.statci_rule_registry <- new.env(parent = emptyenv())

#' Construct a custom `statci` lint rule
#'
#' Creates a `statci_rule` object that can be passed to [stat_lint()] or
#' registered globally with [register_stat_rule()].
#'
#' @param id Unique snake_case identifier for the rule (character scalar).
#' @param title Short human-readable title (character scalar).
#' @param severity Severity level: `"error"`, `"warning"`, or `"info"`.
#' @param category Methodological category (e.g., `"reproducibility"`,
#'   `"parallel_rng"`, `"data_leakage"`, `"modeling"`, `"numerics"`,
#'   `"inference"`, `"hygiene"`).
#' @param description Detailed explanation of the statistical hazard.
#' @param recommendation Actionable remediation guidance.
#' @param check A function `function(ctx)` taking an internal parse context
#'   (containing `file`, `lines`, `units`, `line_to_chunk`) and returning a list
#'   of finding records, each with elements `line`, `col`, `message` (and
#'   optional `chunk`, `code_snippet`).
#'
#' @return An S3 object of class `"statci_rule"`.
#' @export
#'
#' @examples
#' my_rule <- new_stat_rule(
#'   id = "no_attach",
#'   title = "Avoid attach() in analysis scripts",
#'   severity = "warning",
#'   category = "hygiene",
#'   description = "attach() mutates the search path and causes silent variable masking.",
#'   recommendation = "Reference columns explicitly via data$col or with().",
#'   check = function(ctx) {
#'     out <- list()
#'     for (u in ctx$units) {
#'       calls <- xml2::xml_find_all(
#'         u$xml,
#'         ".//expr[expr[1]/SYMBOL_FUNCTION_CALL[text()='attach'] and OP-LEFT-PAREN]"
#'       )
#'       for (nd in calls) {
#'         ln <- as.integer(xml2::xml_attr(nd, "line1")) + u$line_offset
#'         out[[length(out) + 1L]] <- list(
#'           line = ln,
#'           col = as.integer(xml2::xml_attr(nd, "col1")),
#'           message = "Avoid attach(); reference data frames explicitly."
#'         )
#'       }
#'     }
#'     out
#'   }
#' )
#' my_rule
new_stat_rule <- function(id,
                          title,
                          severity = c("warning", "error", "info"),
                          category = "modeling",
                          description = "",
                          recommendation = "",
                          check) {
  severity <- match.arg(severity)
  if (!is.character(id) || length(id) != 1L || !nzchar(id)) {
    rlang::abort("`id` must be a non-empty character scalar.")
  }
  if (!is.character(title) || length(title) != 1L || !nzchar(title)) {
    rlang::abort("`title` must be a non-empty character scalar.")
  }
  if (!is.function(check)) {
    rlang::abort("`check` must be a function taking a parse context `ctx`.")
  }
  structure(
    list(
      id = id,
      title = title,
      severity = severity,
      category = as.character(category)[1L],
      description = as.character(description)[1L],
      recommendation = as.character(recommendation)[1L],
      check = check
    ),
    class = "statci_rule"
  )
}

#' @export
print.statci_rule <- function(x, ...) {
  cli::cli_text("<statci_rule> {.strong {x$id}} [{toupper(x$severity)} | {x$category}]")
  cli::cli_text("  {x$title}")
  if (nzchar(x$description)) {
    cli::cli_text("  {x$description}")
  }
  invisible(x)
}

#' List available statistical lint rules
#'
#' Returns a tibble summarizing all currently registered statistical lint rules
#' (built-in plus any user-registered custom rules).
#'
#' @param category Optional character vector to filter rules by category.
#' @param severity Optional character vector to filter rules by severity
#'   (`"error"`, `"warning"`, `"info"`).
#'
#' @return A [tibble::tibble] with columns `id`, `severity`, `category`,
#'   `title`, `description`, and `recommendation`.
#' @export
#'
#' @examples
#' list_stat_rules()
#' list_stat_rules(severity = "error")
list_stat_rules <- function(category = NULL, severity = NULL) {
  ensure_default_rules()
  ids <- ls(envir = .statci_rule_registry, sorted = TRUE)
  rules <- lapply(ids, function(id) .statci_rule_registry[[id]])
  # Preserve built-in ordering first, then custom rules alphabetically
  builtin_order <- default_rule_ids()
  ord <- match(ids, builtin_order)
  ord[is.na(ord)] <- length(builtin_order) + seq_len(sum(is.na(ord)))
  rules <- rules[order(ord)]

  tbl <- tibble::tibble(
    id = vapply(rules, `[[`, character(1L), "id"),
    severity = vapply(rules, `[[`, character(1L), "severity"),
    category = vapply(rules, `[[`, character(1L), "category"),
    title = vapply(rules, `[[`, character(1L), "title"),
    description = vapply(rules, `[[`, character(1L), "description"),
    recommendation = vapply(rules, `[[`, character(1L), "recommendation")
  )
  if (!is.null(category)) {
    tbl <- tbl[tbl$category %in% category, , drop = FALSE]
  }
  if (!is.null(severity)) {
    tbl <- tbl[tbl$severity %in% severity, , drop = FALSE]
  }
  tbl
}

#' Register a custom statistical lint rule
#'
#' Adds a `statci_rule` created with [new_stat_rule()] to the active `statci`
#' rule registry so that subsequent calls to [stat_lint()] and
#' [stat_lint_dir()] include it automatically.
#'
#' @param rule A `statci_rule` object created by [new_stat_rule()].
#' @param overwrite Logical; if `TRUE`, allows replacing an existing rule with
#'   the same `id`.
#'
#' @return Invisibly returns `rule`.
#' @export
#'
#' @examples
#' r <- new_stat_rule(
#'   id = "demo_rule",
#'   title = "Demo rule",
#'   severity = "info",
#'   check = function(ctx) list()
#' )
#' register_stat_rule(r)
#' "demo_rule" %in% list_stat_rules()$id
#' unregister_stat_rule("demo_rule")
register_stat_rule <- function(rule, overwrite = FALSE) {
  ensure_default_rules()
  if (!inherits(rule, "statci_rule")) {
    rlang::abort("`rule` must be a `statci_rule` object created with `new_stat_rule()`.")
  }
  if (!overwrite && exists(rule$id, envir = .statci_rule_registry, inherits = FALSE)) {
    rlang::abort(paste0("Rule `", rule$id, "` is already registered. Set `overwrite = TRUE` to replace it."))
  }
  assign(rule$id, rule, envir = .statci_rule_registry)
  invisible(rule)
}

#' Remove a rule from the active registry
#'
#' @param id Character scalar rule identifier to remove.
#' @return Invisibly returns `TRUE` if removed, `FALSE` if not present.
#' @export
#'
#' @examples
#' r <- new_stat_rule("temp_rule", "Temp", check = function(ctx) list())
#' register_stat_rule(r)
#' unregister_stat_rule("temp_rule")
unregister_stat_rule <- function(id) {
  ensure_default_rules()
  if (exists(id, envir = .statci_rule_registry, inherits = FALSE)) {
    rm(list = id, envir = .statci_rule_registry)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

#' Reset the rule registry to the built-in `statci` rule set
#'
#' Removes any custom registered rules and restores all built-in rules.
#'
#' @return Invisibly returns a character vector of built-in rule IDs.
#' @export
#'
#' @examples
#' reset_stat_rules()
reset_stat_rules <- function() {
  rm(list = ls(envir = .statci_rule_registry, all.names = TRUE), envir = .statci_rule_registry)
  for (r in build_default_rules()) {
    assign(r$id, r, envir = .statci_rule_registry)
  }
  invisible(default_rule_ids())
}

ensure_default_rules <- function() {
  if (length(ls(envir = .statci_rule_registry)) == 0L) {
    for (r in build_default_rules()) {
      assign(r$id, r, envir = .statci_rule_registry)
    }
  }
}

default_rule_ids <- function() {
  c(
    "unseeded_randomness",
    "unsafe_parallel_rng",
    "duplicate_parallel_seed",
    "preprocessing_before_split",
    "in_sample_prediction_eval",
    "uncentered_mlm_interaction",
    "silent_listwise_mismatch",
    "silent_factor_drop",
    "silent_na_coercion",
    "unverified_merge_cardinality",
    "missing_drop_false_matrix",
    "implicit_na_rm_masking",
    "equality_on_float",
    "unadjusted_multiple_testing",
    "stepwise_selection_unvalidated",
    "missing_convergence_check",
    "global_seed_override_in_fn",
    "short_logical_literal",
    "cor_without_use_arg",
    "absolute_path_in_script"
  )
}

# ===========================================================================
# Built-in Rule Implementations (13 AST Rules)
# ===========================================================================

# 1. unseeded_randomness
check_unseeded_randomness <- function(ctx) {
  stochastic_fns <- c(
    "rnorm", "runif", "rbinom", "rpois", "rexp", "rgamma", "rbeta",
    "rt", "rf", "rchisq", "rweibull", "rlnorm", "rmultinom", "rnbinom",
    "rcauchy", "rgeom", "rhyper", "rlogis", "rwilcox", "rsignrank",
    "rmvnorm", "mvrnorm", "sample", "sample.int", "kmeans", "boot",
    "randomForest", "ranger", "xgb.train", "cv.glmnet",
    "initial_split", "vfold_cv", "bootstraps", "sampling", "stan", "brm"
  )
  # Note: RNGkind() only chooses the algorithm and does NOT seed the RNG
  seed_fns <- c("set.seed", "clusterSetRNGStream", "local_seed")
  wrapper_fns <- c("with_seed", "local_seed", "with_preserve_seed")
  hof_fns <- c("lapply", "sapply", "vapply", "apply", "mapply", "Map", "replicate", "do.call",
               "map", "map_dbl", "map_int", "map_chr", "map_lgl", "pmap")

  findings <- list()
  earliest_seed_line <- Inf

  # First pass: find earliest top-level seed across all units
  for (u in ctx$units) {
    seed_calls <- ast_calls(u$xml, seed_fns)
    for (sc in seed_calls) {
      if (!ast_is_inside_function(sc)) {
        ln <- ast_line(sc, u$line_offset)
        if (ln < earliest_seed_line) earliest_seed_line <- ln
      }
    }
  }

  is_seeded_at <- function(u, nd, ln) {
    if (ln > earliest_seed_line) return(TRUE)
    if (ln == earliest_seed_line) {
      sc_same <- ast_calls(u$xml, seed_fns)
      cols <- vapply(sc_same, function(s) {
        if (!ast_is_inside_function(s) && ast_line(s, u$line_offset) == ln) ast_col(s) else Inf
      }, numeric(1L))
      if (any(cols < ast_col(nd))) return(TRUE)
    }
    FALSE
  }

  # Second pass: inspect top-level direct stochastic calls and higher-order invocations
  for (u in ctx$units) {
    calls <- ast_calls(u$xml, stochastic_fns)
    for (nd in calls) {
      if (ast_is_inside_function(nd)) next
      if (ast_is_inside_call(nd, wrapper_fns)) next
      ln <- ast_line(nd, u$line_offset)
      if (is_seeded_at(u, nd, ln)) next
      fn <- ast_call_name(nd)
      findings[[length(findings) + 1L]] <- make_finding(
        ctx,
        line = ln,
        col = ast_col(nd),
        message = paste0(
          "Stochastic call `", fn, "()` invoked without a preceding `set.seed()` or ",
          "`withr::with_seed()` call."
        )
      )
    }

    # Catch higher-order invocations like lapply(1:5, rnorm) or do.call(rnorm, list(100))
    for (hc in ast_calls(u$xml, hof_fns)) {
      if (ast_is_inside_function(hc)) next
      if (ast_is_inside_call(hc, wrapper_fns)) next
      ln <- ast_line(hc, u$line_offset)
      if (is_seeded_at(u, hc, ln)) next
      hfn <- ast_call_name(hc)
      hargs <- ast_call_args(hc)
      fun_node <- if (hfn == "do.call") {
        if (length(hargs) >= 1L) hargs[[1L]]$expr else NULL
      } else if (hfn == "apply") {
        if (length(hargs) >= 3L) hargs[[3L]]$expr else NULL
      } else {
        if (length(hargs) >= 2L) hargs[[2L]]$expr else NULL
      }
      if (is.null(fun_node)) next
      # Match bare symbol (`rnorm`), ns-qualified (`stats::rnorm`), or string (`"rnorm"` in do.call)
      fun_txt <- trimws(gsub("^['\"]|['\"]$", "", xml2::xml_text(fun_node)))
      fun_base <- sub("^.*::", "", fun_txt)
      if (fun_base %in% stochastic_fns && length(ast_fn_calls_in(fun_node)) == 0L) {
        findings[[length(findings) + 1L]] <- make_finding(
          ctx,
          line = ln,
          col = ast_col(hc),
          message = paste0(
            "Stochastic function `", fun_base, "` invoked via `", hfn,
            "()` without a preceding `set.seed()` or `withr::with_seed()` call."
          )
        )
      }
    }
  }
  findings
}

# 2. unsafe_parallel_rng
check_unsafe_parallel_rng <- function(ctx) {
  fork_fns <- c("mclapply", "mcmapply", "pvec")
  cluster_fns <- c(
    "parLapply", "parSapply", "parApply", "parRapply", "parCapply",
    "clusterApply", "clusterApplyLB", "clusterCall"
  )
  future_fns <- c(
    "future_lapply", "future_sapply", "future_apply", "future_vapply",
    "future_map", "future_map_dbl", "future_map_chr", "future_map_lgl",
    "future_map_int", "future_map_dfr", "future_pmap"
  )

  # Collect global RNG safeguards across units
  has_lecuyer <- FALSE
  lecuyer_line <- Inf
  has_cluster_stream <- FALSE
  cluster_stream_line <- Inf
  has_dorng <- FALSE

  for (u in ctx$units) {
    rng_calls <- ast_calls(u$xml, "RNGkind")
    for (rc in rng_calls) {
      txt <- xml2::xml_text(rc)
      if (grepl("L'Ecuyer-CMRG", txt, fixed = TRUE)) {
        has_lecuyer <- TRUE
        lecuyer_line <- min(lecuyer_line, ast_line(rc, u$line_offset))
      }
    }
    cs_calls <- ast_calls(u$xml, "clusterSetRNGStream")
    if (length(cs_calls) > 0L) {
      has_cluster_stream <- TRUE
      cluster_stream_line <- min(cluster_stream_line, min(vapply(cs_calls, ast_line, integer(1L), offset = u$line_offset)))
    }
    dorng_calls <- ast_calls(u$xml, "registerDoRNG")
    if (length(dorng_calls) > 0L) {
      has_dorng <- TRUE
    }
  }

  findings <- list()

  for (u in ctx$units) {
    # (a) Fork-based parallel calls
    for (nd in ast_calls(u$xml, fork_fns)) {
      fn <- ast_call_name(nd)
      args <- ast_call_args(nd)
      ln <- ast_line(nd, u$line_offset)

      # Skip if explicitly mc.cores = 1 / 1L
      mc_cores <- Filter(function(a) identical(a$name, "mc.cores"), args)
      if (length(mc_cores) > 0L && trimws(mc_cores[[1L]]$text) %in% c("1", "1L")) {
        next
      }

      mc_seed <- Filter(function(a) identical(a$name, "mc.set.seed"), args)
      explicit_false <- length(mc_seed) > 0L && trimws(mc_seed[[1L]]$text) %in% c("FALSE", "F")

      if (explicit_false) {
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(nd),
          paste0("`", fn, "()` sets `mc.set.seed = FALSE`, causing forked workers to share identical RNG states.")
        )
      } else if (!has_lecuyer || ln < lecuyer_line) {
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(nd),
          paste0(
            "`", fn, "()` called without prior `RNGkind(\"L'Ecuyer-CMRG\")`; ",
            "child worker RNG streams may collide or vary across core counts."
          )
        )
      }
    }

    # (b) Socket/PSOCK cluster calls
    for (nd in ast_calls(u$xml, cluster_fns)) {
      fn <- ast_call_name(nd)
      ln <- ast_line(nd, u$line_offset)
      if (!has_cluster_stream || ln < cluster_stream_line) {
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(nd),
          paste0(
            "`", fn, "()` called without prior `parallel::clusterSetRNGStream()` ",
            "to initialize independent L'Ecuyer-CMRG worker streams."
          )
        )
      }
    }

    # (c) foreach %dopar% without registerDoRNG / %dorng%
    dopar_nodes <- xml2::xml_find_all(u$xml, ".//SPECIAL[text() = '%dopar%']")
    for (dp in dopar_nodes) {
      if (!has_dorng) {
        ln <- ast_line(dp, u$line_offset)
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(dp),
          "`foreach() %dopar%` used without `doRNG::registerDoRNG()` or `%dorng%`; parallel RNG streams are not reproducible."
        )
      }
    }

    # (d) future_* calls without future.seed / furrr_options(seed = ...)
    for (nd in ast_calls(u$xml, future_fns)) {
      fn <- ast_call_name(nd)
      args <- ast_call_args(nd)
      arg_names <- vapply(args, function(a) a$name %||% "", character(1L))
      has_seed_arg <- "future.seed" %in% arg_names
      if (!has_seed_arg) {
        opt_args <- Filter(function(a) identical(a$name, ".options"), args)
        if (length(opt_args) > 0L && grepl("seed[[:space:]]*=", opt_args[[1L]]$text)) {
          has_seed_arg <- TRUE
        }
      }
      if (!has_seed_arg) {
        ln <- ast_line(nd, u$line_offset)
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(nd),
          paste0("`", fn, "()` called without `future.seed = TRUE` (or `furrr_options(seed = TRUE)`).")
        )
      }
    }
  }

  findings
}

# 3. preprocessing_before_split
check_preprocessing_before_split <- function(ctx) {
  prep_fns <- c(
    "scale", "prcomp", "princomp", "preProcess", "mice",
    "missForest", "knnImputation", "normalize"
  )
  explicit_split_fns <- c(
    "initial_split", "group_initial_split", "createDataPartition",
    "vfold_cv", "createFolds", "initial_time_split"
  )

  # Collect splits across all units
  splits <- list()
  for (u in ctx$units) {
    # Explicit split functions
    for (sc in ast_calls(u$xml, explicit_split_fns)) {
      ln <- ast_line(sc, u$line_offset)
      syms <- unique(c(ast_symbols(sc), ast_symbols(ast_piped_lhs(sc))))
      splits[[length(splits) + 1L]] <- list(line = ln, symbols = syms, fn = ast_call_name(sc))
    }
    # Base R sample() / sample.int() used for train/test splitting
    for (sc in ast_calls(u$xml, c("sample", "sample.int"))) {
      ln <- ast_line(sc, u$line_offset)
      sc_calls <- ast_fn_calls_in(sc)
      assigned <- ast_assigned_var(sc)
      txt <- xml2::xml_text(sc)
      looks_like_split <- ("nrow" %in% sc_calls) ||
        (!is.na(assigned) && grepl("train|test|split|idx|fold|holdout|val", assigned, ignore.case = TRUE)) ||
        grepl("nrow\\(", txt)
      if (looks_like_split) {
        syms <- unique(ast_symbols(sc))
        splits[[length(splits) + 1L]] <- list(line = ln, symbols = syms, fn = ast_call_name(sc))
      }
    }
  }

  if (length(splits) == 0L) return(list())

  findings <- list()
  for (u in ctx$units) {
    for (pc in ast_calls(u$xml, prep_fns)) {
      ln <- ast_line(pc, u$line_offset)
      fn <- ast_call_name(pc)
      args <- ast_call_args(pc)
      piped <- ast_piped_lhs(pc)

      target_node <- if (!is.null(piped)) piped else if (length(args) >= 1L) args[[1L]]$expr else NULL
      if (is.null(target_node)) next
      target_txt <- xml2::xml_text(target_node)
      target_syms <- ast_symbols(target_node)
      assigned <- ast_assigned_var(pc)
      assigned_root <- if (!is.na(assigned)) sub("(\\$|\\[).*", "", assigned) else NA_character_

      # Walk enclosing mutate()/transmute()/within()/transform() calls so
      # `df2 <- df |> mutate(x = scale(x)); initial_split(df2)` links df/df2 cleanly
      enclosing_syms <- character(0)
      enclosing_target_txt <- ""
      anc_calls <- xml2::xml_find_all(pc, "ancestor::expr[expr[SYMBOL_FUNCTION_CALL]]")
      for (ac in anc_calls) {
        afn <- ast_call_name(ac)
        if (!is.na(afn) && afn %in% c("mutate", "transmute", "within", "transform")) {
          ac_assigned <- ast_assigned_var(ac)
          if (!is.na(ac_assigned)) {
            ac_root <- sub("(\\$|\\[).*", "", ac_assigned)
            enclosing_syms <- c(enclosing_syms, ac_root)
            enclosing_target_txt <- paste(enclosing_target_txt, ac_assigned)
          }
          ac_piped <- ast_piped_lhs(ac)
          if (!is.null(ac_piped)) {
            enclosing_syms <- c(enclosing_syms, ast_symbols(ac_piped))
            enclosing_target_txt <- paste(enclosing_target_txt, xml2::xml_text(ac_piped))
          } else {
            ac_args <- ast_call_args(ac)
            if (length(ac_args) >= 1L && is.na(ac_args[[1L]]$name) && !is.null(ac_args[[1L]]$expr)) {
              enclosing_syms <- c(enclosing_syms, ast_symbols(ac_args[[1L]]$expr))
              enclosing_target_txt <- paste(enclosing_target_txt, xml2::xml_text(ac_args[[1L]]$expr))
            }
          }
        }
      }

      # Skip if operating on an already-split training object or indexed by train/split
      combined_target <- paste(target_txt, enclosing_target_txt)
      if (grepl("train|trn|_tr\\b|\\btr_|fold", combined_target, ignore.case = TRUE)) next
      if (!is.na(assigned) && grepl("train|trn|_tr\\b|\\btr_", assigned, ignore.case = TRUE)) next
      # Skip if target uses row-subsetting with an index symbol (e.g. df[idx, ])
      if (grepl("\\[[^\\]]*,", target_txt) && grepl("idx|train|split|fold", target_txt, ignore.case = TRUE)) next

      # Check if any downstream split happens AFTER this preprocessing call on related data
      related_syms <- unique(c(target_syms, assigned_root, enclosing_syms))
      related_syms <- related_syms[!is.na(related_syms) & nzchar(related_syms)]

      leaked <- FALSE
      for (sp in splits) {
        if (sp$line > ln) {
          if (length(intersect(related_syms, sp$symbols)) > 0L) {
            leaked <- TRUE
            break
          }
        }
      }

      if (leaked) {
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(pc),
          paste0(
            "Potential train/test data leakage: `", fn, "()` is applied before ",
            "data splitting on the full dataset; fit preprocessing parameters on training data only."
          )
        )
      }
    }
  }

  findings
}

# 4. uncentered_mlm_interaction
check_uncentered_mlm_interaction <- function(ctx) {
  mlm_fns <- c("lmer", "glmer", "nlmer", "lme", "glmmTMB", "brm")
  centering_fns <- c("scale", "center", "centre", "ave", "demean", "center_within", "gmc", "cmc")
  centering_name_pat <- paste0(
    "(_c|_wc|_gmc|_cmc|_cw|_cgm|_cent|_centered|_mean|_bar|",
    "\\.c|\\.wc|\\.gmc|\\.cmc|\\.centered)$|",
    "^(c_|wc_|gmc_|cmc_|cent_|centered_|mean_)"
  )

  # Collect variables defined via centering or factor() across units
  centered_vars <- character(0)
  factor_vars <- character(0)

  for (u in ctx$units) {
    assigns <- ast_assignments(u$xml, u$line_offset)
    for (a in assigns) {
      # Extract column name if df$var <- ...
      var_short <- sub("^.*\\$", "", a$lhs)
      var_short <- gsub("[\"\\[\\]]", "", var_short)
      calls <- a$rhs_calls
      txt <- a$rhs_text
      if (any(calls %in% centering_fns) ||
          grepl("-[[:space:]]*(mean|median|ave)\\(", txt)) {
        centered_vars <- unique(c(centered_vars, a$lhs, var_short))
      }
      if (any(calls %in% c("factor", "as.factor", "ordered", "as.character", "ifelse"))) {
        factor_vars <- unique(c(factor_vars, a$lhs, var_short))
      }
    }
    # Also detect dplyr::mutate(x_c = x - mean(x), ...)
    for (mc in ast_calls(u$xml, c("mutate", "transmute", "within"))) {
      for (arg in ast_call_args(mc)) {
        if (!is.na(arg$name) && !is.null(arg$expr)) {
          calls <- ast_fn_calls_in(arg$expr)
          if (any(calls %in% centering_fns) ||
              grepl("-[[:space:]]*(mean|median|ave)\\(", arg$text)) {
            centered_vars <- unique(c(centered_vars, arg$name))
          }
          if (any(calls %in% c("factor", "as.factor", "ordered"))) {
            factor_vars <- unique(c(factor_vars, arg$name))
          }
        }
      }
    }
  }

  findings <- list()

  for (u in ctx$units) {
    for (call_nd in ast_calls(u$xml, mlm_fns)) {
      fn <- ast_call_name(call_nd)
      args <- ast_call_args(call_nd)
      if (length(args) == 0L) next

      # Locate formula expression
      form_arg <- Filter(function(a) identical(a$name, "formula") || identical(a$name, "fixed"), args)
      form_node <- if (length(form_arg) > 0L) form_arg[[1L]]$expr else args[[1L]]$expr
      if (is.null(form_node)) next

      # Must contain `~`
      tilde <- xml2::xml_find_first(form_node, ".//OP-TILDE")
      if (inherits(tilde, "xml_missing")) next

      # Must be a multilevel formula (contains `|` or `||` or `random =` argument)
      has_re <- !inherits(xml2::xml_find_first(form_node, ".//OR | .//OR2"), "xml_missing") ||
        any(vapply(args, function(a) identical(a$name, "random"), logical(1L)))
      if (!has_re) next

      # Find fixed-effect interaction nodes (* or :) NOT inside random-effect parens `(... | ...)`
      inter_nodes <- xml2::xml_find_all(
        form_node,
        ".//expr[(OP-STAR or OP-COLON) and not(ancestor::expr[OR or OR2])]"
      )
      if (length(inter_nodes) == 0L) next

      uncentered <- character(0)
      for (in_nd in inter_nodes) {
        # Skip if inside random effect (extra safety)
        if (!inherits(xml2::xml_find_first(in_nd, ".//OR | .//OR2"), "xml_missing")) next
        syms <- xml2::xml_find_all(in_nd, ".//SYMBOL")
        for (s in syms) {
          # Skip if wrapped inside scale(), center(), factor(), I(... - mean(...))
          if (ast_is_inside_call(s, c(centering_fns, "factor", "as.factor", "ordered", "I"))) next
          v <- xml2::xml_text(s)
          if (grepl(centering_name_pat, v, ignore.case = TRUE)) next
          if (v %in% centered_vars || v %in% factor_vars) next
          uncentered <- unique(c(uncentered, v))
        }
      }

      if (length(uncentered) > 0L) {
        ln <- ast_line(call_nd, u$line_offset)
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(call_nd),
          paste0(
            "Multilevel interaction in `", fn, "()` uses uncentered predictor(s): ",
            paste(paste0("`", uncentered, "`"), collapse = ", "),
            ". Center Level-1/Level-2 predictors (e.g., cluster-mean or grand-mean centering) ",
            "to prevent contextual confounding and aid interpretation."
          )
        )
      }
    }
  }

  findings
}

# 5. silent_listwise_mismatch
check_silent_listwise_mismatch <- function(ctx) {
  compare_fns <- c("anova", "AIC", "BIC", "lrtest", "compare_performance")
  model_fns <- c("lm", "glm", "lmer", "glmer", "nlmer", "lme", "glmmTMB", "glm.nb", "aov")
  clean_fns <- c("na.omit", "na.exclude", "drop_na", "complete.cases", "na.fail")

  # Collect cleaned data frame names & fitted model metadata
  cleaned_dfs <- character(0)
  models <- list()

  for (u in ctx$units) {
    assigns <- ast_assignments(u$xml, u$line_offset)
    for (a in assigns) {
      has_not_isna <- "is.na" %in% a$rhs_calls &&
        !inherits(xml2::xml_find_first(a$rhs_node, ".//OP-EXCLAMATION"), "xml_missing")
      if (any(a$rhs_calls %in% clean_fns) || has_not_isna) {
        cleaned_dfs <- unique(c(cleaned_dfs, a$lhs_root))
      }
      # Check if RHS is a model call
      rhs_calls <- ast_calls(a$rhs_node, model_fns)
      if (length(rhs_calls) >= 1L) {
        mc <- rhs_calls[[1L]]
        margs <- ast_call_args(mc)
        form_arg <- Filter(function(x) identical(x$name, "formula"), margs)
        form_node <- if (length(form_arg) > 0L) form_arg[[1L]]$expr else if (length(margs) >= 1L) margs[[1L]]$expr else NULL
        data_arg <- Filter(function(x) identical(x$name, "data"), margs)
        data_node <- if (length(data_arg) > 0L) data_arg[[1L]]$expr else if (length(margs) >= 2L && is.na(margs[[2L]]$name)) margs[[2L]]$expr else NULL
        na_arg <- Filter(function(x) identical(x$name, "na.action"), margs)
        has_na_fail <- length(na_arg) > 0L && !is.null(na_arg[[1L]]$expr) &&
          ("na.fail" %in% c(ast_symbols(na_arg[[1L]]$expr), ast_fn_calls_in(na_arg[[1L]]$expr)))
        data_calls <- if (!is.null(data_node)) ast_fn_calls_in(data_node) else character(0)
        data_inline_clean <- any(data_calls %in% clean_fns)
        data_syms <- if (!is.null(data_node)) ast_symbols(data_node) else character(0)
        data_root <- if (length(data_syms) >= 1L) data_syms[1L] else NA_character_
        form_vars <- if (!is.null(form_node)) sort(unique(ast_symbols(form_node))) else character(0)

        models[[a$lhs_root]] <- list(
          name = a$lhs_root,
          data_root = data_root,
          data_clean = data_inline_clean || (!is.na(data_root) && data_root %in% cleaned_dfs),
          has_na_fail = has_na_fail,
          form_vars = form_vars,
          line = a$line
        )
      }
    }
  }

  findings <- list()
  for (u in ctx$units) {
    for (cc in ast_calls(u$xml, compare_fns)) {
      args <- ast_call_args(cc)
      pos_args <- Filter(function(a) is.na(a$name) && !is.null(a$expr), args)
      if (length(pos_args) < 2L) next

      # Extract model variable names
      m_names <- vapply(pos_args, function(a) {
        syms <- ast_symbols(a$expr)
        if (length(syms) == 1L) syms[1L] else NA_character_
      }, character(1L))
      m_names <- m_names[!is.na(m_names) & m_names %in% names(models)]
      if (length(m_names) < 2L) next

      minfos <- models[m_names]
      # Check if models share a raw (uncleaned) dataset and differ in variables
      data_roots <- unique(vapply(minfos, `[[`, character(1L), "data_root"))
      data_roots <- data_roots[!is.na(data_roots)]
      if (length(data_roots) != 1L) next

      all_protected <- all(vapply(minfos, function(m) isTRUE(m$data_clean) || isTRUE(m$has_na_fail), logical(1L)))
      if (all_protected) next

      var_sets <- lapply(minfos, `[[`, "form_vars")
      differs <- FALSE
      for (i in seq_len(length(var_sets) - 1L)) {
        if (!setequal(var_sets[[i]], var_sets[[i + 1L]])) {
          differs <- TRUE
          break
        }
      }

      if (differs) {
        fn <- ast_call_name(cc)
        ln <- ast_line(cc, u$line_offset)
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(cc),
          paste0(
            "`", fn, "(", paste(m_names, collapse = ", "), ")` compares models with different ",
            "variable sets on unharmonized dataset `", data_roots[1L], "`. ",
            "Missing values in added predictors can cause silent listwise sample-size mismatches; ",
            "harmonize complete cases first (e.g., `na.omit()` or `complete.cases()`)."
          )
        )
      }
    }
  }
  findings
}

# 6. silent_factor_drop
check_silent_factor_drop <- function(ctx) {
  factor_creators <- c(
    "factor", "as.factor", "ordered", "as.ordered", "cut",
    "fct_relevel", "fct_lump", "fct_recode", "fct_infreq", "fct_rev"
  )
  numeric_coercions <- c("as.numeric", "as.integer", "as.double")
  model_contrast_fns <- c("lm", "glm", "lmer", "glmer", "aov", "contrasts", "table", "xtabs", "model.matrix")

  factor_vars <- character(0)
  subset_dfs <- list()
  dropped_dfs <- character(0)

  for (u in ctx$units) {
    assigns <- ast_assignments(u$xml, u$line_offset)
    for (a in assigns) {
      if (any(a$rhs_calls %in% factor_creators)) {
        var_short <- sub("^.*\\$", "", a$lhs)
        factor_vars <- unique(c(factor_vars, a$lhs, var_short))
      }
      if (any(a$rhs_calls %in% c("droplevels", "fct_drop"))) {
        dropped_dfs <- unique(c(dropped_dfs, a$lhs_root))
      }
      # Detect subset()/filter() or bracket subsetting on equality/inequality/%in%
      is_sub <- any(a$rhs_calls %in% c("subset", "filter")) ||
        (grepl("\\[", a$rhs_text) && grepl("!=|==|%in%", a$rhs_text))
      if (is_sub && !any(a$rhs_calls %in% c("droplevels", "fct_drop"))) {
        subset_dfs[[a$lhs_root]] <- a$line
      }
    }
    # Also check mutate(g = factor(...))
    for (mc in ast_calls(u$xml, "mutate")) {
      for (arg in ast_call_args(mc)) {
        if (!is.na(arg$name) && !is.null(arg$expr) && any(ast_fn_calls_in(arg$expr) %in% factor_creators)) {
          factor_vars <- unique(c(factor_vars, arg$name))
        }
      }
    }
  }

  findings <- list()

  for (u in ctx$units) {
    # (a) as.numeric(factor_var) without as.character() (supports direct & piped calls)
    for (nc in ast_calls(u$xml, numeric_coercions)) {
      args <- ast_call_args(nc)
      arg_expr <- if (length(args) >= 1L && !is.null(args[[1L]]$expr)) {
        args[[1L]]$expr
      } else {
        ast_piped_lhs(nc)
      }
      if (is.null(arg_expr)) next
      arg_calls <- ast_fn_calls_in(arg_expr)
      # Safe if wrapped in as.character() or levels()
      if (any(arg_calls %in% c("as.character", "levels"))) next

      arg_txt <- xml2::xml_text(arg_expr)
      arg_syms <- ast_symbols(arg_expr)
      is_direct_factor_call <- any(arg_calls %in% factor_creators)
      is_known_factor_var <- (arg_txt %in% factor_vars) ||
        (length(arg_syms) >= 1L && tail_chr(arg_syms) %in% factor_vars)

      if (is_direct_factor_call || is_known_factor_var) {
        fn <- ast_call_name(nc)
        ln <- ast_line(nc, u$line_offset)
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(nc),
          paste0(
            "`", fn, "(", arg_txt, ")` coerces a factor to its internal integer level codes ",
            "rather than its labels; use `", fn, "(as.character(...))` instead."
          )
        )
      }
    }

    # (b) Modeling / contrasts on subsetted data frame without droplevels()
    flagged_sub_uses <- character(0)
    for (mc in ast_calls(u$xml, model_contrast_fns)) {
      ln <- ast_line(mc, u$line_offset)
      args <- ast_call_args(mc)
      fn <- ast_call_name(mc)
      # Check `data = sub_df` or first arg for contrasts/table
      candidate_nodes <- list()
      data_arg <- Filter(function(a) identical(a$name, "data"), args)
      if (length(data_arg) > 0L && !is.null(data_arg[[1L]]$expr)) {
        candidate_nodes[[1L]] <- data_arg[[1L]]$expr
      } else if (fn %in% c("contrasts", "table", "xtabs")) {
        for (a in args) if (!is.null(a$expr)) candidate_nodes[[length(candidate_nodes) + 1L]] <- a$expr
      }
      for (cn in candidate_nodes) {
        if (any(ast_fn_calls_in(cn) %in% c("droplevels", "fct_drop"))) next
        syms <- ast_symbols(cn)
        hit <- intersect(syms, setdiff(names(subset_dfs), dropped_dfs))
        hit <- hit[vapply(hit, function(h) subset_dfs[[h]] < ln, logical(1L))]
        if (length(hit) > 0L) {
          key <- paste0(ln, ":", hit[1L])
          if (!(key %in% flagged_sub_uses)) {
            flagged_sub_uses <- c(flagged_sub_uses, key)
            findings[[length(findings) + 1L]] <- make_finding(
              ctx, ln, ast_col(mc),
              paste0(
                "Subsetted data `", hit[1L], "` is passed to `", fn, "()` without `droplevels()` ",
                "or `fct_drop()`; unused factor levels may silently persist in contrasts or tables."
              )
            )
          }
        }
      }
    }
  }

  findings
}

tail_chr <- function(x) {
  if (length(x) == 0L) NA_character_ else x[length(x)]
}

# 7. equality_on_float
check_equality_on_float <- function(ctx) {
  float_fns <- c(
    "mean", "sd", "var", "cor", "cov", "sqrt", "log", "log10", "log2",
    "exp", "sin", "cos", "tan", "pnorm", "qnorm", "dnorm", "pt", "qt",
    "coef", "predict", "fitted", "residuals", "weighted.mean"
  )

  is_float_literal <- function(txt) {
    # Matches decimals like 0.3, .5, 1.0, or scientific with negative exponent 1e-5
    grepl("^[0-9]*\\.[0-9]+([eE][+-]?[0-9]+)?$", txt) ||
      grepl("^[0-9]+\\.$", txt) ||
      grepl("^[0-9]+\\.?[0-9]*[eE]-[0-9]+$", txt)
  }

  is_float_expr <- function(node) {
    if (is.null(node)) return(FALSE)
    # Contains any floating-point numeric literal
    nums <- xml2::xml_text(xml2::xml_find_all(node, ".//NUM_CONST"))
    if (any(vapply(nums, is_float_literal, logical(1L)))) return(TRUE)
    # Contains division `/`
    if (!inherits(xml2::xml_find_first(node, ".//OP-SLASH"), "xml_missing")) return(TRUE)
    # Direct call to a continuous floating-point statistical/math function
    calls <- ast_fn_calls_in(node)
    if (any(calls %in% float_fns)) return(TRUE)
    FALSE
  }

  findings <- list()
  for (u in ctx$units) {
    eq_nodes <- xml2::xml_find_all(u$xml, ".//expr[EQ or NE]")
    for (nd in eq_nodes) {
      ch <- xml2::xml_children(nd)
      if (length(ch) != 3L) next
      lhs <- ch[[1L]]
      op <- xml2::xml_text(ch[[2L]])
      rhs <- ch[[3L]]

      # Skip direct string / NULL / NA literal comparisons (without skipping df[["x"]] == 0.5)
      is_direct_str_or_null <- function(e) {
        !inherits(xml2::xml_find_first(e, "./STR_CONST | ./NULL_CONST"), "xml_missing") ||
          xml2::xml_text(e) %in% c("NA", "NA_real_", "NA_integer_", "NA_character_")
      }
      if (is_direct_str_or_null(lhs) || is_direct_str_or_null(rhs)) next

      if (is_float_expr(lhs) || is_float_expr(rhs)) {
        ln <- ast_line(nd, u$line_offset)
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(nd),
          paste0(
            "Exact floating-point comparison (`", op, "`) on `",
            xml2::xml_text(nd), "`. Use `isTRUE(all.equal(...))` or `abs(a - b) < tol` instead."
          )
        )
      }
    }
  }
  findings
}

# 8. unadjusted_multiple_testing
check_unadjusted_multiple_testing <- function(ctx) {
  test_fns <- c(
    "t.test", "wilcox.test", "cor.test", "chisq.test", "fisher.test",
    "ks.test", "kruskal.test", "oneway.test", "prop.test", "binom.test",
    "shapiro.test", "var.test"
  )
  adjust_fns <- c(
    "p.adjust", "qvalue", "TukeyHSD", "pairwise.t.test",
    "pairwise.wilcox.test", "pairwise.prop.test", "glht", "emmeans",
    "mt.rawp2adjp"
  )
  loop_fns <- c("lapply", "sapply", "vapply", "apply", "map", "map_dbl", "map_dfr", "pmap")

  # Check if any adjustment exists in the document
  for (u in ctx$units) {
    all_calls <- ast_fn_calls_in(u$xml)
    if (any(all_calls %in% adjust_fns)) {
      return(list())
    }
  }

  has_hyp_test <- function(body_node) {
    calls <- ast_fn_calls_in(body_node)
    if (any(calls %in% test_fns)) return(TRUE)
    if (any(calls %in% c("lm", "glm", "lmer", "glmer", "summary"))) {
      dollar_p <- xml2::xml_find_all(body_node, ".//expr[OP-DOLLAR]/SYMBOL[text()='p.value']")
      sub_strs <- xml2::xml_text(xml2::xml_find_all(body_node, ".//expr[OP-LEFT-BRACKET or LBB]//STR_CONST"))
      has_sub_p <- any(grepl("^['\"](p\\.value|Pr\\(>\\|[tz]\\|\\))['\"]$", sub_strs))
      if (length(dollar_p) > 0L || has_sub_p) return(TRUE)
    }
    FALSE
  }

  findings <- list()
  for (u in ctx$units) {
    # for (...) loops
    for_nodes <- xml2::xml_find_all(u$xml, ".//expr[FOR]")
    for (fn_nd in for_nodes) {
      if (has_hyp_test(fn_nd)) {
        ln <- ast_line(fn_nd, u$line_offset)
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(fn_nd),
          "Hypothesis testing executed inside a `for` loop without `p.adjust()` or multiple-testing correction in scope."
        )
      }
    }
    # lapply / sapply / map loops
    for (lc in ast_calls(u$xml, loop_fns)) {
      if (has_hyp_test(lc)) {
        ln <- ast_line(lc, u$line_offset)
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(lc),
          paste0(
            "Repeated hypothesis testing inside `", ast_call_name(lc),
            "()` without `p.adjust()` or multiple-testing correction in scope."
          )
        )
      }
    }
  }
  findings
}

# 9. missing_convergence_check (pure AST: immune to comments/string spoofing)
check_missing_convergence_check <- function(ctx) {
  mlm_opt_fns <- c("lmer", "glmer", "nlmer")
  optim_fns <- c("optim", "nlminb", "nlm")
  other_iter_fns <- c("glmmTMB", "glm.nb")

  # Inspect actual AST nodes across units (never raw comments or bare strings)
  all_calls <- character(0)
  all_slots <- character(0)
  all_dollars <- character(0)
  all_lbb_keys <- character(0)

  for (u in ctx$units) {
    all_calls <- c(all_calls, ast_fn_calls_in(u$xml))
    all_slots <- c(all_slots, xml2::xml_text(xml2::xml_find_all(u$xml, ".//expr[OP-AT]/SLOT")))
    all_dollars <- c(all_dollars, xml2::xml_text(xml2::xml_find_all(u$xml, ".//expr[OP-DOLLAR]/SYMBOL")))
    lbb_strs <- xml2::xml_text(xml2::xml_find_all(u$xml, ".//expr[LBB]/expr[2]/STR_CONST"))
    all_lbb_keys <- c(all_lbb_keys, gsub("^['\"]|['\"]$", "", lbb_strs))
  }

  has_mlm_check <- any(all_calls %in% c("isSingular", "check_singularity", "check_convergence", "allFit", "audit_estimator")) ||
    any(all_slots %in% c("optinfo", "conv", "checkConv")) ||
    any(all_dollars %in% c("optinfo", "conv", "checkConv", "convergence")) ||
    any(all_lbb_keys %in% c("optinfo", "conv", "convergence"))

  has_optim_check <- any(all_calls %in% c("check_convergence", "audit_estimator")) ||
    any(all_dollars %in% c("convergence", "code")) ||
    any(all_lbb_keys %in% c("convergence", "code"))

  has_other_check <- any(all_calls %in% c("check_convergence", "audit_estimator")) ||
    any(all_dollars %in% c("pdHess", "convergence", "converged")) ||
    any(all_lbb_keys %in% c("pdHess", "convergence", "converged"))

  findings <- list()
  for (u in ctx$units) {
    for (nd in ast_calls(u$xml, c(mlm_opt_fns, optim_fns, other_iter_fns))) {
      fn <- ast_call_name(nd)
      ln <- ast_line(nd, u$line_offset)
      if (fn %in% mlm_opt_fns && !has_mlm_check) {
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(nd),
          paste0(
            "`", fn, "()` fitted without checking `lme4::isSingular()` or `@optinfo$conv`; ",
            "boundary/singular fits or optimizer non-convergence may go undetected."
          )
        )
      } else if (fn %in% optim_fns && !has_optim_check) {
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(nd),
          paste0(
            "`", fn, "()` called without checking `$convergence` (or `$code`); ",
            "failed optimization can silently return non-stationary parameter estimates."
          )
        )
      } else if (fn %in% other_iter_fns && !has_other_check) {
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(nd),
          paste0("`", fn, "()` fitted without checking convergence status (`$converged` / `pdHess`).")
        )
      }
    }
  }
  findings
}

# 10. global_seed_override_in_fn (supports function() & \(...), pure AST seed-restore check)
check_global_seed_override_in_fn <- function(ctx) {
  findings <- list()
  for (u in ctx$units) {
    fn_nodes <- xml2::xml_find_all(u$xml, ".//expr[FUNCTION or OP-LAMBDA]")
    for (fn_nd in fn_nodes) {
      seed_calls <- ast_calls(fn_nd, "set.seed")
      if (length(seed_calls) == 0L) next

      # Only consider calls belonging directly to this function (not nested inner functions)
      fn_path <- xml2::xml_path(fn_nd)
      own_calls <- Filter(function(c_nd) {
        nearest <- xml2::xml_find_first(c_nd, "ancestor::expr[FUNCTION or OP-LAMBDA][1]")
        identical(xml2::xml_path(nearest), fn_path)
      }, ast_calls(fn_nd))

      own_call_names <- vapply(own_calls, ast_call_name, character(1L))
      restores_seed <- any(own_call_names %in% c("local_seed", "local_preserve_seed", "with_preserve_seed"))

      if (!restores_seed && "on.exit" %in% own_call_names) {
        # Identify variables assigned from the actual `.Random.seed` symbol in this function
        fn_assigns <- ast_assignments(fn_nd, u$line_offset)
        saved_seed_vars <- unique(vapply(Filter(function(a) ".Random.seed" %in% a$rhs_symbols, fn_assigns),
                                         `[[`, character(1L), "lhs_root"))
        onexit_calls <- Filter(function(c_nd) identical(ast_call_name(c_nd), "on.exit"), own_calls)
        for (oe in onexit_calls) {
          oe_syms <- ast_symbols(oe)
          if (".Random.seed" %in% oe_syms || length(intersect(oe_syms, saved_seed_vars)) > 0L) {
            restores_seed <- TRUE
            break
          }
        }
      }

      if (restores_seed) next

      for (sc in seed_calls) {
        nearest_fn <- xml2::xml_find_first(sc, "ancestor::expr[FUNCTION or OP-LAMBDA][1]")
        if (!identical(xml2::xml_path(nearest_fn), fn_path)) next
        ln <- ast_line(sc, u$line_offset)
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(sc),
          "`set.seed()` called inside a function without restoring `.Random.seed` via `on.exit()` or `withr::local_seed()`."
        )
      }
    }
  }
  findings
}

# 14. duplicate_parallel_seed
check_duplicate_parallel_seed <- function(ctx) {
  iter_fns <- c(
    "lapply", "sapply", "vapply", "apply", "mclapply", "mcmapply",
    "parLapply", "parSapply", "parApply", "clusterApply", "clusterApplyLB",
    "future_lapply", "future_sapply", "future_map", "future_map_dbl",
    "map", "map_dbl", "map_dfr", "map_chr", "map_int", "replicate"
  )

  findings <- list()
  for (u in ctx$units) {
    seed_calls <- ast_calls(u$xml, "set.seed")
    for (sc in seed_calls) {
      in_for <- !inherits(xml2::xml_find_first(sc, "ancestor::expr[FOR]"), "xml_missing")
      in_foreach <- !inherits(
        xml2::xml_find_first(sc, "ancestor::expr[SPECIAL[text()='%dopar%' or text()='%do%' or text()='%dorng%']]"),
        "xml_missing"
      )
      in_iter_call <- ast_is_inside_call(sc, iter_fns)
      if (!in_for && !in_foreach && !in_iter_call) next

      args <- ast_call_args(sc)
      if (length(args) < 1L || is.null(args[[1L]]$expr)) next
      seed_expr <- args[[1L]]$expr
      # Constant seed expression (contains zero variable symbols -> identical across every iteration/worker!)
      if (length(ast_symbols(seed_expr)) == 0L) {
        ln <- ast_line(sc, u$line_offset)
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(sc),
          paste0(
            "Constant `set.seed(", xml2::xml_text(seed_expr), ")` inside loop or parallel worker ",
            "produces identical duplicate draws on every iteration; derive distinct per-iteration L'Ecuyer streams instead."
          )
        )
      }
    }
  }
  findings
}

# 11. short_logical_literal
check_short_logical_literal <- function(ctx) {
  findings <- list()
  arith_ops <- c("OP-PLUS", "OP-MINUS", "OP-STAR", "OP-SLASH", "OP-COLON", "OP-CARET", "LT", "GT", "LE", "GE")

  for (u in ctx$units) {
    # Top-level bindings of T/F apply across the unit; function-local formals/assigns
    # and for-loop variables are scoped strictly to their enclosing function/loop AST node!
    top_bound <- character(0)
    assign_nodes <- xml2::xml_find_all(
      u$xml,
      ".//*[self::expr or self::equal_assign][LEFT_ASSIGN or EQ_ASSIGN or RIGHT_ASSIGN]"
    )
    fn_bindings <- list()
    for (an in assign_nodes) {
      ch <- xml2::xml_children(an)
      if (length(ch) != 3L) next
      op <- xml2::xml_name(ch[[2L]])
      lhs_txt <- if (op == "RIGHT_ASSIGN") xml2::xml_text(ch[[3L]]) else xml2::xml_text(ch[[1L]])
      if (!(lhs_txt %in% c("T", "F"))) next
      enc_fn <- xml2::xml_find_first(an, "ancestor::expr[FUNCTION or OP-LAMBDA][1]")
      if (inherits(enc_fn, "xml_missing")) {
        top_bound <- unique(c(top_bound, lhs_txt))
      } else {
        fp <- xml2::xml_path(enc_fn)
        fn_bindings[[fp]] <- unique(c(fn_bindings[[fp]], lhs_txt))
      }
    }

    # Formal parameters inside each function/lambda
    fn_nodes <- xml2::xml_find_all(u$xml, ".//expr[FUNCTION or OP-LAMBDA]")
    for (fn_nd in fn_nodes) {
      fms <- xml2::xml_text(xml2::xml_find_all(fn_nd, "./SYMBOL_FORMALS"))
      hits <- intersect(fms, c("T", "F"))
      if (length(hits) > 0L) {
        fp <- xml2::xml_path(fn_nd)
        fn_bindings[[fp]] <- unique(c(fn_bindings[[fp]], hits))
      }
    }

    # Loop variables scoped strictly to their enclosing `for` expression
    loop_bindings <- list()
    for_nodes <- xml2::xml_find_all(u$xml, ".//expr[FOR]")
    for (fl in for_nodes) {
      lv <- xml2::xml_text(xml2::xml_find_all(fl, "./forcond/SYMBOL"))
      hits <- intersect(lv, c("T", "F"))
      if (length(hits) > 0L) {
        loop_bindings[[xml2::xml_path(fl)]] <- unique(c(loop_bindings[[xml2::xml_path(fl)]], hits))
      }
    }

    is_lexically_bound <- function(sn, sym_txt) {
      if (sym_txt %in% top_bound) return(TRUE)
      if (length(fn_bindings) > 0L) {
        anc_fns <- xml2::xml_find_all(sn, "ancestor::expr[FUNCTION or OP-LAMBDA]")
        for (af in anc_fns) {
          b <- fn_bindings[[xml2::xml_path(af)]]
          if (!is.null(b) && sym_txt %in% b) return(TRUE)
        }
      }
      if (length(loop_bindings) > 0L) {
        anc_fors <- xml2::xml_find_all(sn, "ancestor::expr[FOR]")
        for (al in anc_fors) {
          b <- loop_bindings[[xml2::xml_path(al)]]
          if (!is.null(b) && sym_txt %in% b) return(TRUE)
        }
      }
      FALSE
    }

    sym_nodes <- xml2::xml_find_all(u$xml, ".//SYMBOL[text() = 'T' or text() = 'F']")
    for (sn in sym_nodes) {
      txt <- xml2::xml_text(sn)
      if (is_lexically_bound(sn, txt)) next
      # Skip column/slot accesses like df$T or obj@F
      parent <- xml2::xml_parent(sn)
      if (!inherits(parent, "xml_missing")) {
        p_ch_names <- xml2::xml_name(xml2::xml_children(parent))
        if (any(p_ch_names %in% c("OP-DOLLAR", "OP-AT"))) next
        # Check grandparent too (SYMBOL sits inside <expr>)
        gp <- xml2::xml_parent(parent)
        if (!inherits(gp, "xml_missing")) {
          gp_ch <- xml2::xml_name(xml2::xml_children(gp))
          if (any(gp_ch %in% c("OP-DOLLAR", "OP-AT", arith_ops))) next
        }
      }
      full <- if (txt == "T") "TRUE" else "FALSE"
      ln <- ast_line(sn, u$line_offset)
      findings[[length(findings) + 1L]] <- make_finding(
        ctx, ln, ast_col(sn),
        paste0(
          "Mutable symbol `", txt, "` used instead of `", full, "`. ",
          "`T`/`F` can be overwritten by variables and silently corrupt statistical arguments."
        )
      )
    }
  }
  findings
}

# 12. cor_without_use_arg
check_cor_without_use_arg <- function(ctx) {
  findings <- list()
  for (u in ctx$units) {
    for (nd in ast_calls(u$xml, c("cor", "cov"))) {
      fn <- ast_call_name(nd)
      args <- ast_call_args(nd)
      arg_names <- vapply(args, function(a) a$name %||% "", character(1L))
      has_use <- "use" %in% arg_names
      if (!has_use) {
        # Positional 3rd arg in cor(x, y, "complete.obs")
        pos_args <- Filter(function(a) is.na(a$name), args)
        if (length(pos_args) >= 3L && grepl("everything|all\\.obs|complete\\.obs|na\\.or\\.complete|pairwise", pos_args[[3L]]$text)) {
          has_use <- TRUE
        }
      }
      if (!has_use) {
        ln <- ast_line(nd, u$line_offset)
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(nd),
          paste0(
            "`", fn, "()` called without an explicit `use =` argument; ",
            "default `use = \"everything\"` silently propagates `NA`s across results. ",
            "Specify e.g. `use = \"complete.obs\"` or `use = \"pairwise.complete.obs\"`."
          )
        )
      }
    }
  }
  findings
}

# 13. absolute_path_in_script
check_absolute_path_in_script <- function(ctx) {
  io_fns <- c(
    "read.csv", "read.csv2", "read.table", "read.delim", "readRDS", "saveRDS",
    "load", "save", "source", "read_csv", "read_tsv", "read_delim",
    "read_excel", "read_xlsx", "read_parquet", "read_feather",
    "read_sav", "read_dta", "read_sas", "fread", "fwrite",
    "write.csv", "write.table", "write_csv", "writeLines", "readLines",
    "ggsave", "pdf", "png", "jpeg", "svg", "file", "setwd", "list.files"
  )
  abs_pat <- "^(/Users/|/home/|[A-Za-z]:[/\\\\]|~/|\\\\\\\\[^\\\\]+\\\\)"

  findings <- list()
  for (u in ctx$units) {
    for (nd in ast_calls(u$xml, io_fns)) {
      fn <- ast_call_name(nd)
      strs <- xml2::xml_find_all(nd, ".//STR_CONST")
      for (st in strs) {
        raw <- xml2::xml_text(st)
        val <- gsub("^['\"]|['\"]$", "", raw)
        is_bad <- grepl(abs_pat, val) || (fn == "setwd" && grepl("^/", val))
        if (is_bad) {
          ln <- ast_line(st, u$line_offset)
          findings[[length(findings) + 1L]] <- make_finding(
            ctx, ln, ast_col(st),
            paste0(
              "Hardcoded non-portable path ", raw, " inside `", fn, "()` breaks CI/reproducibility; ",
              "use project-relative paths (e.g., `file.path()` or `here::here()`)."
            )
          )
        }
      }
    }
  }
  findings
}

# 15. silent_na_coercion
check_silent_na_coercion <- function(ctx) {
  num_coercions <- c("as.numeric", "as.integer", "as.double")
  str_producers <- c(
    "character", "readLines", "strsplit", "gsub", "sub", "trimws",
    "paste", "paste0", "str_extract", "str_replace", "str_trim",
    "str_remove", "str_split", "substr", "substring"
  )
  factor_creators <- c(
    "factor", "as.factor", "ordered", "as.ordered", "cut",
    "fct_relevel", "fct_lump", "fct_recode"
  )
  na_check_fns <- c("is.na", "anyNA", "complete.cases", "na.omit", "na.fail")

  factor_vars <- character(0)
  char_vars <- character(0)
  na_checked_vars <- character(0)
  has_any_na_check <- FALSE

  for (u in ctx$units) {
    all_calls <- ast_fn_calls_in(u$xml)
    if (any(all_calls %in% na_check_fns)) has_any_na_check <- TRUE

    assigns <- ast_assignments(u$xml, u$line_offset)
    for (a in assigns) {
      var_short <- sub("^.*\\$", "", a$lhs)
      if (any(a$rhs_calls %in% factor_creators)) {
        factor_vars <- unique(c(factor_vars, a$lhs, var_short))
      } else {
        has_str_lit <- !inherits(xml2::xml_find_first(a$rhs_node, ".//STR_CONST"), "xml_missing") &&
          !any(a$rhs_calls %in% c("lm", "glm", "lmer", "read.csv", "readRDS", "file.path", "factor", "as.factor", "ordered"))
        is_str_call <- any(a$rhs_calls %in% str_producers) ||
          ("as.character" %in% a$rhs_calls && length(intersect(a$rhs_symbols, factor_vars)) == 0L)
        if (is_str_call || (has_str_lit && (length(a$rhs_calls) == 0L || all(a$rhs_calls == "c")))) {
          char_vars <- unique(c(char_vars, a$lhs, var_short))
        }
      }
    }

    for (nc in ast_calls(u$xml, na_check_fns)) {
      na_checked_vars <- unique(c(na_checked_vars, ast_symbols(nc)))
    }
  }

  findings <- list()
  for (u in ctx$units) {
    for (nd in ast_calls(u$xml, num_coercions)) {
      if (ast_is_inside_call(nd, "suppressWarnings")) next
      args <- ast_call_args(nd)
      arg_expr <- if (length(args) >= 1L && !is.null(args[[1L]]$expr)) {
        args[[1L]]$expr
      } else {
        ast_piped_lhs(nd)
      }
      if (is.null(arg_expr)) next
      arg_calls <- ast_fn_calls_in(arg_expr)
      arg_syms <- ast_symbols(arg_expr)
      arg_txt <- xml2::xml_text(arg_expr)

      # Exempt as.numeric(as.character(factor_var)) — recommended fix for silent_factor_drop
      if ("as.character" %in% arg_calls &&
          (length(intersect(arg_syms, factor_vars)) > 0L || any(arg_calls %in% factor_creators))) {
        next
      }

      is_char_lit <- !inherits(xml2::xml_find_first(arg_expr, ".//STR_CONST"), "xml_missing") &&
        (length(arg_calls) == 0L || all(arg_calls %in% c("c", str_producers)))
      is_str_prod <- any(arg_calls %in% str_producers)
      is_char_var <- (arg_txt %in% char_vars) || (length(arg_syms) >= 1L && tail_chr(arg_syms) %in% char_vars)

      if (!(is_char_lit || is_str_prod || is_char_var)) next

      assigned <- ast_assigned_var(nd)
      assigned_short <- if (!is.na(assigned)) sub("^.*\\$", "", assigned) else NA_character_
      is_checked <- if (!is.na(assigned)) {
        assigned %in% na_checked_vars || assigned_short %in% na_checked_vars
      } else {
        has_any_na_check
      }
      if (is_checked) next

      fn <- ast_call_name(nd)
      ln <- ast_line(nd, u$line_offset)
      findings[[length(findings) + 1L]] <- make_finding(
        ctx, ln, ast_col(nd),
        paste0(
          "`", fn, "()` coerces character/parsed string input without checking `anyNA()` or `is.na()`; ",
          "unparseable tokens silently turn into `NA`."
        )
      )
    }
  }
  findings
}

# 16. stepwise_selection_unvalidated
check_stepwise_selection_unvalidated <- function(ctx) {
  step_fns <- c(
    "step", "stepAIC", "regsubsets",
    "ols_step_both_p", "ols_step_forward_p", "ols_step_backward_p",
    "ols_step_both_aic", "ols_step_forward_aic", "ols_step_backward_aic"
  )
  val_fns <- c(
    "initial_split", "group_initial_split", "vfold_cv", "createDataPartition",
    "createFolds", "trainControl", "cv.glmnet", "boot", "bootstraps",
    "validate", "calibrate"
  )
  train_tok_pat <- "(^|_)(train|trn|training|cv_fold|split|holdout)(_|$)"

  has_formal_validation <- FALSE
  split_syms <- character(0)
  model_train_flags <- list()
  for (u in ctx$units) {
    calls <- ast_fn_calls_in(u$xml)
    if (any(calls %in% val_fns)) {
      has_formal_validation <- TRUE
      break
    }
    # Base R train/test split via sample()/sample.int(): track split-associated symbols
    for (sc in ast_calls(u$xml, c("sample", "sample.int"))) {
      assigned <- ast_assigned_var(sc)
      if (!is.na(assigned) && grepl(train_tok_pat, assigned, ignore.case = TRUE)) {
        split_syms <- unique(c(split_syms, assigned, ast_symbols(sc)))
      }
    }
    # Track models fitted on training splits (e.g., mod <- lm(y ~ ., data = train))
    for (a in ast_assignments(u$xml, u$line_offset)) {
      if (any(grepl(train_tok_pat, a$rhs_symbols, ignore.case = TRUE)) ||
          length(intersect(a$rhs_symbols, split_syms)) > 0L) {
        model_train_flags[[a$lhs_root]] <- TRUE
      }
    }
  }

  if (has_formal_validation) return(list())

  findings <- list()
  for (u in ctx$units) {
    for (nd in ast_calls(u$xml, step_fns)) {
      fn <- ast_call_name(nd)
      # Skip only if operating on an explicitly split/training object (word/token boundary)
      syms <- ast_symbols(nd)
      if (any(grepl(train_tok_pat, syms, ignore.case = TRUE))) next
      if (any(syms %in% names(model_train_flags))) next
      ln <- ast_line(nd, u$line_offset)
      findings[[length(findings) + 1L]] <- make_finding(
        ctx, ln, ast_col(nd),
        paste0(
          "In-sample stepwise variable selection (`", fn, "()`) without cross-validation or ",
          "holdout split biases estimates and invalidates post-selection p-values; ",
          "validate on held-out data or use penalized regression (e.g., `cv.glmnet()`)."
        )
      )
    }
  }
  findings
}

# 17. in_sample_prediction_eval
check_in_sample_prediction_eval <- function(ctx) {
  fit_fns <- c("lm", "glm", "lmer", "glmer", "randomForest", "ranger", "rpart", "xgb.train", "glmnet", "gam")
  metric_fns <- c(
    "rmse", "mae", "mse", "mape", "rmsle", "auc", "roc", "roc_auc",
    "confusionMatrix", "accuracy", "logLoss", "brier", "metrics"
  )

  model_data <- list()
  in_sample_preds <- list()

  resolve_model_train_df <- function(mnode) {
    margs <- ast_call_args(mnode)
    piped <- ast_piped_lhs(mnode)
    d_arg <- Filter(function(x) identical(x$name, "data"), margs)
    d_node <- if (length(d_arg) > 0L) {
      d_arg[[1L]]$expr
    } else if (length(margs) >= 2L && is.na(margs[[2L]]$name)) {
      margs[[2L]]$expr
    } else {
      NULL
    }
    # Resolve R 4.2+ pipe placeholder (`df |> lm(y ~ x, data = _)`) or magrittr `.`
    if (!is.null(d_node) && !is.null(piped)) {
      d_txt <- trimws(xml2::xml_text(d_node))
      if (d_txt %in% c("_", ".")) {
        d_node <- piped
      }
    } else if (is.null(d_node) && !is.null(piped)) {
      d_node <- piped
    }
    d_syms <- if (!is.null(d_node)) ast_symbols(d_node) else character(0)
    if (length(d_syms) >= 1L) d_syms[1L] else NA_character_
  }

  # Resolve (model_name, newdata_node) for direct AND piped predict()/fitted() calls,
  # properly recognizing positional `predict(fit, test_df)` as out-of-sample!
  classify_predict_call <- function(pnode) {
    pfn <- ast_call_name(pnode)
    pargs <- ast_call_args(pnode)
    piped <- ast_piped_lhs(pnode)
    pos_args <- Filter(function(x) is.na(x$name), pargs)
    nd_named <- Filter(function(x) identical(x$name, "newdata"), pargs)

    if (!is.null(piped)) {
      mod_node <- piped
      nd_node <- if (length(nd_named) > 0L) {
        nd_named[[1L]]$expr
      } else if (length(pos_args) >= 1L) {
        pos_args[[1L]]$expr
      } else {
        NULL
      }
    } else {
      mod_node <- if (length(pargs) >= 1L) pargs[[1L]]$expr else NULL
      nd_node <- if (length(nd_named) > 0L) {
        nd_named[[1L]]$expr
      } else if (length(pos_args) >= 2L) {
        pos_args[[2L]]$expr
      } else {
        NULL
      }
    }

    if (is.null(mod_node)) return(list(in_sample = FALSE))
    mod_syms <- ast_symbols(mod_node)
    mod_nm <- if (length(mod_syms) == 1L) mod_syms[1L] else NA_character_
    if (is.na(mod_nm) || !(mod_nm %in% names(model_data))) return(list(in_sample = FALSE))

    train_df <- model_data[[mod_nm]]
    if (pfn == "fitted" || is.null(nd_node)) {
      return(list(in_sample = TRUE, model = mod_nm, data = train_df))
    }
    nd_syms <- ast_symbols(nd_node)
    if (length(nd_syms) == 1L && identical(nd_syms[1L], train_df)) {
      return(list(in_sample = TRUE, model = mod_nm, data = train_df))
    }
    list(in_sample = FALSE)
  }

  for (u in ctx$units) {
    assigns <- ast_assignments(u$xml, u$line_offset)
    for (a in assigns) {
      # Track model -> training data symbol
      mc <- ast_calls(a$rhs_node, fit_fns)
      if (length(mc) >= 1L) {
        train_sym <- resolve_model_train_df(mc[[1L]])
        if (!is.na(train_sym)) {
          model_data[[a$lhs_root]] <- train_sym
        }
      }

      # Track predictions computed on same training data
      pc <- ast_calls(a$rhs_node, c("predict", "fitted"))
      if (length(pc) >= 1L) {
        cl <- classify_predict_call(pc[[1L]])
        if (isTRUE(cl$in_sample)) {
          in_sample_preds[[a$lhs_root]] <- list(model = cl$model, data = cl$data, line = a$line)
        }
      }
    }
  }

  is_inline_in_sample_predict <- function(node) {
    pcs <- ast_calls(node, c("predict", "fitted"))
    for (pnode in pcs) {
      cl <- classify_predict_call(pnode)
      if (isTRUE(cl$in_sample)) return(cl$data)
    }
    NA_character_
  }

  findings <- list()
  flagged_lines <- integer(0)

  for (u in ctx$units) {
    # 1. Explicit metric calls like rmse(df$y, pred) or auc(df$y, predict(fit, newdata = df))
    for (mc in ast_calls(u$xml, metric_fns)) {
      ln <- ast_line(mc, u$line_offset)
      if (ln %in% flagged_lines) next
      syms <- ast_symbols(mc)
      hit_pred <- intersect(syms, names(in_sample_preds))
      inline_df <- is_inline_in_sample_predict(mc)
      if (length(hit_pred) > 0L || !is.na(inline_df)) {
        df_nm <- if (length(hit_pred) > 0L) in_sample_preds[[hit_pred[1L]]]$data else inline_df
        flagged_lines <- c(flagged_lines, ln)
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(mc),
          paste0(
            "In-sample predictive evaluation in `", ast_call_name(mc), "()` using predictions ",
            "from training dataset `", df_nm, "`; evaluate predictive metrics on an independent test or CV split."
          )
        )
      }
    }

    # 2. Manual MSE / MAE expressions: mean((y - pred)^2) or mean(abs(y - pred))
    for (mn in ast_calls(u$xml, "mean")) {
      ln <- ast_line(mn, u$line_offset)
      if (ln %in% flagged_lines) next
      has_minus <- !inherits(xml2::xml_find_first(mn, ".//OP-MINUS"), "xml_missing")
      has_sq_or_abs <- !inherits(xml2::xml_find_first(mn, ".//OP-CARET"), "xml_missing") ||
        "abs" %in% ast_fn_calls_in(mn)
      if (!has_minus || !has_sq_or_abs) next
      syms <- ast_symbols(mn)
      hit_pred <- intersect(syms, names(in_sample_preds))
      inline_df <- is_inline_in_sample_predict(mn)
      if (length(hit_pred) > 0L || !is.na(inline_df)) {
        df_nm <- if (length(hit_pred) > 0L) in_sample_preds[[hit_pred[1L]]]$data else inline_df
        flagged_lines <- c(flagged_lines, ln)
        findings[[length(findings) + 1L]] <- make_finding(
          ctx, ln, ast_col(mn),
          paste0(
            "In-sample prediction error (`mean(...)`) computed on training dataset `", df_nm,
            "`; evaluate out-of-sample error on a held-out test or cross-validation set."
          )
        )
      }
    }
  }

  findings
}

# 18. unverified_merge_cardinality
check_unverified_merge_cardinality <- function(ctx) {
  join_fns <- c("merge", "left_join", "inner_join", "full_join", "right_join")
  verify_fns <- c("nrow", "NROW", "dim", "stopifnot", "duplicated", "anyDuplicated", "assert_that")

  # Collect post-merge verification calls (with line numbers and referenced symbols)
  verifies <- list()
  for (u in ctx$units) {
    for (vc in ast_calls(u$xml, verify_fns)) {
      verifies[[length(verifies) + 1L]] <- list(
        line = ast_line(vc, u$line_offset),
        symbols = ast_symbols(vc)
      )
    }
  }

  findings <- list()
  for (u in ctx$units) {
    for (nd in ast_calls(u$xml, join_fns)) {
      fn <- ast_call_name(nd)
      args <- ast_call_args(nd)
      arg_names <- vapply(args, function(a) a$name %||% "", character(1L))
      # dplyr >= 1.1 explicit relationship = "one-to-one" / "many-to-one" / etc.
      if ("relationship" %in% arg_names) next

      ln <- ast_line(nd, u$line_offset)
      assigned <- ast_assigned_var(nd)
      assigned_root <- if (!is.na(assigned)) sub("(\\$|\\[).*", "", assigned) else NA_character_

      # Check whether a later line verifies row count / uniqueness on the merged object
      # (or, for unassigned/inline joins, directly wraps or pipes the join into a verify call)
      is_verified <- FALSE
      if (!is.na(assigned_root)) {
        for (v in verifies) {
          if (v$line >= ln && assigned_root %in% v$symbols) {
            is_verified <- TRUE
            break
          }
        }
      } else {
        if (ast_is_inside_call(nd, verify_fns)) {
          is_verified <- TRUE
        } else {
          # Check if directly piped into a verify call: merge(...) |> nrow()
          p_node <- xml2::xml_parent(nd)
          if (!inherits(p_node, "xml_missing")) {
            pch <- xml2::xml_children(p_node)
            if (length(pch) == 3L &&
                (xml2::xml_name(pch[[2L]]) == "PIPE" ||
                 (xml2::xml_name(pch[[2L]]) == "SPECIAL" && xml2::xml_text(pch[[2L]]) == "%>%")) &&
                identical(xml2::xml_path(pch[[1L]]), xml2::xml_path(nd))) {
              rhs_fn <- ast_call_name(pch[[3L]])
              if (!is.na(rhs_fn) && rhs_fn %in% verify_fns) {
                is_verified <- TRUE
              }
            }
          }
        }
      }
      if (is_verified) next

      findings[[length(findings) + 1L]] <- make_finding(
        ctx, ln, ast_col(nd),
        paste0(
          "`", fn, "()` called without `relationship =` or a post-merge row-count assertion ",
          "(`nrow()` / `stopifnot()`); unintended duplicate keys can silently multiply rows."
        )
      )
    }
  }
  findings
}

# 19. missing_drop_false_matrix
check_missing_drop_false_matrix <- function(ctx) {
  is_const_multi_slice <- function(expr_nd) {
    if (is.null(expr_nd) || xml2::xml_name(expr_nd) != "expr") return(FALSE)
    # Constant numeric range `1:3` where BOTH endpoints are numeric literals with |b - a| >= 1
    if (!inherits(xml2::xml_find_first(expr_nd, "./OP-COLON"), "xml_missing")) {
      ech <- xml2::xml_children(expr_nd)
      if (length(ech) == 3L) {
        a_nd <- xml2::xml_find_first(ech[[1L]], "./NUM_CONST")
        b_nd <- xml2::xml_find_first(ech[[3L]], "./NUM_CONST")
        if (!inherits(a_nd, "xml_missing") && !inherits(b_nd, "xml_missing")) {
          a_val <- suppressWarnings(as.numeric(sub("L$", "", xml2::xml_text(a_nd))))
          b_val <- suppressWarnings(as.numeric(sub("L$", "", xml2::xml_text(b_nd))))
          if (is.finite(a_val) && is.finite(b_val) && abs(b_val - a_val) >= 1) {
            return(TRUE)
          }
        }
      }
      return(FALSE)
    }
    # Explicit multi-element c(a, b, ...)
    c_calls <- ast_calls(expr_nd, "c")
    if (length(c_calls) >= 1L && identical(xml2::xml_path(c_calls[[1L]]), xml2::xml_path(expr_nd))) {
      if (length(ast_call_args(c_calls[[1L]])) >= 2L) return(TRUE)
    }
    FALSE
  }

  is_logical_filter <- function(expr_nd) {
    if (is.null(expr_nd) || xml2::xml_name(expr_nd) != "expr") return(FALSE)
    if (!inherits(xml2::xml_find_first(
      expr_nd,
      ".//GT | .//LT | .//GE | .//LE | .//EQ | .//NE | .//AND | .//OR | .//OP-EXCLAMATION | .//SPECIAL"
    ), "xml_missing")) {
      return(TRUE)
    }
    any(ast_fn_calls_in(expr_nd) %in% c("which", "is.na", "complete.cases", "grepl"))
  }

  is_single_num_literal <- function(expr_nd) {
    if (is.null(expr_nd) || xml2::xml_name(expr_nd) != "expr") return(FALSE)
    ech <- xml2::xml_children(expr_nd)
    length(ech) == 1L && xml2::xml_name(ech[[1L]]) == "NUM_CONST"
  }

  df_like_names <- c("df", "dat", "data", "tbl", "sub", "train", "test", "d")

  findings <- list()
  for (u in ctx$units) {
    # 2D subsetting expressions inside reusable function()/lambda definitions
    sub_nodes <- xml2::xml_find_all(
      u$xml,
      ".//expr[OP-LEFT-BRACKET and OP-COMMA and ancestor::expr[FUNCTION or OP-LAMBDA]]"
    )
    for (nd in sub_nodes) {
      # Skip LHS of assignment (e.g., X[, j] <- 0)
      parent <- xml2::xml_parent(nd)
      if (!inherits(parent, "xml_missing")) {
        pch <- xml2::xml_children(parent)
        if (length(pch) == 3L &&
            xml2::xml_name(pch[[2L]]) %in% c("LEFT_ASSIGN", "EQ_ASSIGN") &&
            identical(xml2::xml_path(pch[[1L]]), xml2::xml_path(nd))) {
          next
        }
      }

      ch <- xml2::xml_children(nd)
      ch_names <- xml2::xml_name(ch)

      # Safe if `drop =` argument is present
      sub_syms <- xml2::xml_text(ch[ch_names == "SYMBOL_SUB"])
      if ("drop" %in% sub_syms) next

      lb_pos <- which(ch_names == "OP-LEFT-BRACKET")[1L]
      comma_pos <- which(ch_names == "OP-COMMA")[1L]
      if (is.na(lb_pos) || is.na(comma_pos) || comma_pos >= length(ch)) next

      obj_sym <- if (lb_pos >= 2L) trimws(xml2::xml_text(ch[[1L]])) else ""
      row_empty <- (comma_pos == lb_pos + 1L)
      row_node <- if (!row_empty) ch[[lb_pos + 1L]] else NULL

      col_name <- ch_names[comma_pos + 1L]
      col_empty <- col_name %in% c("OP-RIGHT-BRACKET", "OP-COMMA")
      col_node <- if (!col_empty) ch[[comma_pos + 1L]] else NULL

      should_flag <- FALSE
      slice_kind <- "column"

      if (row_empty && !col_empty) {
        # Column slice `X[, j]` or `X[, 1:p]`: drops matrix/df to vector when 1 col selected
        if (!is_const_multi_slice(col_node)) {
          should_flag <- TRUE
          slice_kind <- "column"
        }
      } else if (!row_empty && col_empty) {
        # Row slice `X[1, ]`: drops 2D matrix to 1D vector when 1 row selected
        if (is_single_num_literal(row_node)) {
          should_flag <- TRUE
          slice_kind <- "row"
        } else if (!is_const_multi_slice(row_node) && !is_logical_filter(row_node) &&
                   !(tolower(obj_sym) %in% df_like_names) &&
                   !grepl("^(idx|keep|rows|sel|mask|ok)$", xml2::xml_text(row_node), ignore.case = TRUE)) {
          should_flag <- TRUE
          slice_kind <- "row"
        }
      } else if (!row_empty && !col_empty) {
        # Both row and column supplied: ignore scalar element lookups (`m[i, 1]`,
        # `coef(summary(fit))[2, 4]`, `df[1, "col"]`), flag only when one axis is a
        # multi-element slice (`1:n` or `c(...)`) and the other can collapse to length 1
        row_is_range <- !inherits(xml2::xml_find_first(row_node, "./OP-COLON"), "xml_missing") ||
          is_const_multi_slice(row_node)
        col_is_range <- !inherits(xml2::xml_find_first(col_node, "./OP-COLON"), "xml_missing") ||
          is_const_multi_slice(col_node)
        if (row_is_range && !is_const_multi_slice(col_node)) {
          should_flag <- TRUE
          slice_kind <- "column"
        } else if (col_is_range && !is_const_multi_slice(row_node)) {
          should_flag <- TRUE
          slice_kind <- "row"
        }
      }

      if (!should_flag) next

      ln <- ast_line(nd, u$line_offset)
      findings[[length(findings) + 1L]] <- make_finding(
        ctx, ln, ast_col(nd),
        paste0(
          "2D ", slice_kind, " subsetting `", xml2::xml_text(nd), "` inside a function omits `drop = FALSE`; ",
          "selecting a single ", slice_kind, " silently collapses matrices/arrays into a 1D vector."
        )
      )
    }
  }
  findings
}

# 20. implicit_na_rm_masking
check_implicit_na_rm_masking <- function(ctx) {
  agg_fns <- c(
    "mean", "sd", "var", "median", "sum", "quantile", "min", "max",
    "colMeans", "rowMeans", "colSums", "rowSums"
  )
  na_guard_fns <- c(
    "is.na", "anyNA", "complete.cases", "na.omit", "na.exclude",
    "drop_na", "na.fail", "n_miss", "miss_var_summary"
  )

  # Collect missingness inspection/filtering calls with line numbers and symbols
  guards <- list()
  for (u in ctx$units) {
    for (gc in ast_calls(u$xml, na_guard_fns)) {
      guards[[length(guards) + 1L]] <- list(
        line = ast_line(gc, u$line_offset),
        symbols = ast_symbols(gc)
      )
    }
  }

  is_true_literal <- function(txt) {
    trimws(txt) %in% c("TRUE", "T")
  }

  findings <- list()
  for (u in ctx$units) {
    for (nd in ast_calls(u$xml, agg_fns)) {
      fn <- ast_call_name(nd)
      args <- ast_call_args(nd)
      narm_arg <- Filter(function(a) identical(a$name, "na.rm"), args)
      val_txt <- NA_character_

      if (length(narm_arg) > 0L) {
        if (is_true_literal(narm_arg[[1L]]$text)) {
          val_txt <- trimws(narm_arg[[1L]]$text)
        }
      } else {
        # Positional na.rm arguments: mean(x, trim, na.rm), sd(x, na.rm),
        # median(x, na.rm), var(x, y, na.rm), colMeans/rowMeans/colSums/rowSums(x, na.rm)
        pos_args <- Filter(function(a) is.na(a$name), args)
        pos_idx <- switch(
          fn,
          mean = 3L,
          sd = 2L,
          median = 2L,
          var = 3L,
          colMeans = 2L,
          rowMeans = 2L,
          colSums = 2L,
          rowSums = 2L,
          NA_integer_
        )
        if (!is.na(pos_idx) && length(pos_args) >= pos_idx && is_true_literal(pos_args[[pos_idx]]$text)) {
          val_txt <- trimws(pos_args[[pos_idx]]$text)
        }
      }

      if (is.na(val_txt)) next

      ln <- ast_line(nd, u$line_offset)
      # Collect target symbols from first argument (or piped LHS)
      target_expr <- if (length(args) >= 1L && !is.null(args[[1L]]$expr)) args[[1L]]$expr else ast_piped_lhs(nd)
      target_syms <- if (!is.null(target_expr)) ast_symbols(target_expr) else character(0)

      # Exempt only if a missingness guard occurs on/before this line OR targets the same variable
      has_guard <- FALSE
      for (g in guards) {
        if (g$line <= ln || (length(target_syms) > 0L && length(intersect(target_syms, g$symbols)) > 0L)) {
          has_guard <- TRUE
          break
        }
      }
      if (has_guard) next

      findings[[length(findings) + 1L]] <- make_finding(
        ctx, ln, ast_col(nd),
        paste0(
          "`", fn, "(..., na.rm = ", val_txt, ")` silently discards missing values with no ",
          "`is.na()` / `anyNA()` / `complete.cases()` missingness check or report in scope."
        )
      )
    }
  }
  findings
}

build_default_rules <- function() {
  list(
    new_stat_rule(
      id = "unseeded_randomness",
      title = "Unseeded stochastic call",
      severity = "error",
      category = "reproducibility",
      description = "Random number generation or stochastic modeling called without a preceding RNG seed.",
      recommendation = "Call set.seed() before stochastic operations or wrap them in withr::with_seed().",
      check = check_unseeded_randomness
    ),
    new_stat_rule(
      id = "unsafe_parallel_rng",
      title = "Unsafe parallel random number generation",
      severity = "error",
      category = "parallel_rng",
      description = "Parallel execution without independent L'Ecuyer-CMRG RNG streams can cause seed collisions or core-count sensitivity.",
      recommendation = "Use RNGkind(\"L'Ecuyer-CMRG\"), parallel::clusterSetRNGStream(), doRNG::registerDoRNG(), future.seed = TRUE, or statci_parallel_apply().",
      check = check_unsafe_parallel_rng
    ),
    new_stat_rule(
      id = "duplicate_parallel_seed",
      title = "Constant set.seed() inside loop or parallel worker",
      severity = "error",
      category = "parallel_rng",
      description = "Resetting set.seed() to a fixed constant inside a loop or parallel worker causes every replication to draw identical duplicate data.",
      recommendation = "Seed once before the loop with L'Ecuyer-CMRG or use statci_parallel_apply() for distinct per-task streams.",
      check = check_duplicate_parallel_seed
    ),
    new_stat_rule(
      id = "preprocessing_before_split",
      title = "Feature preprocessing before train/test split",
      severity = "error",
      category = "data_leakage",
      description = "Scaling, PCA, or imputation applied to the full dataset prior to splitting leaks test-set distributional information.",
      recommendation = "Split the dataset first, estimate preprocessing parameters on training data only, and apply them to test data.",
      check = check_preprocessing_before_split
    ),
    new_stat_rule(
      id = "in_sample_prediction_eval",
      title = "Predictive error evaluated in-sample on training data",
      severity = "warning",
      category = "data_leakage",
      description = "Evaluating RMSE, MAE, AUC, or MSE on predictions from the same training dataset yields optimistic generalization estimates.",
      recommendation = "Evaluate predictive metrics on a held-out test set or out-of-fold cross-validation predictions.",
      check = check_in_sample_prediction_eval
    ),
    new_stat_rule(
      id = "uncentered_mlm_interaction",
      title = "Uncentered predictor in multilevel interaction",
      severity = "warning",
      category = "modeling",
      description = "Raw uncentered predictors in multilevel interaction terms conflate within- and between-cluster effects and destabilize estimation.",
      recommendation = "Apply cluster-mean or grand-mean centering (e.g., x_wc <- x - ave(x, group) or scale()) before forming interactions.",
      check = check_uncentered_mlm_interaction
    ),
    new_stat_rule(
      id = "silent_listwise_mismatch",
      title = "Potential silent listwise deletion mismatch in model comparison",
      severity = "warning",
      category = "modeling",
      description = "Comparing nested models with differing predictors via anova()/AIC()/BIC() on unharmonized data risks comparing different sample sizes.",
      recommendation = "Filter to common complete cases across all candidate model variables before fitting (e.g., na.omit() or complete.cases()).",
      check = check_silent_listwise_mismatch
    ),
    new_stat_rule(
      id = "silent_factor_drop",
      title = "Silent factor coercion or retained unused factor levels",
      severity = "warning",
      category = "modeling",
      description = "Coercing factors directly with as.numeric() extracts level indices, and modeling subsetted data without droplevels() retains empty levels.",
      recommendation = "Use as.numeric(as.character(x)) for numeric factor labels, and call droplevels() after subset()/filter().",
      check = check_silent_factor_drop
    ),
    new_stat_rule(
      id = "silent_na_coercion",
      title = "Unchecked numeric coercion of character/parsed strings",
      severity = "warning",
      category = "modeling",
      description = "Coercing character vectors or string-parsed tokens with as.numeric()/as.integer() silently produces NAs for unparseable values.",
      recommendation = "Verify coerced outputs with anyNA() or is.na() before downstream modeling.",
      check = check_silent_na_coercion
    ),
    new_stat_rule(
      id = "unverified_merge_cardinality",
      title = "Unverified join/merge cardinality without row-count check",
      severity = "warning",
      category = "modeling",
      description = "Calling merge() or dplyr joins without specifying `relationship =` or checking nrow() risks silent many-to-many row duplication.",
      recommendation = "Pass relationship = \"one-to-one\" / \"many-to-one\" (dplyr) or assert stopifnot(nrow(merged) == nrow(lhs)).",
      check = check_unverified_merge_cardinality
    ),
    new_stat_rule(
      id = "missing_drop_false_matrix",
      title = "2D column subsetting inside function without drop = FALSE",
      severity = "warning",
      category = "modeling",
      description = "Subsetting matrix or data.frame columns inside reusable functions without drop = FALSE silently collapses 1-column selections into vectors.",
      recommendation = "Pass drop = FALSE explicitly (e.g., X[, j, drop = FALSE]).",
      check = check_missing_drop_false_matrix
    ),
    new_stat_rule(
      id = "implicit_na_rm_masking",
      title = "Summary metric uses na.rm = TRUE without missingness audit",
      severity = "warning",
      category = "modeling",
      description = "Discarding missing values silently via na.rm = TRUE without checking is.na()/anyNA() can mask data corruption or heavy attrition.",
      recommendation = "Audit or assert missingness explicitly (e.g., sum(is.na(x)) or anyNA(x)) before applying na.rm = TRUE.",
      check = check_implicit_na_rm_masking
    ),
    new_stat_rule(
      id = "equality_on_float",
      title = "Exact equality comparison on floating-point expression",
      severity = "warning",
      category = "numerics",
      description = "Exact == or != comparisons on IEEE-754 floating-point values fail due to finite-precision representation error.",
      recommendation = "Use isTRUE(all.equal(a, b)) or abs(a - b) < tol.",
      check = check_equality_on_float
    ),
    new_stat_rule(
      id = "unadjusted_multiple_testing",
      title = "Multiple hypothesis tests in loop without adjustment",
      severity = "warning",
      category = "inference",
      description = "Running repeated hypothesis tests across outcomes or subgroups without multiplicity adjustment inflates Type-I error.",
      recommendation = "Collect raw p-values and adjust them with p.adjust(pvals, method = \"BH\") or \"holm\".",
      check = check_unadjusted_multiple_testing
    ),
    new_stat_rule(
      id = "stepwise_selection_unvalidated",
      title = "Unvalidated in-sample stepwise variable selection",
      severity = "warning",
      category = "inference",
      description = "Automated stepwise selection (step(), stepAIC(), regsubsets()) without holdout or cross-validation biases coefficients and invalidates p-values.",
      recommendation = "Validate stepwise models on an independent test/CV split or use penalized regression (e.g., cv.glmnet()).",
      check = check_stepwise_selection_unvalidated
    ),
    new_stat_rule(
      id = "missing_convergence_check",
      title = "Iterative model fitted without convergence or singularity check",
      severity = "warning",
      category = "modeling",
      description = "Mixed-effects models and numeric optimizers can fail to converge or hit boundary singularities silently in batch runs.",
      recommendation = "Check lme4::isSingular(fit), fit@optinfo$conv, or res$convergence == 0 programmatically.",
      check = check_missing_convergence_check
    ),
    new_stat_rule(
      id = "global_seed_override_in_fn",
      title = "Global RNG seed mutated inside reusable function",
      severity = "warning",
      category = "reproducibility",
      description = "Calling bare set.seed() inside a function mutates global .Random.seed and corrupts outer simulation loops.",
      recommendation = "Use withr::local_seed(seed) or save and restore .Random.seed via on.exit().",
      check = check_global_seed_override_in_fn
    ),
    new_stat_rule(
      id = "short_logical_literal",
      title = "Short logical symbol T/F instead of TRUE/FALSE",
      severity = "warning",
      category = "hygiene",
      description = "Unlike reserved TRUE/FALSE, T and F are mutable variables that can be overwritten in statistical code.",
      recommendation = "Spell out TRUE and FALSE explicitly.",
      check = check_short_logical_literal
    ),
    new_stat_rule(
      id = "cor_without_use_arg",
      title = "cor()/cov() called without explicit `use` missing-data policy",
      severity = "info",
      category = "modeling",
      description = "Omitting `use` in cor() or cov() defaults to \"everything\", causing a single NA to wipe out estimates silently.",
      recommendation = "Pass use = \"complete.obs\" or use = \"pairwise.complete.obs\" explicitly.",
      check = check_cor_without_use_arg
    ),
    new_stat_rule(
      id = "absolute_path_in_script",
      title = "Hardcoded machine-specific absolute file path",
      severity = "warning",
      category = "reproducibility",
      description = "Machine-specific absolute paths fail on collaborators' machines and CI runners.",
      recommendation = "Use project-relative paths with file.path() or here::here().",
      check = check_absolute_path_in_script
    )
  )
}
