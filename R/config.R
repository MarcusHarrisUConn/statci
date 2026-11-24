# Project Configuration (.statci.yml) and Orchestration Engine

#' Read and validate a `.statci.yml` project configuration file
#'
#' Parses a YAML configuration file strictly with `eval.expr = FALSE` (blocking
#' arbitrary R code evaluation during YAML loading) and validates all fields
#' against the `statci` configuration schema, including enabled/disabled lint
#' rules, severity overrides, exclude patterns, manuscript/model paths, report
#' targets, and CI gate thresholds.
#'
#' @param path Path to a `.statci.yml` (or custom YAML) configuration file.
#'
#' @return An S3 object of class `"statci_config"` containing normalized
#'   `lint`, `manuscript`, `gate`, `report`, and `source` fields.
#' @export
#'
#' @examples
#' tmp <- tempfile(fileext = ".yml")
#' writeLines(c(
#'   "version: 1",
#'   "lint:",
#'   "  paths: [R]",
#'   "  exclude_rules: [cor_without_use_arg]",
#'   "gate:",
#'   "  fail_on: fail"
#' ), tmp)
#' cfg <- read_statci_config(tmp)
#' cfg
#' unlink(tmp)
read_statci_config <- function(path = ".statci.yml") {
  validate_existing_file(path, arg = "path")
  raw <- tryCatch(
    yaml::yaml.load_file(path, eval.expr = FALSE),
    error = function(e) {
      rlang::abort(paste0("Failed to parse YAML config `", path, "`: ", conditionMessage(e)))
    }
  )
  if (is.null(raw)) raw <- list()
  if (!is.list(raw)) {
    rlang::abort("`.statci.yml` must contain a YAML mapping at the top level.")
  }

  allowed_top <- c("version", "lint", "manuscript", "gate", "report")
  unknown_top <- setdiff(names(raw), allowed_top)
  if (length(unknown_top) > 0L) {
    rlang::abort(paste0("Unknown top-level key(s) in config: ", paste(unknown_top, collapse = ", "), "."))
  }

  cfg <- default_statci_config_list()
  cfg$source <- normalizePath(path, winslash = "/", mustWork = FALSE)

  if (!is.null(raw$version)) {
    v <- suppressWarnings(as.integer(raw$version)[1L])
    if (is.na(v) || v != 1L) {
      rlang::abort("`version` in `.statci.yml` must be `1`.")
    }
    cfg$version <- v
  }

  # --- lint section ---
  if (!is.null(raw$lint)) {
    if (!is.list(raw$lint)) rlang::abort("`lint` section must be a YAML mapping.")
    allowed_lint <- c("paths", "rules", "exclude_rules", "exclude_pattern", "skip_eval_false", "severity_overrides")
    unk_l <- setdiff(names(raw$lint), allowed_lint)
    if (length(unk_l) > 0L) {
      rlang::abort(paste0("Unknown key(s) under `lint`: ", paste(unk_l, collapse = ", "), "."))
    }
    if (!is.null(raw$lint$paths)) {
      lp <- as.character(unlist(raw$lint$paths))
      if (length(lp) < 1L) rlang::abort("`lint.paths` must be non-empty.")
      cfg$lint$paths <- vapply(lp, validate_safe_rel_path, character(1L), arg = "lint.paths")
    }
    known_rules <- list_stat_rules()$id
    if (!is.null(raw$lint$rules)) {
      rl <- as.character(unlist(raw$lint$rules))
      bad_r <- setdiff(rl, known_rules)
      if (length(bad_r) > 0L) {
        rlang::abort(paste0("Unknown rule(s) in `lint.rules`: ", paste(bad_r, collapse = ", "), "."))
      }
      cfg$lint$rules <- rl
    }
    if (!is.null(raw$lint$exclude_rules)) {
      ex <- as.character(unlist(raw$lint$exclude_rules))
      bad_ex <- setdiff(ex, known_rules)
      if (length(bad_ex) > 0L) {
        rlang::abort(paste0("Unknown rule(s) in `lint.exclude_rules`: ", paste(bad_ex, collapse = ", "), "."))
      }
      cfg$lint$exclude_rules <- ex
    }
    if (!is.null(raw$lint$exclude_pattern)) {
      assert_clean_scalar_string(raw$lint$exclude_pattern, "lint.exclude_pattern")
      cfg$lint$exclude_pattern <- raw$lint$exclude_pattern
    }
    if (!is.null(raw$lint$skip_eval_false)) {
      if (!is.logical(raw$lint$skip_eval_false) || length(raw$lint$skip_eval_false) != 1L || is.na(raw$lint$skip_eval_false)) {
        rlang::abort("`lint.skip_eval_false` must be a logical scalar (`true`/`false`).")
      }
      cfg$lint$skip_eval_false <- raw$lint$skip_eval_false
    }
    if (!is.null(raw$lint$severity_overrides)) {
      so <- raw$lint$severity_overrides
      if (!is.list(so) || is.null(names(so))) {
        rlang::abort("`lint.severity_overrides` must be a named mapping of `rule_id: severity`.")
      }
      bad_so_rules <- setdiff(names(so), known_rules)
      if (length(bad_so_rules) > 0L) {
        rlang::abort(paste0("Unknown rule(s) in `lint.severity_overrides`: ", paste(bad_so_rules, collapse = ", "), "."))
      }
      vals <- vapply(so, function(v) tolower(as.character(v)[1L]), character(1L))
      bad_sev <- setdiff(vals, c("error", "warning", "info"))
      if (length(bad_sev) > 0L) {
        rlang::abort(paste0(
          "Invalid severity override(s): ", paste(bad_sev, collapse = ", "),
          ". Allowed: 'error', 'warning', 'info'."
        ))
      }
      cfg$lint$severity_overrides <- stats::setNames(as.character(vals), names(so))
    }
  }

  # --- manuscript section ---
  if (!is.null(raw$manuscript)) {
    if (!is.list(raw$manuscript)) rlang::abort("`manuscript` section must be a YAML mapping.")
    allowed_ms <- c("path", "models", "tol_extra", "warn_rounding", "check_internal")
    unk_m <- setdiff(names(raw$manuscript), allowed_ms)
    if (length(unk_m) > 0L) {
      rlang::abort(paste0("Unknown key(s) under `manuscript`: ", paste(unk_m, collapse = ", "), "."))
    }
    if (!is.null(raw$manuscript$path)) {
      cfg$manuscript$path <- validate_safe_rel_path(raw$manuscript$path, arg = "manuscript.path")
    }
    if (!is.null(raw$manuscript$models)) {
      cfg$manuscript$models <- validate_safe_rel_path(raw$manuscript$models, arg = "manuscript.models")
    }
    if (!is.null(raw$manuscript$tol_extra)) {
      te <- as.numeric(raw$manuscript$tol_extra)[1L]
      if (is.na(te) || te < 0) rlang::abort("`manuscript.tol_extra` must be a non-negative number.")
      cfg$manuscript$tol_extra <- te
    }
    if (!is.null(raw$manuscript$warn_rounding)) {
      if (!is.logical(raw$manuscript$warn_rounding) || length(raw$manuscript$warn_rounding) != 1L) {
        rlang::abort("`manuscript.warn_rounding` must be a logical scalar.")
      }
      cfg$manuscript$warn_rounding <- raw$manuscript$warn_rounding
    }
    if (!is.null(raw$manuscript$check_internal)) {
      if (!is.logical(raw$manuscript$check_internal) || length(raw$manuscript$check_internal) != 1L) {
        rlang::abort("`manuscript.check_internal` must be a logical scalar.")
      }
      cfg$manuscript$check_internal <- raw$manuscript$check_internal
    }
  }

  # --- gate section ---
  if (!is.null(raw$gate)) {
    if (!is.list(raw$gate)) rlang::abort("`gate` section must be a YAML mapping.")
    unk_g <- setdiff(names(raw$gate), "fail_on")
    if (length(unk_g) > 0L) {
      rlang::abort(paste0("Unknown key(s) under `gate`: ", paste(unk_g, collapse = ", "), "."))
    }
    if (!is.null(raw$gate$fail_on)) {
      fo <- tolower(as.character(raw$gate$fail_on)[1L])
      if (!(fo %in% c("fail", "warn"))) {
        rlang::abort("`gate.fail_on` must be `'fail'` or `'warn'`.")
      }
      cfg$gate$fail_on <- fo
    }
  }

  # --- report section ---
  if (!is.null(raw$report)) {
    if (!is.list(raw$report)) rlang::abort("`report` section must be a YAML mapping.")
    allowed_rep <- c("title", "output_dir", "formats")
    unk_r <- setdiff(names(raw$report), allowed_rep)
    if (length(unk_r) > 0L) {
      rlang::abort(paste0("Unknown key(s) under `report`: ", paste(unk_r, collapse = ", "), "."))
    }
    if (!is.null(raw$report$title)) {
      assert_clean_scalar_string(raw$report$title, "report.title")
      cfg$report$title <- raw$report$title
    }
    if (!is.null(raw$report$output_dir)) {
      cfg$report$output_dir <- validate_safe_rel_path(raw$report$output_dir, arg = "report.output_dir")
    }
    if (!is.null(raw$report$formats)) {
      fmts <- tolower(as.character(unlist(raw$report$formats)))
      bad_f <- setdiff(fmts, c("html", "markdown", "json", "sarif"))
      if (length(bad_f) > 0L) {
        rlang::abort(paste0("Invalid report format(s): ", paste(bad_f, collapse = ", "), "."))
      }
      cfg$report$formats <- unique(fmts)
    }
  }

  structure(cfg, class = "statci_config")
}

default_statci_config_list <- function() {
  list(
    version = 1L,
    source = "<defaults>",
    lint = list(
      paths = c("R"),
      rules = NULL,
      exclude_rules = NULL,
      exclude_pattern = "(^|/)(\\.git|renv|packrat|\\.Rproj\\.user|docs)/",
      skip_eval_false = FALSE,
      severity_overrides = character(0)
    ),
    manuscript = list(
      path = NULL,
      models = NULL,
      tol_extra = 0,
      warn_rounding = TRUE,
      check_internal = TRUE
    ),
    gate = list(
      fail_on = "fail"
    ),
    report = list(
      title = "statci Statistical CI/CD Audit Report",
      output_dir = NULL,
      formats = c("html", "markdown")
    )
  )
}

#' @export
print.statci_config <- function(x, ...) {
  cli::cli_text("<statci_config> (source: {basename(x$source)})")
  cli::cli_text("  lint paths: {paste(x$lint$paths, collapse = ', ')}")
  if (length(x$lint$exclude_rules) > 0L) {
    cli::cli_text("  excluded rules: {paste(x$lint$exclude_rules, collapse = ', ')}")
  }
  if (length(x$lint$severity_overrides) > 0L) {
    ov <- paste0(names(x$lint$severity_overrides), "=", unname(x$lint$severity_overrides))
    cli::cli_text("  severity overrides: {paste(ov, collapse = ', ')}")
  }
  if (!is.null(x$manuscript$path)) {
    cli::cli_text("  manuscript: {x$manuscript$path} (models: {x$manuscript$models %||% 'internal-only'})")
  }
  cli::cli_text("  gate fail_on: {x$gate$fail_on}")
  invisible(x)
}

#' Apply severity overrides cleanly to a `statci_lint` result
#' @noRd
apply_severity_overrides <- function(lint_res, overrides) {
  if (is.null(overrides) || length(overrides) == 0L || nrow(lint_res$findings) == 0L) {
    return(lint_res)
  }
  f <- lint_res$findings
  matched <- f$rule %in% names(overrides)
  if (any(matched)) {
    f$severity[matched] <- unname(overrides[f$rule[matched]])
    lint_res <- build_lint_object(
      findings = f,
      files = lint_res$files,
      rules_run = lint_res$rules_run
    )
  }
  lint_res
}

#' Run the full configured `statci` verification pipeline on a project
#'
#' Orchestrates statistical AST linting, optional manuscript-to-code auditing,
#' optional programmatic estimator/SBC/parallel RNG/sensitivity/power-curve
#' audits, report rendering (HTML, Markdown, JSON, and/or SARIF 2.1.0), and
#' optional CI gate enforcement in a single call driven by `.statci.yml`.
#'
#' @param path Root project directory (default `"."`).
#' @param config One of:
#'   - A `"statci_config"` object from [read_statci_config()];
#'   - Path to a `.statci.yml` file; or
#'   - `NULL` (auto-discovers `file.path(path, ".statci.yml")` if present,
#'     otherwise uses sensible defaults).
#' @param estimator Optional `"statci_estimator_audit"` object to include.
#' @param sbc Optional `"statci_sbc"` object to include.
#' @param parallel_rng Optional `"statci_parallel_rng"` object to include.
#' @param sensitivity Optional `"statci_sensitivity"` object to include.
#' @param power_curve Optional `"statci_power_curve"` object to include.
#' @param multiverse Optional `"statci_multiverse"` object to include.
#' @param enforce_gate Logical; if `TRUE`, calls [statci_check_gate()] using
#'   `config$gate$fail_on`. Default `FALSE`.
#'
#' @return An S3 object of class `"statci_report"` (with `$config` and
#'   `$artifacts` attached when reports are written to disk).
#' @export
#'
#' @examples
#' tmp <- tempfile("statci_proj_")
#' dir.create(file.path(tmp, "R"), recursive = TRUE)
#' writeLines(c("set.seed(42)", "x <- rnorm(20)"), file.path(tmp, "R", "sim.R"))
#' rep <- statci_audit_project(tmp)
#' rep
#' unlink(tmp, recursive = TRUE)
statci_audit_project <- function(path = ".",
                                 config = NULL,
                                 estimator = NULL,
                                 sbc = NULL,
                                 parallel_rng = NULL,
                                 sensitivity = NULL,
                                 power_curve = NULL,
                                 multiverse = NULL,
                                 enforce_gate = FALSE) {
  validate_existing_dir(path, arg = "path")
  root_norm <- normalizePath(path, winslash = "/", mustWork = TRUE)

  if (is.null(config)) {
    auto_cfg <- file.path(root_norm, ".statci.yml")
    if (file.exists(auto_cfg)) {
      cfg <- read_statci_config(auto_cfg)
    } else {
      cfg <- structure(default_statci_config_list(), class = "statci_config")
    }
  } else if (inherits(config, "statci_config")) {
    cfg <- config
  } else if (is.character(config) && length(config) == 1L) {
    cfg <- read_statci_config(config)
  } else {
    rlang::abort("`config` must be NULL, a path to `.statci.yml`, or a `statci_config` object.")
  }

  # 1. Run AST linter across configured project directories
  lint_dirs <- file.path(root_norm, cfg$lint$paths)
  lint_dirs <- lint_dirs[dir.exists(lint_dirs)]
  lint_res <- if (length(lint_dirs) > 0L) {
    stat_lint_dir(
      path = lint_dirs,
      exclude_pattern = cfg$lint$exclude_pattern,
      rules = cfg$lint$rules,
      exclude_rules = cfg$lint$exclude_rules,
      skip_eval_false = cfg$lint$skip_eval_false
    )
  } else {
    build_lint_object(
      findings = empty_findings_tibble(),
      files = character(0),
      rules_run = vapply(resolve_lint_rules(cfg$lint$rules, cfg$lint$exclude_rules), `[[`, character(1L), "id")
    )
  }
  lint_res <- apply_severity_overrides(lint_res, cfg$lint$severity_overrides)

  # 2. Run manuscript audit if configured
  ms_res <- NULL
  if (!is.null(cfg$manuscript$path)) {
    ms_file <- validate_contained_dest(root_norm, cfg$manuscript$path, arg = "manuscript.path")
    mod_arg <- if (!is.null(cfg$manuscript$models)) {
      validate_contained_dest(root_norm, cfg$manuscript$models, arg = "manuscript.models")
    } else {
      NULL
    }
    ms_res <- audit_manuscript(
      manuscript = ms_file,
      models = mod_arg,
      tol_extra = cfg$manuscript$tol_extra,
      warn_rounding = cfg$manuscript$warn_rounding,
      check_internal = cfg$manuscript$check_internal
    )
  }

  # 3. Assemble unified report
  rep <- statci_report(
    lint = lint_res,
    estimator = estimator,
    sbc = sbc,
    parallel_rng = parallel_rng,
    manuscript = ms_res,
    sensitivity = sensitivity,
    power_curve = power_curve,
    multiverse = multiverse,
    title = cfg$report$title,
    metadata = list(project_dir = basename(root_norm))
  )
  rep$config <- cfg

  # 4. Optionally write configured report formats inside project
  artifacts <- character(0)
  if (!is.null(cfg$report$output_dir)) {
    out_dir <- validate_contained_dest(root_norm, cfg$report$output_dir, arg = "report.output_dir")
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    for (fmt in cfg$report$formats) {
      if (fmt == "sarif") {
        sarif_path <- file.path(out_dir, "statci.sarif")
        export_statci_sarif(lint_res, output_file = sarif_path, base_uri = root_norm)
        artifacts <- c(artifacts, sarif_path)
      } else {
        ext <- switch(fmt, html = "html", markdown = "md", json = "json")
        out_file <- file.path(out_dir, paste0("statci-report.", ext))
        render_statci_report(rep, output_file = out_file, format = fmt)
        artifacts <- c(artifacts, out_file)
      }
    }
  }
  rep$artifacts <- artifacts

  if (isTRUE(enforce_gate)) {
    statci_check_gate(rep, fail_on = cfg$gate$fail_on)
  }

  rep
}
