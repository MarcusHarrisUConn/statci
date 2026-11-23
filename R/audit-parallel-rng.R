# Parallel RNG invariance utilities and deterministic L'Ecuyer task runner

#' Execute a stochastic map with core-count-invariant L'Ecuyer-CMRG RNG streams
#'
#' Pre-generates one independent seed per element of `X` from a master
#' `"L'Ecuyer-CMRG"` stream and evaluates `FUN(X[[i]], ...)` inside a locally
#' scoped L'Ecuyer-CMRG RNG context. Because each task receives its own
#' deterministic stream independent of worker partitioning, results are
#' bit-for-bit identical across `cores = 1`, `cores = 2`, or any larger core
#' count on macOS, Linux, and Windows.
#'
#' @param X A vector or list to iterate over.
#' @param FUN A function applied to each element of `X`.
#' @param ... Additional arguments passed to `FUN`.
#' @param cores Integer number of worker processes (`>= 1`). When running under
#'   `R CMD check` (`_R_CHECK_LIMIT_CORES_` set), `cores` is automatically
#'   capped at 2.
#' @param seed Integer master seed used to derive per-task L'Ecuyer streams.
#'
#' @return A list of length `length(X)` containing the results of `FUN`.
#' @export
#'
#' @examples
#' res1 <- statci_parallel_apply(1:4, function(i) rnorm(3), cores = 1L, seed = 42L)
#' res2 <- statci_parallel_apply(1:4, function(i) rnorm(3), cores = 2L, seed = 42L)
#' identical(res1, res2)
statci_parallel_apply <- function(X, FUN, ..., cores = 1L, seed = 42L) {
  X_list <- as.list(X)
  n <- length(X_list)
  if (n == 0L) return(list())
  if (!is.function(FUN)) {
    rlang::abort("`FUN` must be a function.")
  }

  cores <- as.integer(cores)[1L]
  if (is.na(cores) || cores < 1L) cores <- 1L
  if (nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_"))) {
    cores <- min(cores, 2L)
  }
  cores <- min(cores, n)

  # Derive deterministic per-task seeds from a master L'Ecuyer-CMRG stream
  task_seeds <- withr::with_seed(
    seed = as.integer(seed)[1L],
    .rng_kind = "L'Ecuyer-CMRG",
    code = sample.int(.Machine$integer.max, size = n, replace = FALSE)
  )

  dots <- list(...)
  worker_fn <- function(idx, x_val, t_seed, fn, extra_args) {
    withr::with_seed(
      seed = t_seed,
      .rng_kind = "L'Ecuyer-CMRG",
      code = do.call(fn, c(list(x_val), extra_args))
    )
  }

  if (cores <= 1L) {
    out <- lapply(seq_len(n), function(i) {
      worker_fn(i, X_list[[i]], task_seeds[i], FUN, dots)
    })
  } else {
    cl <- parallel::makePSOCKcluster(cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    tasks <- lapply(seq_len(n), function(i) list(i = i, x = X_list[[i]], s = task_seeds[i]))
    out <- parallel::parLapply(
      cl,
      tasks,
      function(task, fn, extra_args) {
        withr::with_seed(
          seed = task$s,
          .rng_kind = "L'Ecuyer-CMRG",
          code = do.call(fn, c(list(task$x), extra_args))
        )
      },
      fn = FUN,
      extra_args = dots
    )
  }

  names(out) <- names(X)
  out
}

#' Audit parallel RNG invariance across core counts
#'
#' Verifies that a stochastic pipeline produces identical results when executed
#' sequentially (`cores = 1`) versus in parallel (`cores = 2`, etc.). Supports
#' either a custom `runner(cores, seed)` function to audit your own parallel
#' code directly, or a task function `fun` evaluated via
#' [statci_parallel_apply()].
#'
#' @param runner Optional function of signature `function(cores, seed)` that
#'   executes the stochastic computation using `cores` workers and master
#'   `seed`. If provided, `check_parallel_rng()` tests whether `runner` yields
#'   identical outputs across all values of `cores`.
#' @param fun Optional task function `function(i, ...)` evaluated over `X` via
#'   [statci_parallel_apply()] when `runner` is `NULL`.
#' @param X Sequence or list of inputs for `fun` (defaults to `seq_len(8L)`).
#' @param cores Integer vector of core counts to compare (default `c(1L, 2L)`).
#'   Must contain at least two distinct values.
#' @param seed Integer seed passed to each run.
#' @param tolerance Numeric comparison tolerance (`0` checks bit-for-bit
#'   exactness via [identical()]).
#' @param ... Additional arguments forwarded to `fun`.
#'
#' @return An S3 object of class `"statci_parallel_rng"` with `status`
#'   (`"pass"` or `"fail"`), `comparisons` tibble, `identical_all`,
#'   `max_abs_diff`, `cores_tested`, and `seed`.
#' @export
#'
#' @examples
#' # Passes: deterministic L'Ecuyer stream per task
#' ok <- check_parallel_rng(
#'   runner = function(cores, seed) {
#'     unlist(statci_parallel_apply(1:6, function(i) mean(rnorm(20)), cores = cores, seed = seed))
#'   },
#'   cores = c(1L, 2L),
#'   seed = 123L
#' )
#' ok$status
check_parallel_rng <- function(runner = NULL,
                               fun = NULL,
                               X = seq_len(8L),
                               cores = c(1L, 2L),
                               seed = 42L,
                               tolerance = 0,
                               ...) {
  cores <- unique(as.integer(cores))
  cores <- cores[!is.na(cores) & cores >= 1L]
  if (nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_"))) {
    cores <- unique(pmin(cores, 2L))
  }
  if (length(cores) < 2L) {
    rlang::abort("`cores` must contain at least two distinct positive integers (e.g., `c(1L, 2L)`).")
  }
  if (is.null(runner) && is.null(fun)) {
    rlang::abort("Supply either `runner = function(cores, seed)` or `fun = function(x, ...)`.")
  }

  dots <- list(...)
  if (is.null(runner)) {
    runner <- function(n_cores, s) {
      do.call(statci_parallel_apply, c(list(X = X, FUN = fun, cores = n_cores, seed = s), dots))
    }
  }

  results <- vector("list", length(cores))
  names(results) <- paste0("cores_", cores)
  for (k in seq_along(cores)) {
    results[[k]] <- withr::with_preserve_seed(runner(cores[k], as.integer(seed)[1L]))
  }

  ref <- results[[1L]]
  ref_core <- cores[1L]
  comp_rows <- list()

  for (k in seq.int(2L, length(cores))) {
    cur <- results[[k]]
    is_ident <- identical(ref, cur)
    max_diff <- compute_max_abs_diff(ref, cur)
    matched <- if (tolerance == 0) {
      is_ident
    } else {
      isTRUE(all.equal(ref, cur, tolerance = tolerance))
    }
    comp_rows[[k - 1L]] <- tibble::tibble(
      ref_cores = ref_core,
      test_cores = cores[k],
      identical = is_ident,
      within_tolerance = matched,
      max_abs_diff = max_diff,
      status = if (matched) "pass" else "fail"
    )
  }

  comparisons <- do.call(rbind, comp_rows)
  overall <- if (all(comparisons$status == "pass")) "pass" else "fail"
  max_diff_all <- if (all(is.na(comparisons$max_abs_diff))) NA_real_ else max(comparisons$max_abs_diff, na.rm = TRUE)

  structure(
    list(
      comparisons = tibble::as_tibble(comparisons),
      cores_tested = cores,
      seed = as.integer(seed)[1L],
      tolerance = tolerance,
      identical_all = all(comparisons$identical),
      max_abs_diff = max_diff_all,
      status = overall
    ),
    class = "statci_parallel_rng"
  )
}

compute_max_abs_diff <- function(a, b) {
  ua <- tryCatch(unlist(a, use.names = FALSE), error = function(e) NULL)
  ub <- tryCatch(unlist(b, use.names = FALSE), error = function(e) NULL)
  if (is.numeric(ua) && is.numeric(ub) && length(ua) == length(ub) && length(ua) > 0L) {
    diffs <- abs(ua - ub)
    if (all(is.na(diffs))) return(NA_real_)
    return(max(diffs, na.rm = TRUE))
  }
  if (identical(a, b)) 0 else NA_real_
}

#' @export
as_tibble.statci_parallel_rng <- function(x, ...) {
  x$comparisons
}

#' @export
print.statci_parallel_rng <- function(x, ...) {
  badge <- format_status_badge(x$status)
  cli::cli_text(
    "{badge} {.strong Parallel RNG Invariance}: cores = {paste(x$cores_tested, collapse = ' vs ')}, ",
    "identical = {x$identical_all}, max |diff| = {fmt_num(x$max_abs_diff, 6L)}"
  )
  invisible(x)
}
