# Simulation-Based Calibration (SBC) rank-uniformity auditing (Talts et al., 2018)

#' Audit posterior calibration via Simulation-Based Calibration (SBC)
#'
#' Implements Simulation-Based Calibration (Talts et al., 2018) for Bayesian or
#' posterior-sampling estimators. Repeatedly draws true parameters
#' \eqn{\tilde{\theta} \sim p(\theta)}, simulates synthetic data
#' \eqn{\tilde{y} \sim p(y \mid \tilde{\theta})}, draws \eqn{L} posterior
#' samples, and evaluates uniformity of the rank statistics via Pearson's
#' chi-square test and a Kolmogorov-Smirnov test, together with empirical 50%
#' and 90% central credible-interval coverage.
#'
#' @param prior Function of zero arguments returning a numeric vector (or named
#'   numeric scalar) drawn from the prior distribution \eqn{p(\theta)}.
#' @param dgp Function `function(theta)` simulating data from the likelihood
#'   given parameter vector `theta`.
#' @param posterior_draws Function `function(data)` returning posterior draws:
#'   either a numeric vector of length \eqn{L} (for a scalar parameter) or an
#'   \eqn{L \times p} numeric matrix/data frame (columns matching parameters).
#' @param n_sim Integer number of SBC replications (default `100L`).
#' @param n_bins Integer number of histogram bins for the chi-square uniformity
#'   test (default `10L`).
#' @param seed Integer RNG seed for reproducibility (default `42L`).
#' @param mc_alpha Significance threshold for failing uniformity tests (default
#'   `0.01`). Values between `mc_alpha` and `0.05` emit `"warn"`.
#'
#' @return An S3 object of class `"statci_sbc"` containing `summary` (a
#'   [tibble::tibble] per parameter), `ranks` (matrix of rank statistics),
#'   `bin_counts`, `n_sim`, `n_draws`, `n_bins`, and `status`.
#' @export
#'
#' @examples
#' # Exact Normal-Normal conjugate posterior (calibrated -> PASS)
#' sbc <- audit_sbc(
#'   prior = function() c(mu = rnorm(1, 0, 2)),
#'   dgp = function(theta) rnorm(20, mean = theta[["mu"]], sd = 1),
#'   posterior_draws = function(y) {
#'     post_var <- 1 / (1 / 4 + length(y))
#'     post_mean <- post_var * sum(y)
#'     rnorm(99, mean = post_mean, sd = sqrt(post_var))
#'   },
#'   n_sim = 80L,
#'   n_bins = 10L,
#'   seed = 42L
#' )
#' sbc
audit_sbc <- function(prior,
                      dgp,
                      posterior_draws,
                      n_sim = 100L,
                      n_bins = 10L,
                      seed = 42L,
                      mc_alpha = 0.01) {
  if (!is.function(prior) || !is.function(dgp) || !is.function(posterior_draws)) {
    rlang::abort("`prior`, `dgp`, and `posterior_draws` must all be functions.")
  }
  n_sim <- as.integer(n_sim)[1L]
  n_bins <- as.integer(n_bins)[1L]
  if (is.na(n_sim) || n_sim < 10L) {
    rlang::abort("`n_sim` must be an integer >= 10.")
  }
  if (is.na(n_bins) || n_bins < 2L) {
    rlang::abort("`n_bins` must be an integer >= 2.")
  }

  withr::local_seed(
    as.integer(seed)[1L],
    .rng_kind = "Mersenne-Twister",
    .rng_normal_kind = "Inversion",
    .rng_sample_kind = "Rejection"
  )

  ranks_list <- vector("list", n_sim)
  cov50_list <- vector("list", n_sim)
  cov90_list <- vector("list", n_sim)
  param_names <- NULL
  n_draws <- NA_integer_
  n_errors <- 0L

  for (s in seq_len(n_sim)) {
    step_out <- tryCatch(
      {
        th_raw <- prior()
        nm <- names(th_raw)
        th <- as.numeric(th_raw)
        if (is.null(nm)) nm <- paste0("theta_", seq_along(th))
        names(th) <- nm

        if (length(th) == 0L || any(!is.finite(th))) {
          rlang::abort("`prior()` returned non-finite parameter value(s).")
        }

        y <- dgp(th)
        dr <- posterior_draws(y)
        if (is.data.frame(dr)) dr <- as.matrix(dr)
        if (is.null(dim(dr))) {
          dr <- matrix(as.numeric(dr), ncol = 1L)
          colnames(dr) <- nm[1L]
        }
        if (ncol(dr) != length(th)) {
          rlang::abort(paste0("`posterior_draws()` returned ", ncol(dr), " column(s), expected ", length(th), "."))
        }
        if (any(!is.finite(dr))) {
          rlang::abort("`posterior_draws()` returned non-finite (NA/NaN/Inf) draw(s).")
        }
        L <- nrow(dr)
        if (L < 5L) {
          rlang::abort("`posterior_draws()` must return at least 5 draws per replication.")
        }

        rk <- numeric(length(th))
        c50 <- logical(length(th))
        c90 <- logical(length(th))
        for (j in seq_along(th)) {
          col_dr <- dr[, j]
          n_less <- sum(col_dr < th[j])
          n_ties <- sum(col_dr == th[j])
          tie_add <- if (n_ties > 0L) stats::rbinom(1L, n_ties, 0.5) else 0L
          rk[j] <- n_less + tie_add
          q50 <- stats::quantile(col_dr, probs = c(0.25, 0.75), names = FALSE)
          q90 <- stats::quantile(col_dr, probs = c(0.05, 0.95), names = FALSE)
          c50[j] <- th[j] >= q50[1L] && th[j] <= q50[2L]
          c90[j] <- th[j] >= q90[1L] && th[j] <= q90[2L]
        }
        list(ok = TRUE, names = nm, L = L, ranks = rk, c50 = c50, c90 = c90)
      },
      error = function(e) list(ok = FALSE, error = conditionMessage(e))
    )

    if (!isTRUE(step_out$ok)) {
      n_errors <- n_errors + 1L
      next
    }
    if (is.null(param_names)) {
      param_names <- step_out$names
      n_draws <- step_out$L
    }
    ranks_list[[s]] <- step_out$ranks
    cov50_list[[s]] <- step_out$c50
    cov90_list[[s]] <- step_out$c90
  }

  valid_idx <- which(!vapply(ranks_list, is.null, logical(1L)))
  n_valid <- length(valid_idx)
  if (n_valid < 10L) {
    empty_sum <- tibble::tibble(
      parameter = character(0), n_valid = integer(0), n_draws = integer(0),
      n_bins = integer(0), chisq_stat = numeric(0), chisq_p = numeric(0),
      ks_stat = numeric(0), ks_p = numeric(0), cov_50 = numeric(0),
      cov_90 = numeric(0), status = character(0)
    )
    return(structure(
      list(
        summary = empty_sum,
        ranks = matrix(numeric(0), nrow = 0L, ncol = 0L),
        bin_counts = list(),
        n_sim = n_sim,
        n_valid = as.integer(n_valid),
        n_errors = as.integer(n_errors),
        n_draws = n_draws,
        n_bins = n_bins,
        mc_alpha = mc_alpha,
        status = "fail"
      ),
      class = "statci_sbc"
    ))
  }

  ranks_mat <- do.call(rbind, ranks_list[valid_idx])
  colnames(ranks_mat) <- param_names
  c50_mat <- do.call(rbind, cov50_list[valid_idx])
  c90_mat <- do.call(rbind, cov90_list[valid_idx])

  # Guard small-B / high-bin ratio so chi-square test stays well-posed
  eff_bins <- max(2L, min(n_bins, n_draws + 1L, as.integer(floor(n_valid / 2))))
  rows <- vector("list", length(param_names))
  bin_counts_list <- list()

  for (j in seq_along(param_names)) {
    pnm <- param_names[j]
    rk <- ranks_mat[, j]
    # Map discrete uniform 0..L into 1..eff_bins
    bins <- pmin(eff_bins, floor(rk * eff_bins / (n_draws + 1)) + 1L)
    counts <- tabulate(bins, nbins = eff_bins)
    bin_counts_list[[pnm]] <- counts

    # Exact probability per bin under DiscreteUniform{0..L}
    all_r <- 0:n_draws
    all_b <- pmin(eff_bins, floor(all_r * eff_bins / (n_draws + 1)) + 1L)
    bin_probs <- tabulate(all_b, nbins = eff_bins) / (n_draws + 1)

    sim_p <- any(n_valid * bin_probs < 5)
    chi <- suppressWarnings(
      stats::chisq.test(counts, p = bin_probs, simulate.p.value = sim_p, B = 2000L)
    )
    u <- (rk + 0.5) / (n_draws + 1)
    ks <- suppressWarnings(stats::ks.test(u, "punif"))

    cov50 <- mean(c50_mat[, j])
    cov90 <- mean(c90_mat[, j])
    bt90 <- stats::binom.test(sum(c90_mat[, j]), n_valid, p = 0.90)

    chi_p <- as.numeric(chi$p.value)
    ks_p <- as.numeric(ks$p.value)
    st <- if (chi_p < mc_alpha || ks_p < mc_alpha) {
      "fail"
    } else if (chi_p < 0.05 || ks_p < 0.05 || bt90$p.value < mc_alpha) {
      "warn"
    } else {
      "pass"
    }

    rows[[j]] <- tibble::tibble(
      parameter = pnm,
      n_valid = n_valid,
      n_draws = n_draws,
      n_bins = eff_bins,
      chisq_stat = as.numeric(chi$statistic),
      chisq_p = chi_p,
      ks_stat = as.numeric(ks$statistic),
      ks_p = ks_p,
      cov_50 = cov50,
      cov_90 = cov90,
      status = st
    )
  }

  summary_tbl <- do.call(rbind, rows)
  overall <- combine_status(summary_tbl$status)
  if (n_errors / n_sim > 0.05) {
    overall <- combine_status(c(overall, "fail"))
  }

  structure(
    list(
      summary = tibble::as_tibble(summary_tbl),
      ranks = ranks_mat,
      bin_counts = bin_counts_list,
      n_sim = n_sim,
      n_valid = n_valid,
      n_errors = n_errors,
      n_draws = n_draws,
      n_bins = eff_bins,
      mc_alpha = mc_alpha,
      status = overall
    ),
    class = "statci_sbc"
  )
}

#' @export
as_tibble.statci_sbc <- function(x, ...) {
  x$summary
}

#' @export
print.statci_sbc <- function(x, ...) {
  badge <- format_status_badge(x$status)
  cli::cli_text(
    "{badge} {.strong Simulation-Based Calibration (SBC)}: ",
    "{x$n_valid}/{x$n_sim} reps, L = {x$n_draws} draws, {x$n_bins} bins"
  )
  for (i in seq_len(nrow(x$summary))) {
    r <- x$summary[i, ]
    b <- format_status_badge(r$status)
    cli::cli_text(
      "  {b} {r$parameter}: chi2 p = {fmt_num(r$chisq_p, 4L)}, ",
      "KS p = {fmt_num(r$ks_p, 4L)}, 50% cov = {fmt_pct(r$cov_50)}, 90% cov = {fmt_pct(r$cov_90)}"
    )
  }
  invisible(x)
}

#' Plot SBC rank histogram with expected uniformity band
#'
#' @param x A `"statci_sbc"` object from [audit_sbc()].
#' @param parameter Optional parameter name to plot (defaults to the first).
#' @param ... Unused.
#'
#' @return A `ggplot` object if `ggplot2` is installed, otherwise draws a base
#'   R barplot and returns `x` invisibly.
#' @export
#'
#' @examples
#' sbc <- audit_sbc(
#'   prior = function() c(mu = rnorm(1, 0, 2)),
#'   dgp = function(th) rnorm(15, mean = th[["mu"]], sd = 1),
#'   posterior_draws = function(y) {
#'     pv <- 1 / (1 / 4 + length(y))
#'     rnorm(49, mean = pv * sum(y), sd = sqrt(pv))
#'   },
#'   n_sim = 30L,
#'   n_bins = 5L,
#'   seed = 42L
#' )
#' plot(sbc)
plot.statci_sbc <- function(x, parameter = NULL, ...) {
  pnm <- parameter %||% x$summary$parameter[1L]
  if (!(pnm %in% names(x$bin_counts))) {
    rlang::abort(paste0("Unknown parameter `", pnm, "`."))
  }
  counts <- x$bin_counts[[pnm]]
  n_b <- length(counts)
  exp_c <- x$n_valid / n_b
  lo <- stats::qbinom(0.005, size = x$n_valid, prob = 1 / n_b)
  hi <- stats::qbinom(0.995, size = x$n_valid, prob = 1 / n_b)

  if (rlang::is_installed("ggplot2")) {
    df <- data.frame(bin = seq_len(n_b), count = counts)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["bin"]], y = .data[["count"]])) +
      ggplot2::annotate("rect", xmin = 0.5, xmax = n_b + 0.5, ymin = lo, ymax = hi, fill = "#CBD5E1", alpha = 0.5) +
      ggplot2::geom_hline(yintercept = exp_c, linetype = "dashed", color = "#334155") +
      ggplot2::geom_col(fill = "#2563EB", alpha = 0.85, width = 0.8) +
      ggplot2::labs(
        title = paste0("SBC Rank Histogram: ", pnm),
        subtitle = paste0("Shaded band: 99% uniform expectation (S = ", x$n_valid, ", B = ", n_b, ")"),
        x = "Rank Bin",
        y = "Count"
      ) +
      ggplot2::theme_minimal()
    return(p)
  }

  graphics::barplot(
    counts,
    names.arg = seq_len(n_b),
    col = "#2563EB",
    main = paste0("SBC Rank Histogram: ", pnm),
    xlab = "Rank Bin",
    ylab = "Count"
  )
  graphics::abline(h = c(lo, exp_c, hi), lty = c(2, 1, 2), col = "#334155")
  invisible(x)
}
