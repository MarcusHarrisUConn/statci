# Execute a stochastic map with core-count-invariant L'Ecuyer-CMRG RNG streams

Pre-generates one independent seed per element of `X` from a master
`"L'Ecuyer-CMRG"` stream and evaluates `FUN(X[[i]], ...)` inside a
locally scoped L'Ecuyer-CMRG RNG context. Because each task receives its
own deterministic stream independent of worker partitioning, results are
bit-for-bit identical across `cores = 1`, `cores = 2`, or any larger
core count on macOS, Linux, and Windows.

## Usage

``` r
statci_parallel_apply(X, FUN, ..., cores = 1L, seed = 42L)
```

## Arguments

- X:

  A vector or list to iterate over.

- FUN:

  A function applied to each element of `X`.

- ...:

  Additional arguments passed to `FUN`.

- cores:

  Integer number of worker processes (`>= 1`). When running under
  `R CMD check` (`_R_CHECK_LIMIT_CORES_` set), `cores` is automatically
  capped at 2.

- seed:

  Integer master seed used to derive per-task L'Ecuyer streams.

## Value

A list of length `length(X)` containing the results of `FUN`.

## Examples

``` r
res1 <- statci_parallel_apply(1:4, function(i) rnorm(3), cores = 1L, seed = 42L)
res2 <- statci_parallel_apply(1:4, function(i) rnorm(3), cores = 2L, seed = 42L)
identical(res1, res2)
#> [1] TRUE
```
