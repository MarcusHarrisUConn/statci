# Audit parallel RNG invariance across core counts

Verifies that a stochastic pipeline produces identical results when
executed sequentially (`cores = 1`) versus in parallel (`cores = 2`,
etc.). Supports either a custom `runner(cores, seed)` function to audit
your own parallel code directly, or a task function `fun` evaluated via
[`statci_parallel_apply()`](https://marcusharrisphd.com/statci/reference/statci_parallel_apply.md).

## Usage

``` r
check_parallel_rng(
  runner = NULL,
  fun = NULL,
  X = seq_len(8L),
  cores = c(1L, 2L),
  seed = 42L,
  tolerance = 0,
  ...
)
```

## Arguments

- runner:

  Optional function of signature `function(cores, seed)` that executes
  the stochastic computation using `cores` workers and master `seed`. If
  provided, `check_parallel_rng()` tests whether `runner` yields
  identical outputs across all values of `cores`.

- fun:

  Optional task function `function(i, ...)` evaluated over `X` via
  [`statci_parallel_apply()`](https://marcusharrisphd.com/statci/reference/statci_parallel_apply.md)
  when `runner` is `NULL`.

- X:

  Sequence or list of inputs for `fun` (defaults to `seq_len(8L)`).

- cores:

  Integer vector of core counts to compare (default `c(1L, 2L)`). Must
  contain at least two distinct values.

- seed:

  Integer seed passed to each run.

- tolerance:

  Numeric comparison tolerance (`0` checks bit-for-bit exactness via
  [`identical()`](https://rdrr.io/r/base/identical.html)).

- ...:

  Additional arguments forwarded to `fun`.

## Value

An S3 object of class `"statci_parallel_rng"` with `status` (`"pass"` or
`"fail"`), `comparisons` tibble, `identical_all`, `max_abs_diff`,
`cores_tested`, and `seed`.

## Examples

``` r
# Passes: deterministic L'Ecuyer stream per task
ok <- check_parallel_rng(
  runner = function(cores, seed) {
    unlist(statci_parallel_apply(1:6, function(i) mean(rnorm(20)), cores = cores, seed = seed))
  },
  cores = c(1L, 2L),
  seed = 123L
)
ok$status
#> [1] "pass"
```
