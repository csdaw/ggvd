#' Bit combinations
#'
#' @description Generate a matrix with all combinations for an `n` bit number.
#'
#' @param n `numeric` or `integer`, number of bits.
#' @param boolean `boolean`, output a binary `matrix` (default) or a boolean `matrix`.
#'
#' @return Returns a binary `matrix` by default, or a boolean `matrix`.
#'   ncol = `n`, nrow = `2^n`. The bit order in each row is little endian i.e.
#'   least significant bit first.
#' @export
#'
#' @examples
#' bit_comb(3)
#'
bit_comb <- function(n, boolean = FALSE) {
  stopifnot(length(n) == 1L, is.numeric(n))
  stopifnot(is.logical(boolean))

  out <- matrix(0, nrow = 2^n, ncol = n)

  n_rev <- n:1

  for (i in seq_len(n)) {
    out[, n_rev[i]] <- rep(0:1, times = 2^(i - 1), each = 2^(n - i))
  }

  if (boolean) out == 1 else out
}
