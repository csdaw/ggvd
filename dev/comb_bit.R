n_sets <- 3

n_segments <- 2^3 - 1

test_mat <- matrix(0, nrow = n_segments + 1, ncol = n_sets)
test_mat

for (j in 1:n_sets) test_mat[, j] <- rep(0:1, times = 2^(j - 1), each = 2^(n_sets - j))

test_mat



expand.grid(c(0, 1), c(0, 1), c(0, 1))

f1 <- function(n) {
  expand.grid(lapply(1:n, function(x) c(0, 1)))
}

f1(3)

f2 <- function(n, boolean = FALSE) {
  out <- matrix(0, nrow = 2^n, ncol = n)

  n_rev <- n:1

  for (i in seq_len(n)) {
    out[, n_rev[i]] <- rep(0:1, times = 2^(i - 1), each = 2^(n - i))
  }

  if (boolean) out == 1 else out
}

f2(3)

eulerr:::bit_indexr(3)

# f2 almost as fast as cpp!
microbenchmark::microbenchmark(
  f1(6),
  f2(6),
  eulerr:::bit_indexr(6),
  times = 1000L
)

