test_that("bit_comb defaults work", {
  result <- bit_comb(2)

  ground_truth <- matrix(c(0, 1, 0, 1, 0, 0, 1, 1), ncol = 2)

  expect_true(is.numeric(result))
  expect_equal(result, ground_truth)
})

test_that("bit_comb can be boolean", {
  result <- bit_comb(2, boolean = TRUE)

  ground_truth <- matrix(c(0, 1, 0, 1, 0, 0, 1, 1), ncol = 2) == 1

  expect_true(is.logical(result))
  expect_equal(result, ground_truth)
})
