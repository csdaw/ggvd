e1 <- df2ellipse(data.frame(x0 = 0, y0 = 0))[, c("x", "y")]
e2 <- df2ellipse(data.frame(x0 = 0.4, y0 = 0))[, c("x", "y")]
e3 <- df2ellipse(data.frame(x0 = 10, y0 = 0))[, c("x", "y")]
e4 <- df2ellipse(data.frame(x0 = 0.2, y0 = 0.2))[, c("x", "y")]

truth_table1 <- bit_comb(2, boolean = TRUE)[-1, ]

truth_table2 <- bit_comb(3, boolean = TRUE)[-1, ]

test_that("poly_segment works normally", {
  segments1 <- poly_segment(list(as.list(e1), as.list(e2)), truth_table1)

  expect_equal(length(segments1), 3)
  expect_equal(lengths(segments1), rep(2, 3), ignore_attr = TRUE)
})

# segments of length(0) will be discarded during calculation
test_that("poly_segment works with coincident shapes", {
  segments2 <- poly_segment(list(as.list(e1), as.list(e1)), truth_table1)

  expect_equal(length(segments2), 1)
  expect_equal(lengths(segments2), 2, ignore_attr = TRUE)
})

test_that("poly_segment works with mutually exclusive shapes", {
  segments3 <- poly_segment(list(as.list(e1), as.list(e3)), truth_table1)

  expect_equal(length(segments3), 2)
  expect_equal(lengths(segments3), rep(2, 2), ignore_attr = TRUE)
})

test_that("poly_segment works with 3 overlapping shapes", {
  segments4 <- poly_segment(list(as.list(e1), as.list(e2), as.list(e4)), truth_table2)

  expect_equal(length(segments4), 7)
  expect_equal(lengths(segments4), rep(2, 7), ignore_attr = TRUE)
})
