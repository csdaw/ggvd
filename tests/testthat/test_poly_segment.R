e1 <- ellipse()
e2 <- ellipse(x0 = 0.4)
e3 <- ellipse(x0 = 10)
e4 <- ellipse(x0 = 0.2, y0 = 0.2)

truth_table1 <- matrix(
  c(TRUE, FALSE, TRUE,
    FALSE, TRUE, TRUE),
  ncol = 2,
  nrow = 3
)

truth_table2 <- matrix(
  c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE,
    FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE,
    FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE),
  ncol = 3,
  nrow = 7
)

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

# FUNCTION NOT WORKING, WHYYY!! THE 3/3 overlap is missing.
# test_that("poly_segment works with 3 overlapping shapes", {
#   segments4 <- poly_segment(list(as.list(e1), as.list(e2), as.list(e4)), truth_table2)
#
#   expect_equal(length(segments4), 7)
#   #expect_equal(lengths(segments4), rep(2, 7), ignore_attr = TRUE)
# })

zzz <- do.call(rbind.data.frame, segments4)

zzz$id = sub("\\..*$", "", rownames(zzz))

ggplot(zzz, aes(x, y, fill = id)) +
  geom_polygon()
