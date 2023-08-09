test_that("df2ellipse defaults work", {
  unit_circle <- df2ellipse(data.frame(x0 = 0, y0 = 0))

  expect_equal(colnames(unit_circle), c("x0", "y0", "x", "y", "group"))
  expect_equal(dim(unit_circle), c(360, 5))
})
