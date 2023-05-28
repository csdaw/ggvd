test_that("ellipse default creates unit circle", {
  unit_circle <- ellipse()

  expect_equal(dim(unit_circle), c(360, 2))
})
