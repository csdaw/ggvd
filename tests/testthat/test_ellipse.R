test_that("ellipse default creates unit circle", {
  unit_circle <- ellipse()

  expect_equal(dim(unit_circle), c(360, 2))

  # Just check 4 intercepts
  expect_equal(which.max(unit_circle[, "x"]), 1)
  expect_equal(which.min(unit_circle[, "x"]), 180)
  expect_equal(which.max(unit_circle[, "y"]), 91)
  expect_equal(which.min(unit_circle[, "y"]), 270)
})
