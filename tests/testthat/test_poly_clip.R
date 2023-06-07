empty <- list()
triangle <- list(x = c(0, 2, 1),
                 y = c(0, 0, 1))
square <- list(x = c(-1, 1, 1, -1),
               y = c(-1, -1, 1, 1))
big_square <- list(x = c(-1, 3, 3, -1),
                   y = c(-1, -1, 3, 3))
far_square <- list(x = c(10, 11, 11, 10),
                   y = c(20, 20, 21, 21))

test_that("poly_clip works with partially overlapping shapes", {
  expect_type(poly_clip(triangle, square, "intersection"), "list")
  expect_type(poly_clip(triangle, square, "union"), "list")
  expect_type(poly_clip(triangle, square, "xor"), "list")
  expect_type(poly_clip(triangle, square, "minus"), "list")
})

test_that("poly_clip works with coincident identical shapes", {
  expect_type(poly_clip(triangle, triangle, "intersection"), "list")
  expect_type(poly_clip(triangle, triangle, "union"), "list")
  expect_equal(poly_clip(triangle, triangle, "xor"), empty)
  expect_equal(poly_clip(triangle, triangle, "minus"), empty)
})

test_that("poly_clip works with coincident non-identical shapes", {
  expect_type(poly_clip(triangle, big_square, "intersection"), "list")
  expect_type(poly_clip(triangle, big_square, "union"), "list")
  expect_type(poly_clip(big_square, triangle, "union"), "list")
  expect_type(poly_clip(triangle, big_square, "xor"), "list")
  expect_equal(poly_clip(triangle, big_square, "minus"), empty)
})

test_that("poly_clip works with mutually exclusive shapes", {
  expect_equal(poly_clip(triangle, far_square, "intersection"), empty)
  expect_type(poly_clip(triangle, far_square, "union"), "list")
  expect_type(poly_clip(triangle, far_square, "xor"), "list")
  expect_type(poly_clip(triangle, far_square, "minus"), "list")
})

test_that("poly_clip works when one shape is empty list", {
  expect_equal(poly_clip(empty, square, "intersection"), empty)
  expect_equal(poly_clip(empty, square, "union"), square)
  expect_equal(poly_clip(square, empty, "union"), square)
  expect_equal(poly_clip(empty, empty, "minus"), empty)
})
