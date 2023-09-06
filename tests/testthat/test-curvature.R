test_that("CURV: straight line", {
  expect_equal(curvature(c(0, 1, 2, 3, 4), c(0, 1, 2, 3, 4)), 1)
})

test_that("CURV: upward triangle", {
  expect_equal(curvature(c(0, 0, 0, 1, 2), c(0, 1, 2, 2, 2)), sqrt(2))
})

test_that("CURV: downward triangle", {
  expect_equal(curvature(c(0, 1, 2, 2, 2), c(0, 0, 0, 1, 2)), sqrt(2))
})

test_that("CURV: areas add to 0", {
  expect_equal(curvature(c(0, 0, 1, 2, 2), c(0, 1, 1, 1, 2)), sqrt(2))
})
