test_that("PC: actually crosses", {
  expect_equal(point_crosses(c(-1, 1, -1, 1, -1, 1), 0), 5)
})

test_that("PC: lower bound", {
  expect_equal(point_crosses(c(-1, 1, -1, 1, -1, 1), -1), 0)
})

test_that("PC: upper bound", {
  expect_equal(point_crosses(c(-1, 1, -1, 1, -1, 1), 1), 0)
})

test_that("PC: stay on point", {
  expect_equal(point_crosses(rep(0, 100)), NA)
})

