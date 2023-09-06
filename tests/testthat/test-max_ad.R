test_that("MAD: positive triangle", {
  expect_equal(max_ad(c(0, 0, 0, 1, 2), c(0, 1, 2, 2, 2)), sqrt(2))
})

test_that("MAD: negative triangle", {
  expect_equal(max_ad(c(0, 1, 2, 2, 2), c(0, 0, 0, 1, 2)), -sqrt(2))
})

test_that("MAD: positive triangle, negative x values", {
  expect_equal(max_ad(c(0, -1, -2, -2, -2), c(0, 0, 0, 1, 2)), sqrt(2))
})

test_that("MAD: return first maximum - test1", {
  expect_equal(max_ad(c(0, 1, 2, 3), c(0, 1, -1, 0)), 1)
})

test_that("MAD: return first maximum - test1", {
  expect_equal(max_ad(c(0, 1, 2, 3), c(0, -1, 1, 0)), -1)
})

test_that("MAD: ideal line parameters", {
  x <- c(0, 0, 1)
  y <- c(0, 1, 1)

  # one parameter at a time
  expect_equal(max_ad(x, y, x_start = 1, y_start = 0), 1)
  expect_equal(max_ad(x, y, x_end = 1, y_end = 0), 1)

  # same ideal line, but differently written down
  expect_equal(
    max_ad(x, y, x_start = -1, y_start = -1, x_end = 0, y_end = 0),
    max_ad(x, y)
  )
  expect_equal(
    max_ad(x, y, x_start = 1, y_start = 1, x_end = 2, y_end = 2),
    max_ad(x, y)
  )

  # shift up/down
  expect_equal(
    max_ad(x, y, x_start = 0, y_start = -1, x_end = 1, y_end = 0),
    sqrt(2)
  )
  expect_equal(
    max_ad(x, y, x_start = 0, y_start = 1, x_end = 1, y_end = 2),
    -max_ad(x, y)
  )
})
