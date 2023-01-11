test_that("TTPA: simple example", {
  expect_equal(index_max_acceleration(c(0, 1, 2, 3, 6, 10, 12, 14, 15), rep(0, 9)), 4)
})

test_that("TTPA: trigonometric example", {
  numbers <- seq(-(1/4)*pi, (3/4)*pi, by=0.001)
  should <- which.max(abs(-sin(numbers)))
  actual <- index_max_acceleration(rep(0, length(numbers)), sin(numbers))
  expect_true(abs(should-actual) <= 3)
})

