test_that("TTPA: simple example", {
  expect_equal(index_max_velocity(
    c(0, 1, 2, 3, 6, 10, 12, 14, 15),
    rep(0, 9)
  ), 5)
})

test_that("TTPA: trigonometric example", {
  skip_on_cran()
  numbers <- seq(-(3 / 4) * pi, (3 / 4) * pi, by = 0.001)
  should <- which.max(cos(numbers)) # first derivative of sin, max at 0 degrees
  actual <- index_max_velocity(rep(0, length(numbers)), sin(numbers))
  expect_true(abs(should - actual) <= 1)
})
