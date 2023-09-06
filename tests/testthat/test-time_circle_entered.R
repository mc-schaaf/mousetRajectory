test_that("default circle and example", {
  expect_equal(
    time_circle_entered(-5:5, rep(0, 11), -5:5),
    -1
  )
  expect_equal(
    time_circle_entered(-5:5, rep(0, 11), -5:5, include_radius = FALSE),
    0
  )

  expect_equal(
    time_circle_entered(0:10, rep(0, 11), 0:10, x_mid = 10, y_mid = 0, radius = 1),
    9
  )
  expect_equal(
    time_circle_entered(0:10, rep(0, 11), 0:10,
      x_mid = 10, y_mid = 0, radius = 1,
      include_radius = FALSE
    ),
    10
  )
})

test_that("change some parameters", {
  expect_equal(
    time_circle_entered(-5:5, rep(0, 11), -5:5, x_mid = 3, radius = 5),
    -2
  )
  expect_equal(
    time_circle_entered(-5:5, rep(0, 11), -5:5,
      x_mid = 3, radius = 5,
      include_radius = FALSE
    ),
    -1
  )

  expect_equal(
    time_circle_entered(0:10, rep(0, 11), 0:10, x_mid = 10, y_mid = 0, radius = 9),
    1
  )
  expect_equal(
    time_circle_entered(0:10, rep(0, 11), 0:10,
      x_mid = 10, y_mid = 0, radius = 9,
      include_radius = FALSE
    ),
    2
  )
})

test_that("warnings and NA", {
  expect_warning(time_circle_entered(0:10, rep(0, 11), 0:10))
  expect_warning(time_circle_entered(-5:5, rep(0, 11), radius = 999, 0:10))
  expect_equal(time_circle_entered(0:10, rep(0, 11), 0:10, x_mid = 999), NA)
})


test_that("reenter circle", {
  expect_equal(
    time_circle_entered(c(0:10, 0:10), rep(0, 22), 0:21,
      x_mid = 10, y_mid = 0, radius = 1
    ),
    9
  )
})
