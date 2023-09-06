test_that("default circle", {
  expect_equal(
    time_circle_left(0:10, rep(0, 11), 0:10, include_radius = TRUE),
    2
  )
  expect_equal(
    time_circle_left(0:10, rep(0, 11), 0:10, include_radius = FALSE),
    1
  )
})

test_that("change some parameters", {
  expect_equal(
    time_circle_left(0:10, rep(0, 11), 0:10,
      radius = 4,
      include_radius = TRUE
    ),
    5
  )
  expect_equal(
    time_circle_left(0:10, rep(0, 11), 0:10,
      radius = 4,
      include_radius = FALSE
    ),
    4
  )
  expect_equal(
    time_circle_left(0:10, rep(0, 11), 0:10,
      xMid = 2, radius = 3,
      include_radius = TRUE
    ),
    6
  )
  expect_equal(
    time_circle_left(0:10, rep(0, 11), 0:10,
      xMid = 2, radius = 3,
      include_radius = FALSE
    ),
    5
  )
  expect_equal(
    time_circle_left(0:10, rep(0, 11), 0:10,
      xMid = 2, radius = 2,
      include_radius = TRUE
    ),
    5
  )
})

test_that("warnings and NA", {
  expect_warning(
    time_circle_left(0:10, rep(0, 11), 0:10,
      xMid = 2, radius = 2,
      include_radius = FALSE
    )
  )
  expect_warning(
    time_circle_left(-5:5, rep(0, 11), 0:10)
  )
  expect_equal(
    time_circle_left(0:10, rep(0, 11), 0:10, radius = 999), NA
  )
})

test_that("reenter circle", {
  expect_equal(
    time_circle_left(c(0:10, 0:10), rep(0, 22), 0:21), 2
  )
})
