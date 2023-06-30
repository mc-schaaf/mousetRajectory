test_that("SA: rightward motion", {
  expect_equal(starting_angle(0, 1, 0, 0), 90)
})

test_that("SA: leftward motion", {
  expect_equal(starting_angle(0, -1, 0, 0), -90)
})

test_that("SA: upward motion", {
  expect_equal(starting_angle(0, 0, 0, 1), 0)
})

test_that("SA: right-down motion", {
  expect_equal(starting_angle(0, 1, 0, -1), 135)
})

test_that("SA: left-down motion", {
  expect_equal(starting_angle(0, -1, 0, -1), -135)
})

test_that("SA: axis-swap1", {
  expect_equal(starting_angle(0, 1, 0, 0, swap_x_y = FALSE), 0)
})

test_that("SA: axis-swap2", {
  expect_equal(starting_angle(0, -1, 0, 0, swap_x_y = FALSE), 180)
})

test_that("SA: axis-swap3", {
  expect_equal(starting_angle(0, -sqrt(2), 0, -sqrt(2), swap_x_y = FALSE), -135)
})

test_that("SA: crappy input", {
  expect_equal(starting_angle(0, -sqrt(2), 0, NA, swap_x_y = FALSE), NA)
})
