test_that("DC: increasing, 0 changes", {
  expect_equal(direction_changes(c(0, 1, 1, 2)), 0)
})

test_that("DC: increasing, 1 changes", {
  expect_equal(direction_changes(c(0, 1, 1, 0)), 1)
})

test_that("DC: increasing, 2 changes", {
  expect_equal(direction_changes(c(0, 1, 0, 1)), 2)
})

test_that("DC: decreasing, 0 changes", {
  expect_equal(direction_changes(-c(0, 1, 1, 2)), 0)
})

test_that("DC: decreasing, 1 changes", {
  expect_equal(direction_changes(-c(0, 1, 1, 0)), 1)
})

test_that("DC: decreasing, 2 changes", {
  expect_equal(direction_changes(-c(0, 1, 0, 1)), 2)
})

test_that("PC: stay on point", {
  expect_equal(direction_changes(rep(0, 100)), 0)
})

test_that("PC: crappy input", {
  expect_equal(direction_changes(1), NA)
})
