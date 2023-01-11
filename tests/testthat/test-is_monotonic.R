test_that("MON: strong monotonic increase", {
  expect_true(is_monotonic(c(1,2,3,4), warn = FALSE))
})

test_that("MON: monotonic increase1", {
  expect_false(is_monotonic(c(1,2,2,3), warn = FALSE))
})

test_that("MON: monotonic increase2", {
  expect_true(is_monotonic(c(1,2,2,3), strict = FALSE, warn = FALSE))
})

test_that("MON: monotonic decrease", {
  expect_true(is_monotonic(c(4,0,-1,-1,-5), decreasing = TRUE, strict = FALSE, warn = FALSE))
})

