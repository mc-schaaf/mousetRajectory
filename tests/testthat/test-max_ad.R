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
