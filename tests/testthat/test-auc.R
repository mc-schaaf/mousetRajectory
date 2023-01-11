test_that("AUC: positive triangle", {
  expect_equal(auc(c(0, 0, 0, 1, 2), c(0, 1, 2, 2, 2)), 2)
})

test_that("AUC: negative triangle", {
  expect_equal(auc(c(0, 1, 2, 2, 2), c(0, 0, 0, 1, 2)), -2)
})

test_that("AUC: positive triangle, negative x values", {
  expect_equal(auc(c(0, -1, -2, -2, -2), c(0, 0, 0, 1, 2)), 2)
})

test_that("AUC: areas add to 0", {
  expect_equal(auc(c(0, 0, 1, 2, 2), c(0, 1, 1, 1, 2)), 0)
})
