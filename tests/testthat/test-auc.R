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

test_that("AUC: parameter 'geometric'", {
  expect_equal(auc(c(0, 1, 2, 1), c(0, 1, 1, 0), geometric = TRUE),  1)
  expect_equal(auc(c(0, 1, 2, 1), c(0, 1, 1, 0), geometric = FALSE), 2)
})

test_that("AUC: ideal line parameters", {
  x <- c(0, 0, 1)
  y <- c(0, 1, 1)

  # one parameter at a time
  expect_equal(auc(x, y, x_start=1, y_start=0), 1)
  expect_equal(auc(x, y, x_end  =1, y_end  =0), 1)

  # same ideal line, but differently written down
  expect_equal(auc(x, y, x_start=-1, y_start=-1, x_end=0, y_end=0), auc(x,y))
  expect_equal(auc(x, y, x_start=1,  y_start=1,  x_end=2, y_end=2), auc(x,y))

  # shift up/down
  expect_equal(auc(x, y, x_start=0, y_start=-1, x_end=1, y_end=0), 3/2)
  expect_equal(auc(x, y, x_start=0, y_start=1,  x_end=1, y_end=2), -1/2)

})
