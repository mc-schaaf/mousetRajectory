test_that("interp2: output length", {
  for (i in 2:10) {
    expect_equal(length(interp2(0:1, 0:1, (2^i) - 1)), (2^i) - 1)
  }
})

test_that("interp2: output range", {
  for (i in 2:10) {
    expect_equal(min(interp2(0:(2^i), 0:(2^i), (2^i) + 1)), 0)
    expect_equal(max(interp2(0:(2^i), 0:(2^i), (2^i) + 1)), (2^i))
  }
})

test_that("interp2: output accuracy", {
  for (i in 2:10) {
    expect_equal((interp2(0:(2^i), 0:(2^i), (2^i) + 1)), 0:((2^i)))
  }
  skip_on_cran()
  random_vals <- sample(2:(2^10), 10)
  for (i in random_vals) {
    curr_mult <- runif(n = 1, min = 1, max = i)
    expect_equal((interp2(c(i, 2 * i), c(0, i) * curr_mult, i + 1)), (0:i) * curr_mult)
  }
})
