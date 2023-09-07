test_that("MON-id: strong/weak monotonic increase - basic monotony tests", {
  expect_true(is_monotonic_along_ideal(c(1, 2, 3, 4),
    rep(0, 4),
    warn = FALSE
  ))
  expect_false(is_monotonic_along_ideal(c(1, 2, 2, 3),
    rep(0, 4),
    warn = FALSE, strict = TRUE
  ))
  expect_true(is_monotonic_along_ideal(c(1, 2, 2, 3),
    rep(0, 4),
    warn = FALSE, strict = FALSE
  ))
})

test_that("MON: check warning functionality", {
  expect_warning(
    is_monotonic_along_ideal(c(1, 2, 1, 2), rep(0, 4), warn = TRUE)
    )
})

test_that("MON: only one value supplied", {
  expect_true(is_monotonic_along_ideal(1, 1))
})

test_that("MON-id: NA value", {
  expect_false(is_monotonic_along_ideal(c(1, 2, NA, 4, 5, 6),
    rep(0, 6),
    warn = FALSE
  ))
  expect_warning(is_monotonic_along_ideal(c(1, 2, NA, 4, 5, 6),
    rep(0, 6),
    warn = TRUE
  ))
})

test_that("MON-id: crappy input", {
  expect_warning(is_monotonic_along_ideal(c("1", "2", "3"), c(1, 2, 3)))
  expect_warning(is_monotonic_along_ideal(c(1, 2, 3), c("1", "2", "3")))
  expect_warning(is_monotonic_along_ideal(c("1", "2", "3"), c("1", "2", "3")))
})

test_that("MON-id: test compairson relative to ideal", {
  # base case
  expect_false(is_monotonic_along_ideal(c(1, 2, 3, 4),
    rep(0, 4),
    x_end = -4, warn = FALSE
  ))

  # test different rotations of a sine wave as something that is monotonic
  x_template <- seq(0, 10 * pi, length.out = 1000)
  y_template <- sin(x_template)
  any_fail <- FALSE
  for (i in 1:1000) {
    random_x <- x_template + rnorm(1, sd = 10)
    random_y <- y_template + rnorm(1, sd = 10)
    random_angle <- runif(1, min = 0, max = pi)
    random_x_rot <- random_x * cos(random_angle) - random_y * sin(random_angle)
    random_y_rot <- random_x * sin(random_angle) + random_y * cos(random_angle)
    any_fail <- any_fail |
      !is_monotonic_along_ideal(random_x_rot, random_y_rot, warn = FALSE)
  }
  expect_false(any_fail)

  # test different rotations of a sine wave as something that is
  # monotonic, but swap last two x values
  x_template <- seq(0, 10 * pi, length.out = 1000)
  y_template <- sin(x_template)
  tmp <- x_template[length(x_template)]
  x_template[length(x_template)] <- x_template[length(x_template) - 1]
  x_template[length(x_template) - 1] <- tmp

  all_fail <- TRUE
  for (i in 1:1000) {
    random_x <- x_template + rnorm(1, sd = 10)
    random_y <- y_template + rnorm(1, sd = 10)
    random_angle <- runif(1, min = 0, max = pi)
    random_x_rot <- random_x * cos(random_angle) - random_y * sin(random_angle)
    random_y_rot <- random_x * sin(random_angle) + random_y * cos(random_angle)
    all_fail <- all_fail &
      !is_monotonic_along_ideal(random_x_rot, random_y_rot, warn = FALSE)
  }
  expect_true(all_fail)
})



test_that("MON-id: Test examples", {
  # movement 1:
  x_vals1 <- c(0, 0.95, 1)
  y_vals1 <- c(0, 1.3, 1)
  # movement 2:
  x_vals2 <- y_vals1
  y_vals2 <- x_vals1
  # movement 3:
  x_vals3 <- c(0, -0.1, 0.5, 1)
  y_vals3 <- c(0, 0.5, 0, 1)
  # movement 4:
  x_vals4 <- c(0, -0.95, -1)
  y_vals4 <- c(0, 1.3, 1)
  # movement 5:
  x_vals5 <- c(0, 0.1, -0.5, -1)
  y_vals5 <- c(0, 0.5, 0, 1)


  expect_false(is_monotonic_along_ideal(x_vals1, y_vals1, warn = FALSE))
  expect_false(is_monotonic_along_ideal(x_vals2, y_vals2, warn = FALSE))
  expect_true(is_monotonic_along_ideal(x_vals3, y_vals3, warn = FALSE))
  expect_false(is_monotonic_along_ideal(x_vals4, y_vals4, warn = FALSE))
  expect_true(is_monotonic_along_ideal(x_vals5, y_vals5, warn = FALSE))
})
