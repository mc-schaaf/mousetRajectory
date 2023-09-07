#' @title Test if vector is monotonically increasing along the ideal trajectory
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' Checks if a trajectory, defined by vectors of x and y
#' coordinates, is monotonically increasing relative to an
#' ideal line passing through the start and end points.
#'
#' @inheritParams auc
#' @param strict Must the values increase _strictly_? Defaults to `FALSE`,
#' indicating that a weak, not a strict definition of monotony is applied.
#' @param warn Will a warning be issued if the trajectory is not monotonic
#' (relative to the ideal line)? Defaults to `TRUE`.
#'
#' @details
#' Computes the orthogonal projection of the trajectory points onto the ideal
#' line and checks whether the distances of this projection to the start point
#' are monotonic.
#' All objects of length 0 or 1 are monotonic.
#' Data with missing values will not pass the check.
#'
#' @returns A length-one logical, indicating whether the trajectory is
#' monotonic.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#' # common use-case: exclude movements that miss the target area and to go back
#' # movement 1:
#' x_vals1 <- c(0, 0.95, 1)
#' y_vals1 <- c(0, 1.3, 1)
#' # movement 2:
#' x_vals2 <- y_vals1
#' y_vals2 <- x_vals1
#' # movement 3:
#' x_vals3 <- c(0, -0.1, 0.5, 1)
#' y_vals3 <- c(0, 0.5, 0, 1)
#'
#' # note that the first two movements are symmetric to the ideal line:
#' plot(x_vals1, y_vals1, type = "l", xlim = c(-0.1, 1.3), ylim = c(-0.1, 1.3))
#' lines(x_vals2, y_vals2, type = "l")
#' lines(x_vals3, y_vals3, type = "l")
#' lines(c(0, 1), c(0, 1), lty = "dashed", lwd = 2) # ideal
#' is_monotonic_along_ideal(x_vals1, y_vals1, warn = FALSE)
#' is_monotonic_along_ideal(x_vals2, y_vals2, warn = FALSE)
#' is_monotonic_along_ideal(x_vals3, y_vals3, warn = FALSE)
#' # Note that the third movement is regarded as monotonic although both
#' # x and y coordinates are not.
#' # In contrast, excluding movements based on monotony of the y-coordinate
#' # would exclude the first and third movement:
#' is_monotonic(y_vals1, warn = FALSE)
#' is_monotonic(y_vals2, warn = FALSE)
#' is_monotonic(y_vals3, warn = FALSE)
#'
#' # Also works if movements go into negative direction:
#' # movement 1:
#' x_vals1 <- c(0, -0.95, -1)
#' y_vals1 <- c(0, 1.3, 1)
#' # movement 3:
#' x_vals3 <- c(0, 0.1, -0.5, -1)
#' y_vals3 <- c(0, 0.5, 0, 1)
#'
#' plot(x_vals1, y_vals1, type = "l", xlim = c(-1.3, 0.1), ylim = c(-0.1, 1.3))
#' lines(x_vals3, y_vals3, type = "l")
#' lines(-c(0, 1), c(0, 1), lty = "dashed", lwd = 2) # ideal
#' is_monotonic_along_ideal(x_vals1, y_vals1, warn = FALSE)
#' is_monotonic_along_ideal(x_vals3, y_vals3, warn = FALSE)
#' @export
#'

is_monotonic_along_ideal <- function(x_vector,
                                     y_vector,
                                     x_start,
                                     y_start,
                                     x_end,
                                     y_end,
                                     strict = TRUE,
                                     warn = TRUE) {
  # check for optional parameters
  if (missing(x_start)) {
    x_start <- x_vector[1]
  }
  if (missing(y_start)) {
    y_start <- y_vector[1]
  }
  if (missing(x_end)) {
    x_end <- x_vector[length(x_vector)]
  }
  if (missing(y_end)) {
    y_end <- y_vector[length(x_vector)]
  }

  # input check
  if (length(x_vector) <= 1) {
    return(TRUE)
  }
  if (!is.numeric(x_vector) || !is.numeric(y_vector)) {
    if (warn) {
      warning("The supplied 'numeric_vector' is not of type numeric!")
    }
    return(FALSE)
  }
  # check for NA values
  if (any(is.na(x_vector)) || any(is.na(y_vector))) {
    if (warn) {
      warning("'numeric_vector' contains NA values!")
    }
    return(FALSE)
  }

  # shift data
  x_shift <- x_vector - x_start
  y_shift <- y_vector - y_start

  # rotate data to ideal trajectory, as defined by start to end points
  angle <- atan2(y_end - y_start, x_end - x_start)
  sin1 <- sin(-angle)
  cos1 <- cos(-angle)

  x_rot <- x_shift * cos1 - y_shift * sin1

  # check for monotony
  diffs <- x_rot[2:length(x_rot)] - x_rot[1:(length(x_rot) - 1)]

  if (strict) {
    out <- all(diffs > 0)
  } else {
    out <- all(diffs >= 0)
  }

  if (warn && !out) {
    warning("'numeric_vector' is not monotonic!")
  }

  return(out)
}
