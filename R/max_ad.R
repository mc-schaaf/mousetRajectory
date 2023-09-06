#' @title (signed) Maximum Absolute Deviation
#'
#' @description Computes the (signed) Maximum Absolute Deviation (MAD) of a
#' path, defined by vectors of x and y coordinates, as compared to an
#' ideal line passing through the start and end points.
#'
#' @inheritParams auc
#'
#' @returns (signed) MAD as single number (-Inf to +Inf).
#'
#' @details The ideal line is a line, not a line segment, i.e., it has
#' infinite length. The supplied vectors are assumed to be ordered by time.
#' Counterclockwise deviations from the ideal line are considered
#' positive, clockwise deviations as negative for the computation of the MAD.
#' Thus, negative MADs are possible. If more than one value is
#' considered maximal, the first maximal value is returned.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#' x_vals <- c(0, 0, 0, 1, 2)
#' y_vals <- c(0, 1, 2, 2, 2)
#' plot(x_vals, y_vals, type = "l")
#' lines(c(0, 2), c(0, 2), lty = "dashed", lwd = 2) # ideal
#' max_ad(x_vals, y_vals) # counterclockwise deviation: positive
#'
#' x_vals <- c(0, 1, 2, 2, 2)
#' y_vals <- c(0, 0, 0, 1, 2)
#' plot(x_vals, y_vals, type = "l")
#' lines(c(0, 2), c(0, 2), lty = "dashed", lwd = 2) # ideal
#' max_ad(x_vals, y_vals) # clockwise deviation: negative
#' x_vals <- -x_vals
#' max_ad(x_vals, y_vals) # now it is counterclockwise again
#'
#' x_vals <- c(0, 0, 1, 2, 3, 6, 3)
#' y_vals <- c(0, 2, 2, 2, 2, 1, 0)
#' plot(x_vals, y_vals, type = "l")
#' lines(c(0, 3), c(0, 0), lty = "dashed", lwd = 2) # ideal
#' max_ad(x_vals, y_vals) # the ideal trajectory has infinite length
#'
#' x_vals <- c(0, 1, 2, 3)
#' y_vals <- c(0, 1, -1, 0)
#' plot(x_vals, y_vals, type = "l")
#' lines(x_vals, -y_vals, col = "red")
#' lines(c(0, 3), c(0, 0), lty = "dashed", lwd = 2) # ideal
#' max_ad(x_vals, y_vals)
#' max_ad(x_vals, -y_vals) # the "first" maximal value is returned
#'
#' @export
#'

max_ad <- function(x_vector,
                   y_vector,
                   x_start = NULL,
                   y_start = NULL,
                   x_end = NULL,
                   y_end = NULL) {
  # check for optional parameters
  if (is.null(x_start)) {
    x_start <- x_vector[1]
  }
  if (is.null(y_start)) {
    y_start <- y_vector[1]
  }
  if (is.null(x_end)) {
    x_end <- x_vector[length(x_vector)]
  }
  if (is.null(y_end)) {
    y_end <- y_vector[length(x_vector)]
  }

  # check inputs
  # stopifnot(
  #   is_xy_v(x_vector, y_vector),
  #   is_n_a(x_start),
  #   is_n_a(y_start),
  #   is_n_a(x_end),
  #   is_n_a(y_end)
  # )

  # shift data
  x_shift <- x_vector - x_start
  y_shift <- y_vector - y_start

  # rotate data to ideal trajectory, as defined by start to end points
  angle <- atan2((y_end - y_start), (x_end - x_start))
  m_sin <- sin(-angle)
  m_cos <- cos(-angle)

  # x_rot <- (x_shift*m_cos) - (y_shift*m_sin)         # not needed
  y_rot <- (x_shift * m_sin) + (y_shift * m_cos)

  # find the index of maximum deviation from ideal trajectory
  index <- which.max(abs(y_rot))

  return(y_rot[index])
}
