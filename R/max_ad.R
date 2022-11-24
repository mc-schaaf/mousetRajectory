#' @title (signed) Maximum Absolute Deviation
#'
#' @description Computes the maximum absolute deviation of a vector of points,
#' defined by vectors of x and y coordinates, as compared to an ideal
#' trajectory, defined by the coordinated of start and end points.
#'
#' @param x_vector x-coordinates of the executed trajectory
#' @param y_vector y-coordinates of the executed trajectory
#' @param x_start x-coordinate of the start point of the ideal trajectory.
#' Defaults to the first value in \code{x_vector}.
#' @param y_start y-coordinate of the start point of the ideal trajectory.
#' Defaults to the first value in \code{y_vector}.
#' @param x_end x-coordinate of the end point of the ideal trajectory.
#' Defaults to the last value in \code{x_vector}.
#' @param y_end y-coordinate of the end point of the ideal trajectory.
#' Defaults to the last value in \code{y_vector}.
#'
#' @returns MAD as single number.
#'
#' @details The ideal trajectory is thought of as being of infinite length and
#' the order of the supplied vectors indicates timeadjacency.
#' Counterclockwise deviations from the ideal trajectory are considered
#' positive, clockwise deviations as negative for the computation of the MAD.
#' Thus, negative MADs are possible.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#' data("dat_one_trajectory")
#' max_ad(dat_one_trajectory$xvals, dat_one_trajectory$yvals)
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

  # shift data
  x_shift <- x_vector - x_start
  y_shift <- y_vector - y_start

  # rotate data to ideal trajectory, as defined by start to end points
  angle <- atan2(y_end - y_start, x_end - x_start)
  sin1 <- sin(-angle)
  cos1 <- cos(-angle)

  # x_rot <- x_shift * cos1 - y_shift * sin1         # not needed
  y_rot <- x_shift * sin1 + y_shift * cos1

  # find the index of maximum deviation from ideal trajectory
  index <- which.max(abs(y_rot))

  return(y_rot[index])

}
