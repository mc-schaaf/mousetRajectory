#' @title Starting angle
#'
#' @description Computes the angle (in degrees) between a line,
#' defined by two points p0 and p1 with coordinates (\code{x0}, \code{y0})
#' and (\code{x1}, \code{y1}), and the specified axis.
#'
#' @param x0 x-value at the start point of your choosing.
#' @param x1 x-value at the end point of your choosing.
#' @param y0 y-value at the start point of your choosing.
#' @param y1 y-value at the end point of your choosing.
#' @param swap_x_y Whether to compute the angle relative to the x or y axis.
#' defaults to the y axis.
#'
#' @return Angle in degrees with \eqn{-180 <= angle <= 180}.
#'
#' @details If the angle is computed relative to the x axis, counterclockwise
#' changes are counted as positive. If the angle is computed relative to the
#' y axis, clockwise changes are counted as positive.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#' @examples
#' starting_angle(0, 1, 0, 1)
#' starting_angle(0, -1, 0, 1)
#' starting_angle(0, 1, 0, 1, FALSE)
#' starting_angle(0, -1, 0, 1, FALSE)
#'
#' @export
#'

starting_angle <- function(x0, x1, y0, y1, swap_x_y = TRUE){
  if (!swap_x_y) {
    atan2_in_rad <- atan2(y1-y0, x1-x0)
  } else {
    atan2_in_rad <- atan2(x1-x0, y1-y0)
  }
  return(atan2_in_rad * (180/pi))
}
