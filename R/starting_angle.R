#' @title Starting angle
#'
#' @description Computes the angle (in degrees) between a line,
#' defined by two points with coordinates (`x0`, `y0`)
#' and (`x1`, `y1`), and the specified axis.
#'
#' @param x0 x-value of the first point.
#' @param x1 x-value of the second point.
#' @param y0 y-value of the first point.
#' @param y1 y-value of the second point.
#' @param swap_x_y Whether to compute the angle relative to the x or y axis.
#' Defaults to `TRUE`, indicating that the angle is relative to the y axis.
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
#'
#' @examples
#' # Note that not the mathematical definition of angle is used by default:
#' starting_angle(0, 1, 0, 0)
#' starting_angle(0, 1, 0, 0, swap_x_y = FALSE)
#' # angles are clockwise and relative to the y-axis.
#'
#' # Note that return values are in the range [-180, 180], not [0, 360]:
#' starting_angle(0, -1, 0, -1)
#' starting_angle(0, 1, 0, -1, swap_x_y = FALSE)
#'
#' @export
#'

starting_angle <- function(x0, x1, y0, y1, swap_x_y = TRUE) {
  # check inputs
  if (any(is.na(c(x0, x1, y0, y1))) ||
        min(c(length(x0), length(x1), length(y0), length(y1))) < 1) {
    return(NA)
  }

  if (!swap_x_y) {
    atan2_in_rad <- atan2(y1 - y0, x1 - x0)
  } else {
    atan2_in_rad <- atan2(x1 - x0, y1 - y0)
  }
  return(atan2_in_rad * (180 / pi))
}
