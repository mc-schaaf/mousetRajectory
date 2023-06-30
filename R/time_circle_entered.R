#' @title Completion Time
#'
#' @description Checks when the specified circle was first entered by a
#' trajectory.
#'
#' @inheritParams time_circle_left
#'
#' @return Value of `t_vector` at the first time at which the trajectory
#' is in the circle.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#' time_circle_entered(0:10, rep(0,11), 0:10, xMid=10, yMid=0, radius=1)
#' time_circle_entered(0:10, rep(0,11), 0:10, xMid=10, yMid=0, radius=1, include_radius = FALSE)
#'
#' @export

time_circle_entered <- function(x_vector,
                                y_vector,
                                t_vector,
                                xMid = 0,
                                yMid = 0,
                                radius = 1,
                                include_radius = TRUE) {
  # check inputs
  # stopifnot(
  #   is_xy_v(x_vector, y_vector),
  #   is_xy_v(y_vector, t_vector),
  #   is_n_a(xMid),
  #   is_n_a(yMid),
  #   is_n_a(radius),
  #   is_l_a(include_radius)
  # )

  # For each point: compute distance to center of circle
  dSquare <- (x_vector-xMid)^2 + (y_vector-yMid)^2

  # check whether distance is smaller than the radius
  if (include_radius) {
    isIn <- dSquare <= radius^2
  } else {
    isIn <- dSquare < radius^2
  }

  # sanity check 1: throw warning if the first point is not outside the circle
  if (isIn[1]) {
    warning("The first point was already in the circle! Returning the first entry of t_vector.")
  }
  # sanity check 2: return NA if no point is outside the circle
  if (all(!isIn)) {
    return(NA)
  }
  # if everything is ok: return the first time out of the circle
  index <- which.max(isIn)
  return(t_vector[index])
}
