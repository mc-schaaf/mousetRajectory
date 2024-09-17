#' @title Completion Time
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' Checks when the specified circle was first entered by a trajectory.
#'
#' @inheritParams time_circle_left
#'
#' @return Value of `t_vector` at the first time at which the trajectory
#' is in the circle.
#'
#' @references Pfister, R., Tonn, S., Schaaf, M., Wirth, R. (2024).
#' mousetRajectory: Mouse tracking analyses for behavioral scientists.
#' The Quantitative Methods for Psychology, 20(3), 217-229.
#' \doi{10.20982/tqmp.20.3.p217}
#'
#'
#' @examples
#' time_circle_entered(0:10, rep(0, 11), 0:10,
#'   x_mid = 10, y_mid = 0, radius = 1
#' )
#' time_circle_entered(0:10, rep(0, 11), 0:10,
#'   x_mid = 10, y_mid = 0, radius = 1,
#'   include_radius = FALSE
#' )
#'
#' @export

time_circle_entered <- function(x_vector,
                                y_vector,
                                t_vector,
                                x_mid = 0,
                                y_mid = 0,
                                radius = 1,
                                include_radius = TRUE,
                                warn = TRUE) {
  # For each point: compute distance to center of circle
  d_square <- (x_vector - x_mid)^2 + (y_vector - y_mid)^2

  # check whether distance is smaller than the radius
  if (include_radius) {
    is_in <- d_square <= radius^2
  } else {
    is_in <- d_square < radius^2
  }

  # sanity check 1: throw warning if the first point is not outside the circle
  if (is_in[1] && warn) {
    warning(paste(
      "The first point was already in the circle!",
      "Returning the first entry of t_vector."
    ))
  }
  # sanity check 2: return NA if no point is outside the circle
  if (all(!is_in)) {
    return(NA)
  }
  # if everything is ok: return the first time out of the circle
  index <- which.max(is_in)
  return(t_vector[index])
}
