#' @title Initiation Time
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' Checks when the specified circle was first left by a trajectory.
#'
#' @inheritParams auc
#' @param t_vector Timestamps of the executed trajectory.
#' @param x_mid x-coordinate of the center of the circle.
#' @param y_mid y-coordinate of the center of the circle.
#' @param radius radius of the center of the circle.
#' @param include_radius Whether points lying exactly on the radius should be
#' included in the circle. Defaults to `TRUE`.
#' @param warn whether a warning should be thrown if the first entry of t_vector
#' is returned. Defaults to `TRUE`.
#'
#' @return Value of `t_vector` at the first time at which the trajectory
#' is out of the circle.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#' time_circle_left(0:10, rep(0, 11), 0:10)
#' time_circle_left(0:10, rep(0, 11), 0:10, include_radius = FALSE)
#'
#' @export

time_circle_left <- function(x_vector,
                             y_vector,
                             t_vector,
                             x_mid = 0,
                             y_mid = 0,
                             radius = 1,
                             include_radius = TRUE,
                             warn = TRUE) {
  # For each point: compute distance to center of circle
  d_square <- (x_vector - x_mid)^2 + (y_vector - y_mid)^2

  # check whether distance is larger than the radius
  if (include_radius) {
    is_out <- d_square > radius^2
  } else {
    is_out <- d_square >= radius^2
  }

  # sanity check 1: throw warning if the first point is not in the circle
  if (is_out[1] && warn) {
    warning(paste(
      "The first point was not in the circle!",
      "Returning the first entry of t_vector."
    ))
  }
  # sanity check 2: return NA if no point is outside the circle
  if (all(!is_out)) {
    return(NA)
  }
  # if everything is ok: return the first time out of the circle
  index <- which.max(is_out)
  return(t_vector[index])
}
