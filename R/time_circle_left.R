#' @title Initiation Time
#'
#' @description Checks when the specified circle was first left.
#'
#' @inheritParams auc
#' @param t_vector Timestamps of the executed trajectory.
#' @param xMid x-coordinate of the center of the circle.
#' @param yMid y-coordinate of the center of the circle.
#' @param radius radius of the center of the circle.
#' @param include_radius Whether points lying exactly on the radius should be
#' included in the circle. Defaults to `TRUE`.
#'
#' @return Value of `t_vector` at the first time the circle is left.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#' time_circle_left(0:10, 0:10, 0:10)
#' time_circle_left(0:10, 0:10, 0:10, include_radius = TRUE)
#'
#' @export

time_circle_left <- function(x_vector,
                             y_vector,
                             t_vector,
                             xMid = 0,
                             yMid = 0,
                             radius = 1,
                             include_radius = TRUE) {
  dSquare <- (x_vector - xMid) ^ 2 + (y_vector - yMid) ^ 2
  if (include_radius) {
    isOut <- dSquare > radius ^ 2
  } else {
    isOut <- dSquare >= radius ^ 2
  }
  if (isOut[1]) {
    warning("The first point was not in the circle! Returning the first entry of t_vector.")
  }
  if (all(!isOut)) {
    return(NA)
  }
  index <- which.max(isOut)
  return(t_vector[index])
}
