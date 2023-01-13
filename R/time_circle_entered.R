#' @title Completion Time
#'
#' @description TODO
#'
#' @inheritParams time_circle_left.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#' time_circle_entered(0:10, 0:10, 0:10)
#' time_circle_entered(0:10, 0:10, 0:10, include_radius = TRUE)
#'
#' @export

time_circle_entered <- function(x_vector,
                                y_vector,
                                t_vector,
                                xMid = 0,
                                yMid = 0,
                                radius = 1,
                                include_radius = TRUE) {
  dSquare <- (x_vector - xMid) ^ 2 + (y_vector - yMid) ^ 2
  if (include_radius) {
    isIn <- dSquare <= radius ^ 2
  } else {
    isIn <- dSquare < radius ^ 2
  }
  if (all(!isIn)) {
    return(NA)
  }
  index <- which.max(isIn)
  if (index == 1) {
    warning("The first point was already in the circle!")
  }
  return(t_vector[index])
}
