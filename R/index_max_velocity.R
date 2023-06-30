#' @title Time point of maximum velocity
#'
#' @description Computes the index of the peak velocity of a trajectory,
#' defined by vectors of x and y coordinates, and assumed to be equidistant
#' in time.
#'
#' @inheritParams auc
#'
#' @return Single number indicating the index of peak velocity (1 to +Inf).
#'
#' @details The supplied vectors are assumed to be ordered by time with equal
#' time differences.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#' x_vals <- c(0, 1, 2, 3, 6, 10, 12, 14, 15)
#' y_vals <- c(0, 0, 0, 0, 0,  0,  0,  0,  0)
#' index_max_velocity(x_vals, y_vals) # velocity maximal between x_vals[5] and x_vals[6]
#'
#' numbers <- seq(-(3/4)*pi, (3/4)*pi, by=0.001)
#' y_vector <- sin(numbers)
#' plot(numbers, y_vector)
#' index_max_velocity(rep(0, length(numbers)), y_vector)
#' abline(v = numbers[index_max_velocity(rep(0, length(numbers)), y_vector)])
#' which.max(cos(numbers)) # first derivative of sin, max at 0 degrees
#'
#' @export
#'

index_max_velocity <- function(x_vector, y_vector) {
  # check inputs
  # stopifnot(is_xy_v(x_vector, y_vector))

  # distances = velocity if time-difference stays constant
  ds_real <-
    sqrt(
      (x_vector[2:length(x_vector)] - x_vector[1:(length(x_vector)-1)])^2 +
      (y_vector[2:length(y_vector)] - y_vector[1:(length(y_vector)-1)])^2
    )

  return(which.max(ds_real))

}
