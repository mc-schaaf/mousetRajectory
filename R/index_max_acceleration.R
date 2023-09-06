#' @title Time point of maximum acceleration
#'
#' @description Computes the index of the peak acceleration of a trajectory,
#' defined by vectors of x and y coordinates, and assumed to be equidistant
#' in time.
#'
#' @inheritParams auc
#' @param absolute Should negative accelerations (i.e., deceleration)
#' be included? Defaults to `FALSE`.
#'
#' @return Single number indicating the index of peak acceleration (1 to +Inf).
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
#' y_vals <- c(0, 0, 0, 0, 0, 0, 0, 0, 0)
#' index_max_acceleration(x_vals, y_vals)
#' # acceleration maximal between x_vals[4] and x_vals[5]
#'
#' @export
#'

index_max_acceleration <- function(x_vector, y_vector, absolute = FALSE) {
  # check inputs
  # stopifnot(is_xy_v(x_vector, y_vector), is_l_a(absolute))

  # distances
  distances <-
    sqrt(
      (x_vector[2:length(x_vector)] - x_vector[1:(length(x_vector) - 1)])^2 +
        (y_vector[2:length(y_vector)] - y_vector[1:(length(y_vector) - 1)])^2
    )

  # acceleration = difference in velocity
  #              = differences in distances of time-equidistant timepoints
  accelerations <-
    distances[2:length(distances)] -
    distances[1:(length(distances) - 1)]

  if (absolute) {
    accelerations <- abs(accelerations)
  }

  # two data points lost by computing differences twice; offset return value
  # by 2/2=1 to account for this
  return(which.max(accelerations) + 1)
}
