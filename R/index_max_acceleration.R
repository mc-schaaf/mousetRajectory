#' @title Time-point of maximum acceleration
#'
#' @description Computes the index of the peak acceleration of a trajectory,
#' defined by arrays of x and y coordinates, and assumed to be equidistant
#' in time.
#'
#' @param x_vector Vector of the x-coordinates of the executed trajectory.
#' @param y_vector Vector of the y-coordinates of the executed trajectory.
#' @param absolute Should negative accelerations (i.e., deceleration)
#' be included?
#'
#' @return Single number indicating the index of peak acceleration.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#' data("dat_one_trajectory")
#' index_max_acceleration(dat_one_trajectory$xvals, dat_one_trajectory$yvals)
#'
#' @export
#'

index_max_acceleration <- function(x_vector, y_vector, absolute = FALSE) {

    # distances
    distances <-
      sqrt(
        (x_vector[2:length(x_vector)] - x_vector[1:(length(x_vector)-1)])^2 +
        (y_vector[2:length(y_vector)] - y_vector[1:(length(y_vector)-1)])^2
        )

    # acceleration = difference in velocity
    #              = differences in distances of time-equidistant timepoints
    accelerations <- distances[2:length(distances)] -
      distances[1:(length(distances) - 1)]

    if (absolute) {
      accelerations <- abs(accelerations)
    }

    return(which.max(accelerations))
  }
