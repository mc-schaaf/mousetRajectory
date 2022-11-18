#' @title Curvature
#'
#' @description Computes the curvature of a trajectory, defined by arrays
#' of x and y coordinates, as compared to an ideal trajectory,
#' as defined by the start and end points of the trajectory.
#'
#' @param x_vector vector of the x-coordinates of the executed trajectory.
#' @param y_vector vector of the y-coordinates of the executed trajectory.
#'
#' @return Single number indicating the curvature.
#'
#'@references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#' data("dat_one_trajectory")
#' curvature(dat_one_trajectory$xvals, dat_one_trajectory$yvals)
#'
#' @export
#'

curvature <- function(x_vector, y_vector) {
  # distance of the ideal trajectory
  d_ideal <- sqrt(((x_vector[1] - x_vector[length(x_vector)]) ^ 2) +
                    ((y_vector[1] - y_vector[length(y_vector)]) ^ 2))

  # distance of the real trajectory
  ds_real <-
    sqrt(
      (x_vector[2:length(x_vector)] - x_vector[1:(length(x_vector)-1)])^2 +
      (y_vector[2:length(y_vector)] - y_vector[1:(length(y_vector)-1)])^2
      )
  d_real <- sum(ds_real)

  return(d_real / d_ideal)
}
