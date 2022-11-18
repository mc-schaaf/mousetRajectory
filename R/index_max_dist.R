#' @title Time-point of maximum velocity
#'
#' @description Computes the index of the peak velocity of a trajectory,
#' defined by arrays of x and y coordinates, and assumed to be equidistant in
#' time.
#'
#' @param x_vector Vector of the x-coordinates of the executed trajectory.
#' @param y_vector Vector of the y-coordinates of the executed trajectory.
#'
#' @return Single number indicating the index of peak velocity.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#' data("dat_one_trajectory")
#' index_max_dist(dat_one_trajectory$xvals, dat_one_trajectory$yvals)
#'
#' @export
#'

index_max_dist <- function(x_vector, y_vector) {
  # distances
  ds_real <-
    sqrt(
      (x_vector[2:length(x_vector)] - x_vector[1:(length(x_vector)-1)])^2 +
        (y_vector[2:length(y_vector)] - y_vector[1:(length(y_vector)-1)])^2
    )

  return(which.max(ds_real))

}
