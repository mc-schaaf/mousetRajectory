#' @title Curvature
#'
#' @description Computes the curvature of a path, defined by vectors
#' of x and y coordinates, as compared to an ideal path,
#' as defined by the start and end points of the path.
#'
#' @inheritParams auc
#'
#' @return Single number indicating the curvature (1 to +Inf).
#'
#' @details The supplied vectors are assumed to be ordered by time.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#' x_vals <- c(0, 0, 0, 1, 2)
#' y_vals <- c(0, 1, 2, 2, 2)
#' plot(x_vals, y_vals, type = "l")
#' lines(c(0,2), c(0,2), lty="dashed", lwd=2) # ideal
#' curvature(x_vals, y_vals)
#'
#' x_vals <- c(0, 1, 2, 2, 2)
#' y_vals <- c(0, 0, 0, 1, 2)
#' plot(x_vals, y_vals, type = "l")
#' lines(c(0,2), c(0,2), lty="dashed", lwd=2) # ideal
#' curvature(x_vals, y_vals)
#'
#' x_vals <- c(0, 0, 1, 2, 2)
#' y_vals <- c(0, 1, 1, 1, 2)
#' plot(x_vals, y_vals, type = "l")
#' lines(c(0,2), c(0,2), lty="dashed", lwd=2) # ideal
#' curvature(x_vals, y_vals)
#'
#' @export
#'

curvature <- function(x_vector, y_vector) {
  # check inputs
  # stopifnot(is_xy_v(x_vector, y_vector))

  # distance of the ideal trajectory
  d_ideal <-
    sqrt(
    (x_vector[1] - x_vector[length(x_vector)])^2 +
    (y_vector[1] - y_vector[length(y_vector)])^2
    )

  # distance of the real trajectory
  ds_real <-
    sqrt(
      (x_vector[2:length(x_vector)] - x_vector[1:(length(x_vector)-1)])^2 +
      (y_vector[2:length(y_vector)] - y_vector[1:(length(y_vector)-1)])^2
      )
  d_real <- sum(ds_real)

  return(d_real / d_ideal)
}
