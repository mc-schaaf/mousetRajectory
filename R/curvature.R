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
#' @references Pfister, R., Tonn, S., Schaaf, M., Wirth, R. (2024).
#' mousetRajectory: Mouse tracking analyses for behavioral scientists.
#' The Quantitative Methods for Psychology, 20(3), 217-229.
#' \doi{10.20982/tqmp.20.3.p217}
#'
#'
#' @examples
#' x_vals <- c(0, 0, 0, 1, 2)
#' y_vals <- c(0, 1, 2, 2, 2)
#' plot(x_vals, y_vals, type = "l")
#' lines(c(0, 2), c(0, 2), lty = "dashed", lwd = 2) # ideal
#' curvature(x_vals, y_vals)
#'
#' x_vals <- c(0, 1, 2, 2, 2)
#' y_vals <- c(0, 0, 0, 1, 2)
#' plot(x_vals, y_vals, type = "l")
#' lines(c(0, 2), c(0, 2), lty = "dashed", lwd = 2) # ideal
#' curvature(x_vals, y_vals)
#'
#' x_vals <- c(0, 0, 1, 2, 2)
#' y_vals <- c(0, 1, 1, 1, 2)
#' plot(x_vals, y_vals, type = "l")
#' lines(c(0, 2), c(0, 2), lty = "dashed", lwd = 2) # ideal
#' curvature(x_vals, y_vals)
#'
#' @export
#'

curvature <- function(x_vector, y_vector) {
  # distance of the ideal trajectory
  d_ideal <-
    sqrt(
      (x_vector[1] - x_vector[length(x_vector)])^2 +
        (y_vector[1] - y_vector[length(y_vector)])^2
    )

  # distance of the real trajectory
  ds_real <-
    sqrt(
      (x_vector[2:length(x_vector)] - x_vector[1:(length(x_vector) - 1)])^2 +
        (y_vector[2:length(y_vector)] - y_vector[1:(length(y_vector) - 1)])^2
    )
  d_real <- sum(ds_real)

  return(d_real / d_ideal)
}
