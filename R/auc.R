#' @title Area Under the Curve
#'
#' @description Computes the (signed) Area Under the Curve (AUC) of a
#' path, defined by vectors of x and y coordinates, as compared to an
#' ideal line passing through the start and end points.
#'
#' @param x_vector x-coordinates of the executed path.
#' @param y_vector y-coordinates of the executed path.
#' @param x_start x-coordinate of the start point of the ideal line.
#' Defaults to the first value in `x_vector`.
#' @param y_start y-coordinate of the start point of the ideal line.
#' Defaults to the first value in `y_vector`.
#' @param x_end x-coordinate of the end point of the ideal line.
#' Defaults to the last value in `x_vector`.
#' @param y_end y-coordinate of the end point of the ideal line.
#' Defaults to the last value in `y_vector`.
#' @param geometric Whether the sign of areas that stem from a movement in the
#' reverse direction of the ideal line should be reversed.
#' Defaults to `FALSE`, indicating an time-based instead of geometric
#' interpretation. Only impacts the AUC if the trajectory is not monotonically
#' increasing relative to the ideal line.
#'
#' @returns AUC as single number (-Inf to +Inf).
#'
#' @details The ideal line is a line, not a line segment, i.e., it has
#' infinite length. The supplied vectors are assumed to be ordered by time.
#' Counterclockwise deviations from the ideal line are considered
#' positive, clockwise deviations as negative for the computation of the AUC.
#' Thus, negative AUCs are possible.
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
#' auc(x_vals, y_vals) # counterclockwise deviation: positive
#'
#' x_vals <- c(0, 1, 2, 2, 2)
#' y_vals <- c(0, 0, 0, 1, 2)
#' auc(x_vals, y_vals) # clockwise deviation: negative
#' plot(x_vals, y_vals, type = "l")
#' lines(c(0, 2), c(0, 2), lty = "dashed", lwd = 2) # ideal
#' x_vals <- -x_vals
#' auc(x_vals, y_vals) # now it is counterclockwise again
#'
#' x_vals <- c(0, 0, 1, 2, 2)
#' y_vals <- c(0, 1, 1, 1, 2)
#' plot(x_vals, y_vals, type = "l")
#' lines(c(0, 2), c(0, 2), lty = "dashed", lwd = 2) # ideal
#' auc(x_vals, y_vals) # might create small rounding errors; this should be 0
#' all.equal(0, auc(x_vals, y_vals)) # indeed interpreted by R as basically 0
#'
#' x_vals <- c(0, 1, 2, 1)
#' y_vals <- c(0, 1, 1, 0)
#' plot(x_vals, y_vals, type = "l")
#' lines(c(0, 1), c(0, 0), lty = "dashed", lwd = 2) # ideal
#' auc(x_vals, y_vals)
#' auc(x_vals, y_vals, geometric = TRUE) # note the difference
#'
#' @export
#'

auc <- function(x_vector,
                y_vector,
                x_start,
                y_start,
                x_end,
                y_end,
                geometric = FALSE) {
  # check for optional parameters
  if (missing(x_start)) {
    x_start <- x_vector[1]
  }
  if (missing(y_start)) {
    y_start <- y_vector[1]
  }
  if (missing(x_end)) {
    x_end <- x_vector[length(x_vector)]
  }
  if (missing(y_end)) {
    y_end <- y_vector[length(x_vector)]
  }

  # shift data
  x_shift <- x_vector - x_start
  y_shift <- y_vector - y_start

  # rotate data to ideal trajectory, as defined by start to end points
  angle <- atan2(y_end - y_start, x_end - x_start)
  sin1 <- sin(-angle)
  cos1 <- cos(-angle)

  x_rot <- x_shift * cos1 - y_shift * sin1
  y_rot <- x_shift * sin1 + y_shift * cos1

  # compute differences between (time-) adjacent points
  d_x <- x_rot[2:length(x_rot)] - x_rot[1:(length(x_rot) - 1)]
  d_y <- y_rot[2:length(y_rot)] - y_rot[1:(length(y_rot) - 1)]

  if (!geometric) {
    d_x <- abs(d_x)
  }

  # compute square under the curve and triangle under the curve
  auc_increment <-
    d_x * y_rot[1:(length(y_rot)) - 1] + d_x * d_y * 0.5

  # cumulate over it
  c_auc <- cumsum(auc_increment)

  return(c_auc[length(c_auc)])
}
