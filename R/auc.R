#' @title Area Under the Curve
#'
#' @description Computes the (cumulative) Area Under the Curve (AUC) of a
#' trajectory, defined by vectors of x and y coordinates, as compared to an
#' ideal trajectory, defined by the start and end points.
#'
#' @param x_vector x-coordinates of the executed trajectory.
#' @param y_vector y-coordinates of the executed trajectory.
#' @param x_start x-coordinate of the start point of the ideal trajectory.
#' Defaults to the first value in `x_vector`.
#' @param y_start y-coordinate of the start point of the ideal trajectory.
#' Defaults to the first value in `y_vector`.
#' @param x_end x-coordinate of the end point of the ideal trajectory.
#' Defaults to the last value in `x_vector`.
#' @param y_end y-coordinate of the end point of the ideal trajectory.
#' Defaults to the last value in `y_vector`.
#' @param geometric Whether the sign of areas that stem from a movement in the
#' reverse direction of the ideal trajectory should be reversed.
#' Defaults to `FALSE`, indicating an time-based instead of geometric
#' interpretation.
#'
#' @returns Cumulative AUC as single number (-Inf to +Inf).
#'
#' @details The ideal trajectory is thought of as being of infinite length and
#' the supplied vectors are assumed to be ordered by time.
#' Counterclockwise deviations from the ideal trajectory are considered
#' positive, clockwise deviations as negative for the computation of the AUC.
#' Thus, negative AUCs are possible.
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
#' auc(x_vals, y_vals) # counterclockwise deviation: positive
#'
#' x_vals <- c(0, 1, 2, 2, 2)
#' y_vals <- c(0, 0, 0, 1, 2)
#' auc(x_vals, y_vals) # clockwise deviation: negative
#' plot(x_vals, y_vals, type = "l")
#' lines(c(0,2), c(0,2), lty="dashed", lwd=2) # ideal
#' x_vals <- -x_vals
#' auc(x_vals, y_vals) # now it is counterclockwise again
#'
#' x_vals <- c(0, 0, 1, 2, 2)
#' y_vals <- c(0, 1, 1, 1, 2)
#' plot(x_vals, y_vals, type = "l")
#' lines(c(0,2), c(0,2), lty="dashed", lwd=2) # ideal
#' auc(x_vals, y_vals) # might create small rounding errors; this should be 0
#' all.equal(0, auc(x_vals, y_vals)) # indeed interpreted by R as basically 0
#'
#' x_vals <- c(0, 1, 2, 1)
#' y_vals <- c(0, 1, 1, 0)
#' plot(x_vals, y_vals, type = "l")
#' lines(c(0,1), c(0,0), lty="dashed", lwd=2) # ideal
#' auc(x_vals, y_vals)
#' auc(x_vals, y_vals, geometric = TRUE) # note the difference
#'
#' # Inspired by a German kids' riddle:
#' x_vals <- c(0, 0, 1, 0, 1, 0, 0.5, 1, 1)
#' y_vals <- c(0, 1, 1, 0, 0, 1, 1.5, 1, 0)
#' plot(x_vals, y_vals, type = "l")
#' lines(c(0,1), c(0,0), lty="dashed", lwd=2) # ideal
#' auc(x_vals, y_vals)
#' auc(x_vals, y_vals, geometric = TRUE)
#'
#' @export
#'

auc <- function(x_vector,
                y_vector,
                x_start = NULL,
                y_start = NULL,
                x_end = NULL,
                y_end = NULL,
                geometric = FALSE) {
  # check for optional parameters
  if (is.null(x_start)) {
    x_start <- x_vector[1]
  }
  if (is.null(y_start)) {
    y_start <- y_vector[1]
  }
  if (is.null(x_end)) {
    x_end <- x_vector[length(x_vector)]
  }
  if (is.null(y_end)) {
    y_end <- y_vector[length(x_vector)]
  }

  # check inputs
  # stopifnot(
  #   is_xy_v(x_vector, y_vector),
  #   is_n_a(x_start),
  #   is_n_a(y_start),
  #   is_n_a(x_end),
  #   is_n_a(y_end),
  #   is_l_a(geometric)
  # )

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
  AUC_increment <-
    d_x * y_rot[1:(length(y_rot)) - 1] + d_x * d_y * 0.5

  # cumulate over it
  c_AUC <- cumsum(AUC_increment)

  return(c_AUC[length(c_AUC)])
}
