#' @title Area Under the Curve
#'
#' @description Computes the (cumulative) AUC of a point defined by x and y
#' coordinates, as compared to an ideal trajectory, defined by the
#' start and end points.
#' Importantly, the ideal trajectory is thought of as being of infinite length.
#' Works via matrix-rotation.
#'
#' @param x_vector Vector of the x-coordinates of the executed trajectory
#' @param y_vector Vector of the y-coordinates of the executed trajectory
#' @param x_start x-coordinate of the start point of the ideal trajectory
#' @param y_start y-coordinate of the start point of the ideal trajectory
#' @param x_end x-coordinate of the end point of the ideal trajectory
#' @param y_end y-coordinate of the end point of the ideal trajectory
#' @param cumulative Whether one single number (cumulative=F)
#' or an array of cumulative AUCs should be returned
#' @param RP_style Should areas that stem from a change of direction be added
#' or subtracted?
#'
#' @returns AUC as single number or as vector of cumulative AUCs.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#' data("dat_one_trajectory")
#' auc(dat_one_trajectory$xvals, dat_one_trajectory$yvals)
#'
#' @export
#'

auc <- function(x_vector,
                y_vector,
                x_start = NULL,
                y_start = NULL,
                x_end = NULL,
                y_end = NULL,
                cumulative = FALSE,
                RP_style = FALSE) {
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

  if (RP_style) {
    d_x <- abs(d_x)
  }

  # compute square under the curve and triangle under the curve
  AUC_increment <- d_x * y_rot[1:(length(y_rot)) - 1] + d_x * d_y * 0.5

  # cumulate over it
  c_AUC <- cumsum(AUC_increment)

  if (cumulative) {
    return(c(0, c_AUC))
  }
  return(c_AUC[length(c_AUC)])
}
