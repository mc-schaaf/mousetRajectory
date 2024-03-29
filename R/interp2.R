#' @title Interpolation aka Time-Normalization
#'
#' @description Convenient wrapper to [`signal::interp1()`] for linear
#' interpolation. Assumes that you want interpolated values of
#' `xy_old` at `n_xy_new` equidistant data points.
#'
#' @param time_old Timestamps of the `xy_old` coordinates.
#' @param xy_old To-be normalized x or y coordinates.
#' @param n_xy_new Number of equidistant timepoints that should be generated.
#' Defaults to 101.
#'
#' @return Vector of length `n_xy_new` with interpolated x or y values.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#' plot(interp2(0:10, (0:10)^2))
#'
#' @export

interp2 <- function(time_old, xy_old, n_xy_new = 101) {
  time_old <- time_old - min(time_old)
  time_old <- time_old / max(time_old)
  time_old <- time_old * (n_xy_new - 1)

  return(interp1(time_old, xy_old, seq(0, (n_xy_new - 1)), method = "linear"))
}
