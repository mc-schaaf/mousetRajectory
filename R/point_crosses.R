#' @title Number of times a point is crossed
#'
#' @description Checks how often a certain point is being crossed by an number
#' sequence where the order of the array indicates timeadjacency.
#'
#' @param numeric_array Array of numbers ordered by their time of appearance.
#' @param relevant_point Number which has to be crossed.
#'
#' @returns Number of times that \code{numeric_array} crosses
#' the \code{relevant_point}.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#' data("dat_one_trajectory")
#' point_crosses(dat_one_trajectory$xvals)
#'
#' @export
#'

point_crosses <- function(numeric_array, relevant_point = 0) {
  if (length(numeric_array) < 3) {
    if (length(numeric_array) != 0) {
      warning("Less than three values supplied!")
    }
    return(NA)
  }


  # compute array of booleans indicating if value is greater than relevant_point
  bool_a <- numeric_array > relevant_point

  # compute if consecutive bools differ, and sum up how often this happens
  bool_b <- bool_a[1:(length(bool_a) - 1)] != bool_a[2:length(bool_a)]

  return(sum(bool_b))

}
