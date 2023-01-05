#' @title Number of times a point is crossed
#'
#' @description Checks how often a number (`relevant_point`) is being
#' crossed by an number sequence (`numeric_array`).
#'
#' @param numeric_array Array of numbers ordered by their time of appearance.
#' @param relevant_point Number which has to be crossed.
#'
#' @returns Number of times that `numeric_array` crosses
#' the `relevant_point`.
#'
#' @details The order of the supplied vectors indicates timeadjacency.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#' x_vals <- c(-1, 1, -1, 1, -1, 1)
#' point_crosses(x_vals, 0)
#' point_crosses(x_vals, 1)
#' point_crosses(x_vals, -1)
#'
#' @export
#'

point_crosses <- function(numeric_array, relevant_point = 0) {
  if (length(numeric_array) < 3 |
      length(numeric_array[numeric_array != relevant_point]) < 3) {
    if (length(numeric_array) != 0) {
      warning("Too few values supplied!")
    }
    return(NA)
  }


  # compute array of booleans indicating if value is greater than relevant_point
  bool_a <- numeric_array[numeric_array != relevant_point] > relevant_point

  # compute if consecutive bools differ, and sum up how often this happens
  bool_b <- bool_a[1:(length(bool_a) - 1)] != bool_a[2:length(bool_a)]

  return(sum(bool_b))

}
