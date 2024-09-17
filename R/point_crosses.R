#' @title Number of times a point is crossed
#'
#' @description Checks how often a number (`relevant_point`) is being
#' crossed by an number sequence (`numeric_vector`).
#'
#' @param numeric_vector Numbers, ordered by their time of appearance.
#' @param relevant_point Number which has to be crossed.
#'
#' @returns Number of times that `numeric_vector` crosses
#' the `relevant_point` (0 to +Inf).
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
#' x_vals <- c(-1, 1, -1, 1, -1, 1)
#' point_crosses(x_vals, 0)
#' point_crosses(x_vals, 1)
#' point_crosses(x_vals, -1)
#'
#' @export
#'

point_crosses <- function(numeric_vector, relevant_point = 0) {
  if (length(numeric_vector) < 3 ||
    length(numeric_vector[numeric_vector != relevant_point]) < 3) {
    return(NA)
  }


  # compute array of booleans indicating if value is greater than relevant_point
  is_greater <-
    numeric_vector[numeric_vector != relevant_point] > relevant_point

  # compute if consecutive bools differ, and sum up how often this happens
  is_sign_diff <-
    is_greater[1:(length(is_greater) - 1)] != is_greater[2:length(is_greater)]

  return(sum(is_sign_diff))
}
