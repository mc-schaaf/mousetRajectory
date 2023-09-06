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
