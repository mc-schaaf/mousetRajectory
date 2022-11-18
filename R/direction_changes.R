#' @title xFlips
#'
#' @description Checks how often a number sequence changes from decreasing
#' monotonically to increasing monotonically (or vice versa).
#' The order of the array indicates timeadjacency.
#'
#' @param numeric_array array of numbers ordered by their time of appearance.
#'
#' @return Single number indicating how often \code{numeric_array}
#' changes direction.
#'
#' @details Values do not have to be strictly monotonically in-/decreasing.
#' I.e., \code{c(0, 1, 1, 2)} would return 0,
#' as \eqn{x_n >= x_n-1} is satisfied for \eqn{2 <= n <= length(c(0, 1, 1, 2))}.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#' direction_changes(c(0, 1, 1, 2))
#' direction_changes(c(0, 1, 1, 0))
#' direction_changes(c(0, 1, 0, 1))
#'
#' @export
#'

direction_changes <- function(numeric_array) {
  # input check
  if (length(numeric_array) <= 3) {
    if (length(numeric_array) != 0) {
      warning("Less than three values supplied!")
    }
    return(NA)
  }

  # compute first derivative
  d1 <- numeric_array[2:length(numeric_array)] -
    numeric_array[1:(length(numeric_array) - 1)]

  # throw out cases where consecutive values are the same
  d1 <- d1[d1 != 0]

  # compute array of booleans indicating if numeric_array inceases or decreases
  bool1 <- d1 > 0

  # compute if consecutive bools differ, and sum up how often this happens
  n_changes <- sum(bool1[1:(length(bool1) - 1)] != bool1[2:length(bool1)])

  return(n_changes)

}
