#' @title Test if vector is monotonically in-/decreasing
#'
#' @description Checks if a `numeric_vector` is monotonically in-/decreasing.
#' In particular, it always a good idea to check the time stamps of trajectory
#' data and verify that the logging worked properly.
#'
#' @param numeric_vector Number sequence to-be checked.
#' @param decreasing Should the `numeric_vector` be increasing or decreasing?
#' Defaults to `FALSE`.
#' @param strict Must the values in-/decrease _strictly_? Defaults to `TRUE`,
#' indicating that a strict, not a weak definition of monotony is applied.
#' @param warn Will a warning be issued if the `numeric_vector` is not
#' monotonic? Defaults to `TRUE`.
#'
#' @details All objects of length 0 or 1 are monotonic. Data with missing
#' values will not pass the check.
#'
#' @returns A length-one logical, indicating whether the vector is monotonic.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#'
#' is_monotonic(c(1, 2, 3, 4), warn = FALSE)
#' is_monotonic(c(1, 2, 2, 3), warn = FALSE)
#' is_monotonic(c(1, 2, 2, 3), strict = FALSE, warn = FALSE)
#' is_monotonic(c(4, 0, -1, -1, -5),
#'   decreasing = TRUE,
#'   strict = FALSE, warn = FALSE
#' )
#'
#' @export
#'

is_monotonic <- function(numeric_vector,
                         decreasing = FALSE,
                         strict = TRUE,
                         warn = TRUE) {
  # input check
  if (length(numeric_vector) <= 1) {
    return(TRUE)
  }
  if (!is.numeric(numeric_vector)) {
    if (warn) {
      warning("The supplied 'numeric_vector' is not of type numeric!")
    }
    return(FALSE)
  }
  # check for NA values
  if (any(is.na(numeric_vector))) {
    if (warn) {
      warning("'numeric_vector' contains NA values!")
    }
    return(FALSE)
  }

  # check for monotony
  diffs <-
    numeric_vector[2:length(numeric_vector)] -
    numeric_vector[1:(length(numeric_vector) - 1)]

  if (decreasing) {
    diffs <- -diffs
  }

  if (strict) {
    out <- all(diffs > 0)
  } else {
    out <- all(diffs >= 0)
  }

  if (warn && !out) {
    warning("'numeric_vector' is not monotonic!")
  }

  return(out)
}
