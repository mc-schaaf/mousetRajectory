#' @title Sample entropy
#'
#' @description Computes the sample entropy (sampen), as given by
#' Richman & Moorman (2000), \doi{10.1152/ajpheart.2000.278.6.H2039}.
#'
#' @param timeseries_array Array of numbers over which the sampen is to be
#' computed.
#' @param dimensions Number of embedding dimensions for which to compute the
#' sampen. Sometimes also called "template length".
#' @param tolerance Tolerance for the comparisons of two number sequences.
#' @param standardize Whether to standardize the timeseries_array.
#' @param use_diff Whether to use the differences between adjacent points.
#'
#' @return Single number indicating the sampen for the given parameters
#' (0 to +Inf).
#'
#' @details As suggested by Richman & Moorman (2000),
#' \doi{10.1152/ajpheart.2000.278.6.H2039}, the last possible vector of length
#' `dimensions` is not considered because it has no corresponding vector of
#' length `dimensions + 1`, ensuring a sampen estimation with a low bias
#' introduced by the length of the `timeseries_array`.
#' The function was deliberately implemented in R with C-style code. While this
#' makes the function rather slow for large `timeseries_array`s,
#' it enables maximal transparency. For an overview over faster sampen
#' functions in R that, however, are distributed in binary or need source
#' compilation, see Chen et al. (2019), \doi{10.1093/biomethods/bpz016}.
#'
#' @references Pfister, R., Tonn, S., Schaaf, M., Wirth, R. (2024).
#' mousetRajectory: Mouse tracking analyses for behavioral scientists.
#' The Quantitative Methods for Psychology, 20(3), 217-229.
#' \doi{10.20982/tqmp.20.3.p217}
#'
#'
#' @examples
#' x_vals <- rep(c(0, 0, 0, 0, 0, 1), 20)
#' sampen(x_vals, dimensions = 1, tolerance = 1 / 2, standardize = FALSE)
#' sampen(x_vals, dimensions = 3, tolerance = 1 / 2, standardize = FALSE)
#' sampen(x_vals,
#'   dimensions = 3, tolerance = 1 / 2, standardize = FALSE,
#'   use_diff = TRUE
#' )
#' sampen(x_vals, dimensions = 3, tolerance = 1, standardize = FALSE)
#'
#' @export
#'

sampen <- function(timeseries_array,
                   dimensions = 2,
                   tolerance = 0.2,
                   standardize = TRUE,
                   use_diff = FALSE) {
  # input conversion for shorter variable names
  y <- timeseries_array
  m <- dimensions
  r <- tolerance

  if (use_diff) {
    if (length(y) <= 2) {
      warning("Insufficient length of timeseries_array!")
      return(NA)
    }
    y <- y[2:length(y)] - y[1:(length(y) - 1)]
  }

  # possibly: standardization
  if (standardize) {
    y <- y - mean(y)
    y <- y / stats::sd(y)
  }

  n <- length(y) - m
  mat_m <- 0 # counter for matches of length m
  mat_m1 <- 0 # counter for matches of length m+1

  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      # for each possible pair of starting points of a vector of length m+1
      for (k in 0:m) {
        # test whether the vector of length m and the vector of length m+1
        # are within the tolerance
        if (abs(y[i + k] - y[j + k]) > r) {
          break
        }
        if ((k + 1) == m) {
          mat_m <- mat_m + 1
        }
        if ((k + 1) > m) {
          mat_m1 <- mat_m1 + 1
        }
      }
    }
  }

  if (mat_m == 0) {
    return(NA)
  }
  return(-log(mat_m1 / mat_m))
}
