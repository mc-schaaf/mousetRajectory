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
#' \doi{10.1152/ajpheart.2000.278.6.H2039}, the last vector of length
#' `dimensions` is not considered because it has no corresponding vector of
#' length `dimensions + 1`, ensuring a sampen estimation with a low bias
#' introduced by the length of the `timeseries_array`.
#' The function was deliberately implemented in R with C-style code. While this
#' makes the function rather slow for large `timeseries_array`s,
#' it enables maximal transparency. For an overview over faster sampen
#' functions in R that, however, are distributed in binary or need source
#' compilation, see Chen et al. (2019), \doi{10.1093/biomethods/bpz016}.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#' x_vals <- rep(c(0,0,0,0,0,1), 20)
#' sampen(x_vals, dimensions = 1, tolerance = 1/2, standardize = FALSE)
#' sampen(x_vals, dimensions = 3, tolerance = 1/2, standardize = FALSE)
#' sampen(x_vals, dimensions = 3, tolerance = 1/2, standardize = FALSE, use_diff = TRUE)
#' sampen(x_vals, dimensions = 3, tolerance = 1, standardize = FALSE)
#'
#' @export
#'

sampen <- function(timeseries_array,
                   dimensions = 2,
                   tolerance = 0.2,
                   standardize = TRUE,
                   use_diff = FALSE) {
  # check inputs
  # stopifnot(
  #   is_n_v(timeseries_array),
  #   is_n_a(dimensions),
  #   is_n_a(tolerance),
  #   is_l_a(standardize),
  #   is_l_a(use_diff)
  # )

  # input conversion for shorter variable names
  y <- timeseries_array
  M <- dimensions
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

  N <- length(y) - M
  mat_m <- 0              # counter for matches of length M
  mat_m1 <- 0             # counter for matches of length M+1

  for (i in 1:(N - 1)) {
    for (j in (i + 1):N) {
      # for each possible pair of starting points of a vector of length M+1
      for (k in 0:(M)) {
        # test whether the vector of length M and the vector of length M+1
        # are within the tolerance
        if (abs(y[i + k] - y[j + k]) > r) {
          break
        }
        if ((k + 1) == M) {
          mat_m <- mat_m + 1
        }
        if ((k + 1) > M) {
          mat_m1 <- mat_m1 + 1
        }
      }
    }
  }

  if(mat_m == 0){return(NA)}
  return(-log(mat_m1 / mat_m))
}
