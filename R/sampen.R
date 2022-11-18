#' @title SAMPle ENtropy
#'
#' @description Computes the sample Entropy, as given by
#' Richman & Moorman (2000) <\doi{10.1152/ajpheart.2000.278.6.H2039}>.
#'
#' @param timeseries_array Array of numbers over which the sampEn is to be
#' computed.
#' @param dimensions Number of embedding dimensions for which to compute the
#' sampEn. Sometimes also called "template length".
#' @param tolerance Tolerance for the comparisons of two number sequences.
#' @param standardize Whether to standardize the timeseries_array.
#' @param use_diff Whether to use the differences between adjacent points.
#'
#' @return Single number indicating the sample entropy for the given parameters.
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @examples
#' data("dat_one_trajectory")
#' sampen(dat_one_trajectory$xvals)
#'
#' @export
#' @importFrom stats "sd"
#'

sampen <- function(timeseries_array,
                   dimensions = 2,
                   tolerance = 0.2,
                   standardize = TRUE,
                   use_diff = FALSE) {
  # input conversion to mirror variable names of RP
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
    y <- y / sd(y)
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

  return(-log(mat_m1 / mat_m))
}
