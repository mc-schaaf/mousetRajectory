# -----------------------------------------------------------------------------
# internal function helping with the handling of parameters

#' checks whether something is numeric vector or numeric atomic value
#' returns `FALSE` when `NA` values are present
#' @noRd
is_n_v <- function(in1) {
  if (is.null(in1) ||
    length(in1) < 1 ||
    !methods::is(in1, "numeric") ||
    any(is.na(in1))) {
    return(FALSE)
  }
  return(TRUE)
}

#' checks whether two vectors are of equal length and numeric
#' returns `FALSE` when `NA` values are present
#' @noRd
is_xy_v <- function(in1, in2) {
  if (!is_n_v(in1) || !is_n_v(in1) || length(in1) != length(in2)) {
    return(FALSE)
  }
  return(TRUE)
}

#' checks whether something is a atomic numeric value or
#' vector with all equal values
#' returns `FALSE` when `NA` values are present
#' @noRd
is_n_a <- function(in1) {
  if (is.null(in1) ||
    length(unique(in1)) != 1 ||
    !methods::is(in1, "numeric") ||
    any(is.na(in1))) {
    return(FALSE)
  }
  return(TRUE)
}

#' checks whether something is a atomic logical value or
#' vector with all equal values
#' returns `FALSE` when `NA` values are present
#' @noRd
is_l_a <- function(in1) {
  if (is.null(in1) ||
    length(unique(in1)) != 1 ||
    !methods::is(in1, "logical") ||
    any(is.na(in1))) {
    return(FALSE)
  }
  return(TRUE)
}
