# -----------------------------------------------------------------------------
# specify behaviour when this package is loaded

# -----------------------------------------------------------------------------
# internal function helping with the handling of parameters
check_input_length_is_1 <- function(in1){
  if (is.null(in1) || length(in1) > 1){
    return(FALSE)
  }
  return(TRUE)
}

