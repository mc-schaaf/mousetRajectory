#' @export
time_circle_entered <- function(x_vector, y_vector, t_vector, xMid, yMid, radius, include_radius=TRUE){
  dSquare <- (x_vector-xMid)^2 + (y_vector-yMid)^2
  if(include_radius){
    isIn <- dSquare <= radius^2
  } else {
    isIn <- dSquare < radius^2
  }
  if(all(!isIn)){
    return(NA)
  }
  index <- which.max(isIn)
  if (index == 1) {warning("The first point was already in the circle!")}
  return(t_vector[index])
}

#' @export
time_circle_left <- function(x_vector, y_vector, t_vector, xMid, yMid, radius, include_radius=FALSE){
  dSquare <- (x_vector-xMid)^2 + (y_vector-yMid)^2
  if(include_radius){
    isOut <- dSquare >= radius^2
  } else {
    isOut <- dSquare > radius^2
  }
  if(isOut[1]){
    warning("The first point was not in the circle! Returning the first time point.")
  }
  if(all(!isOut)){
    return(NA)
  }
  index <- which.max(isOut)
  return(t_vector[index])
}

#' @export
interp2 <- function(time_old, xy_old, n_xy_new = 101){
  time_old <- time_old - min(time_old)
  time_old <- time_old / max(time_old)
  time_old <- time_old * n_xy_new
  return(interp1(time_old, xy_old, seq(0, n_xy_new-1), method="linear"))
}
