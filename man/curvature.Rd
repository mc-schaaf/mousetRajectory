% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/curvature.R
\name{curvature}
\alias{curvature}
\title{Curvature}
\usage{
curvature(x_vector, y_vector)
}
\arguments{
\item{x_vector}{x-coordinates of the executed path.}

\item{y_vector}{y-coordinates of the executed path.}
}
\value{
Single number indicating the curvature (1 to +Inf).
}
\description{
Computes the curvature of a path, defined by vectors
of x and y coordinates, as compared to an ideal path,
as defined by the start and end points of the path.
}
\details{
The supplied vectors are assumed to be ordered by time.
}
\examples{
x_vals <- c(0, 0, 0, 1, 2)
y_vals <- c(0, 1, 2, 2, 2)
plot(x_vals, y_vals, type = "l")
lines(c(0, 2), c(0, 2), lty = "dashed", lwd = 2) # ideal
curvature(x_vals, y_vals)

x_vals <- c(0, 1, 2, 2, 2)
y_vals <- c(0, 0, 0, 1, 2)
plot(x_vals, y_vals, type = "l")
lines(c(0, 2), c(0, 2), lty = "dashed", lwd = 2) # ideal
curvature(x_vals, y_vals)

x_vals <- c(0, 0, 1, 2, 2)
y_vals <- c(0, 1, 1, 1, 2)
plot(x_vals, y_vals, type = "l")
lines(c(0, 2), c(0, 2), lty = "dashed", lwd = 2) # ideal
curvature(x_vals, y_vals)

}
\references{
Pfister, R., Tonn, S., Schaaf, M., Wirth, R. (2024).
mousetRajectory: Mouse tracking analyses for behavioral scientists.
The Quantitative Methods for Psychology, 20(3), 217-229.
\doi{10.20982/tqmp.20.3.p217}
}
