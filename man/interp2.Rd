% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interp2.R
\name{interp2}
\alias{interp2}
\title{Interpolation aka Time-Normalization}
\usage{
interp2(time_old, xy_old, n_xy_new = 101)
}
\arguments{
\item{time_old}{Timestamps of the \code{xy_old} coordinates.}

\item{xy_old}{To-be normalized x or y coordinates.}

\item{n_xy_new}{Number of equidistant timepoints that should be generated.
Defaults to 101.}
}
\value{
Vector of length \code{n_xy_new} with interpolated x or y values.
}
\description{
Convenient wrapper to \code{\link[signal:interp1]{signal::interp1()}} for linear
interpolation. Assumes that you want interpolated values of
\code{xy_old} at \code{n_xy_new} equidistant data points.
}
\examples{
plot(interp2(0:10, (0:10)^2))

}
\references{
Pfister, R., Tonn, S., Schaaf, M., Wirth, R. (2024).
mousetRajectory: Mouse tracking analyses for behavioral scientists.
The Quantitative Methods for Psychology, 20(3), 217-229.
\doi{10.20982/tqmp.20.3.p217}
}
