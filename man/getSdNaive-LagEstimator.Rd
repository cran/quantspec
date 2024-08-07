% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class-LagEstimator.R
\name{getSdNaive-LagEstimator}
\alias{getSdNaive-LagEstimator}
\alias{getSdNaive,LagEstimator-method}
\title{Get estimates for the standard deviation of the lagEstimator derived from
the asymptotics (see Birr et al (2015))}
\usage{
\S4method{getSdNaive}{LagEstimator}(
  object,
  frequencies = 2 * pi * (0:(length(object@Y) - 1))/length(object@Y),
  levels.1 = getLevels(object, 1),
  levels.2 = getLevels(object, 2)
)
}
\arguments{
\item{object}{\code{\link{LagEstimator}} of which to get the estimates for the
standard deviation.}

\item{frequencies}{a vector of frequencies for which to get the result}

\item{levels.1}{the first vector of levels for which to get the result}

\item{levels.2}{the second vector of levels for which to get the result}
}
\value{
Returns the estimate described above.
}
\description{
Determines and returns an array of dimension \code{[J,K1,K2]},
where \code{J=length(frequencies)}, \code{K1=length(levels.1)}, and
\code{K2=length(levels.2))}.
At position \code{(j,k1,k2)} the returned value is the standard deviation 
estimated corresponding to \code{frequencies[j]}, \code{levels.1[k1]} and 
\code{levels.2[k2]} that are closest to the \code{frequencies}, \code{levels.1}
and \code{levels.2} available in \code{object}; \code{\link{closest.pos}} is 
used to determine what closest to means.
}
\details{
Requires that the \code{\link{LagEstimator}} is available at all Fourier
frequencies from \eqn{(0,\pi]}{(0,pi]}. If this is not the case the missing
values are imputed by taking one that is available and has a frequency
that is closest to the missing Fourier frequency; \code{closest.pos} is used
to determine which one this is.

Note the ``standard deviation'' estimated here is not the square root of the
complex-valued variance. It's real part is the square root of the variance
of the real part of the estimator and the imaginary part is the square root
of the imaginary part of the variance of the estimator.
}
\keyword{Access-functions}
