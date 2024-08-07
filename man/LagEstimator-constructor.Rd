% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class-LagEstimator.R
\name{LagEstimator-constructor}
\alias{LagEstimator-constructor}
\alias{lagEstimator}
\title{Create an instance of the \code{LagEstimator} class.}
\usage{
lagEstimator(
  Y,
  frequencies = 2 * pi/length(Y) * 0:(length(Y) - 1),
  levels.1 = 0.5,
  levels.2 = levels.1,
  weight = lagKernelWeight(K = length(Y), bw = 100),
  type = c("clippedCov")
)
}
\arguments{
\item{Y}{a time series (\code{numeric}, \code{ts}, or \code{zoo} object) or a 
\code{\link{LagOperator}} from which to determine the \code{LagEstimator}}

\item{frequencies}{A vector containing (Fourier-)frequencies at which to determine the
smoothed periodogram.}

\item{levels.1}{the first vector of levels for which to compute the LagEstimator}

\item{levels.2}{the second vector of levels for which to compute the LagEstimator}

\item{weight}{Object of type \code{\link{Weight}} to be used for smoothing.}

\item{type}{if \code{Y} is a time series, this indicates which LagOperator will be used}
}
\value{
Returns an instance of \code{LagEstimator}.
}
\description{
A \code{LagEstimator} object can be created from \code{numeric}, a \code{ts},
or a \code{zoo} object. Also a \code{\link{LagOperator}} and a 
\code{\link{Weight}} object can be used to create different types of 
estimators.
}
\examples{
Y <- rnorm(100)
levels.1 <- c(0.1,0.5,0.9)
weight <- lagKernelWeight(W = WParzen,  bw = 10, K = length(Y))
lagOp <- clippedCov(Y,levels.1 = levels.1)
lagEst <- lagEstimator(lagOp, weight = weight)
}
\keyword{Constructors}
