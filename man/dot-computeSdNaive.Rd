% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{.computeSdNaive}
\alias{.computeSdNaive}
\title{Workhorse function for \code{\link{getSdNaive-SmoothedPG}}.}
\arguments{
\item{V}{a 3-dimensional array of complex numbers; dimensions are
\code{[N, K1, K2]}, where \code{N} frequencies are
\eqn{\omega_j := 2\pi j/N} for \eqn{j=0,\ldots,N}.}

\item{W}{a vector of length \code{W} of length \code{N} used for smoothing.}
}
\value{
Returns an array with complex numbers
        \eqn{\sigma(\tau_1, \tau_2, \omega_j} as defined in
        Kley et. al (2016), p. 26.
}
\description{
C++ implementation to increase performance.
}
\references{
Dette, H., Hallin, M., Kley, T. & Volgushev, S. (2015).
Of Copulas, Quantiles, Ranks and Spectra: an \eqn{L_1}{L1}-approach to
spectral analysis. \emph{Bernoulli}, \bold{21}(2), 781--831.
[cf. \url{http://arxiv.org/abs/1111.7205}]
}
\keyword{internals}
