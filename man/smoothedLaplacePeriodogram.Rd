\name{smoothedLaplacePeriodogram}
\alias{smoothedLaplacePeriodogram}
\title{Perform smoothing of a Laplace or rank-based Laplace periodogram}
\description{
    Calculates the smoothed Laplace or rank-based Laplace periodogram kernel
    from the unsmoothed version.
}
\usage{
smoothedLaplacePeriodogram(LPG, taus, W)
}
\arguments{
  \item{LPG}{
    A (rank-based) Laplace periodogram as returned by the \code{\link{LaplacePeriodogram}}-function.
    Needs to be calculated at all frequencies,
    i.e., the default setting \code{1:(ceiling(length(X)/2)-1)}.
}
  \item{taus}{
    Vector of values between 0 and 1. The vector that was used for the
    calculation of the (rank-based) Laplace periodogram has to be used.
}
  \item{W}{
    A vector of an odd number of positive weights or a \code{tskernel}-object.
    If a \code{tskernel}-object is specified the weights from its \code{$coef}
    are used.
}
}
\details{
    Details on the calculation can be found in Dette et. al (2011).
}
\value{
    A matrix in the same format as from \code{\link{LaplacePeriodogram}}, but with the smoothed values.
}
\references{
    Dette, H., Hallin, M., Kley, T. and Volgushev, S. (2011). Of copulas, quantiles, ranks and spectra: An $L_1$-approach to spectral analysis. arxiv.org/abs/1111.7205.
}
\author{
    Tobias Kley
}
\seealso{
    \code{\link{LaplacePeriodogram}} and \code{\link{plotLaplacePeriodogram}}.
}
\examples{
# See the example in quantspec-package.
}
