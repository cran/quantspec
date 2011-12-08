\name{LaplacePeriodogram}
\alias{LaplacePeriodogram}
\title{Determine Laplace or rank-based Laplace periodogram}
\description{
    Calculates the Laplace or rank-based Laplace periodogram kernel from a
    given time series \code{X} at quantile orders \code{taus} and frequencies \code{omegas}.
}
\usage{
LaplacePeriodogram(X, taus, omegas = 1:(ceiling(length(X)/2) - 1),
    fromRanks = TRUE, showProgressBar = FALSE)
}
\arguments{
  \item{X}{
    Time series to calculate the (rank-based) Laplace periodogram from.
    Needs to be either a vector, a \code{ts}-object or a \code{zoo}-object.
}
  \item{taus}{
    Vector of values between 0 and 1. The (rank-based) Laplace periodogram will
    be calculated at all pairs \code{tau1}, \code{tau2} from the vector of quantile orders \code{taus}.
}
  \item{omegas}{
    A vector specifying the frequencies the (rank-based) Laplace peridogram will be calculated at.
    2 pi / n times the values from \code{omegas} will be used as frequencies, where n denotes the
    length of the time series \code{X}.
    
    DEFAULT: all Fourier frequencies from (0,pi).
}
  \item{fromRanks}{
    If true the rank-based Laplace periodogram will be determined.
    Otherwise the Laplace periodogram will be determined.
    
    DEFAULT: the rank-based Laplace periodogram.
}
  \item{showProgressBar}{
    If true, a progress bar will be shown during the calculation.
    
    DEFAULT: no progress bar.
}
}
\details{
    Details on the calculation can be found in Dette et. al (2011).
}
\value{
    A matrix with the values of the Laplace or rank-based Laplace periodogram determined.
    It has one row for each frequency and one column for each pair (tau1, tau2) with
    tau1 < tau2 and tau1 and tau2 both from the vector \code{taus}. Use \code{\link{ct}}
    to access the column that corresponds to (tau1, tau2).
}
\references{
Dette, H., Hallin, M., Kley, T. and Volgushev, S. (2011). Of copulas, quantiles, ranks and spectra: An $L_1$-approach to spectral analysis. arxiv.org/abs/1111.7205.
}
\author{
    Tobias Kley
}
\seealso{
    \code{\link{smoothedLaplacePeriodogram}} and \code{\link{plotLaplacePeriodogram}}.
}
\examples{
# See the example in quantspec-package.
}
