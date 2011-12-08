\name{plotLaplacePeriodogram}
\alias{plotLaplacePeriodogram}
\title{
Plot Laplace or rank-based Laplace periodogram kernel
}
\description{
    Plots a Laplace or rank-based Laplace periodogram kernel
}
\usage{
plotLaplacePeriodogram(LPG, taus, F = 1:length(LPG[, 1]), CL = 1:length(taus),
    hRange = FALSE, hOffset = FALSE,
    ylabel = expression({{hat(f)}[n]^{list(tau[1], tau[2])}}(omega)),
    oma = c(2.5, 2.5, 2.5, 2.5), mar = c(4.5, 4.5, 1, 0) + 0.1, cex.lab = 1.5)
}
\arguments{
  \item{LPG}{
    A Laplace Periodogram at all Fourier-frequencies, as returned by the
    \code{\link{LaplacePeriodogram}}-function or the \code{\link{smoothedLaplacePeriodogram}}-function.
}
  \item{taus}{
    Vector of quantile orders that was used to calculate \code{LPG}.
}
  \item{F}{
    Subset of frequencies to plot. To be specified as a subset of \code{omegas} in the \code{\link{LaplacePeriodogram}}-function.
}
  \item{CL}{
    Subset of taus to plot. To be specified as a vector of indices.
}
  \item{hRange}{
    If true, the scale of all plots will be homogeneous.
}
  \item{hOffset}{
    If true, the scale of all plots will have one common point (e.g. the origin).
}
  \item{ylabel}{
    Label for the ordinate of the plots.
}
  \item{oma}{
    Parameter that will be forwarded to \code{par}.
    The default value is suited for 3x3 on screen plots.
    Try \code{c(5,5,5,5)} for usage with \code{pdf} device.
}
  \item{mar}{
    Parameter that will be forwarded to \code{par}.
    The default value is suited for 3x3 on screen plots.
    Try \code{c(5,5,1,0)+0.1} for usage with \code{pdf} device.
}
  \item{cex.lab}{
    Parameter that will be forwarded to \code{par}.
    Determines the size of the outside labels (\code{taus}, \code{omegas}, and \code{ylabel}).
}
}
\value{
    A plot best to be returned to a pdf device.
}
\references{
Dette, H., Hallin, M., Kley, T. and Volgushev, S. (2011). Of copulas, quantiles, ranks and spectra: An $L_1$-approach to spectral analysis. arxiv.org/abs/1111.7205.
}
\author{
    Tobias Kley
}
\seealso{
    \code{\link{LaplacePeriodogram}} and \code{\link{smoothedLaplacePeriodogram}}.
}
\examples{
# See the example in quantspec-package.
}
