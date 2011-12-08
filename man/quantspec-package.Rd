\name{quantspec-package}
\alias{quantspec-package}
\docType{package}
\title{
    Quantile-based Spectral Analysis of Time Series
}
\description{Methods to determine, smooth and plot Laplace periodograms	and rank-based Laplace periodograms for univariate time series.
}
\details{
\tabular{ll}{
Package: \tab quantspec\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2011-12-08\cr
License: \tab GPL (>= 2)\cr
}
}
\author{
Tobias Kley

Maintainer: Tobias Kley <tobias.kley@ruhr-uni-bochum.de>
}
\references{
Dette, H., Hallin, M., Kley, T. and Volgushev, S. (2011). Of copulas, quantiles, ranks and spectra: An $L_1$-approach to spectral analysis. arxiv.org/abs/1111.7205.
}
\examples{
# Define the quantile orders.
taus <- c(0.05,0.25,0.5,0.75,0.95)

# Determine the rank-based Laplace periodogram kernel
# for the quantile orders taus.
LPG <- LaplacePeriodogram(sunspot.year, taus, showProgressBar=TRUE)

# Determine the smoothed, rank-based Laplace periodogram kernel
# using weights from a Daniell kernel.
smoothedLPG <- smoothedLaplacePeriodogram(LPG,taus,kernel("daniell",c(3,2)))

# Plot the result for the values 0.25, 0.5 and 0.75 of tau
# as a 3x3-matrix plot.
plotLaplacePeriodogram(LPG=smoothedLPG, CL=c(2,3,4), hRange=TRUE, taus=taus)

# Plot the full (5x5) matrix to a pdf-file.
pdf("sunspot_rbLPG.pdf",width=5*4, height=5*3)
   plotLaplacePeriodogram(LPG=smoothedLPG, hRange=TRUE, taus=taus,
                 oma=c(5,5,5,5), mar=c(5,5,1,0)+0.1)
dev.off()
}
