% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class-QuantileSD.R
\docType{class}
\name{QuantileSD-class}
\alias{QuantileSD-class}
\alias{QuantileSD}
\title{Class for a simulated quantile (i. e., Laplace or copula)
density kernel.}
\description{
\code{QuantileSD} is an S4 class that implements the necessary
calculations to determine a numeric approximation to the quantile spectral
density kernel of a model from which a time series of length \code{N} can be
sampled via a function call \code{ts(N)}.
}
\details{
In the simulation a number of \code{R} independent quantile periodograms
based on the clipped time series are simulated. If \code{type=="copula"},
then the rank-based version is used. The sum and the sum of the squared
absolute value is stored to the slots \code{sumPG} and \code{sumSqPG}.
After the simulation is completed the mean and it's standard error (of the
simulated quantile periodograms) are determined and stored to \code{meanPG}
and \code{stdError}. Finally, the (copula) spectral density kernel is
determined by smoothing real and imaginary part of \code{meanPG} seperately
for each combination of levels using \code{\link[stats]{smooth.spline}}.

Note that, all remarks made in the documentation of the super-class
\code{\link{QSpecQuantity}} apply.
}
\section{Slots}{

\describe{
\item{\code{N}}{a \code{numeric} specifying the number of equaly spaced
Fourier frequencies from
\eqn{[0,2\pi)}{[0,2pi)} for which the (copula) spectral density
will be simulated; note that due to the simulation mechanism a
larger number will also yield a better approximation.}

\item{\code{R}}{the number of independent repetitions performed; note that due to
the simulation mechanism a larger number will also yield a better
approximation; can be enlarged
using \code{\link{increasePrecision-QuantileSD}}.}

\item{\code{type}}{can be either \code{Laplace} or \code{copula}; indicates whether
the marginals are to be assumed uniform \eqn{[0,1]} distributed.}

\item{\code{ts}}{a \code{function} that allows to draw independent samples
\eqn{Y_0, \ldots, Y_{n-1}} from the process for which the (copula)
spectral density kernel is to be simulated}

\item{\code{seed.last}}{used internally to store the state of the pseudo random number
generator, so the precision can be increased by generating
more pseudo random numbers that are independent from the ones
previously used.}

\item{\code{sumPG}}{an \code{array} used to store the sum of the simulated quantile
periodograms}

\item{\code{sumSqPG}}{an \code{array} used to store the sum of the squared absolute
values of the simulated quantile periodograms}

\item{\code{meanPG}}{an \code{array} used to store the mean of the simulated quantile
periodograms}

\item{\code{stdError}}{an \code{array} used to store the estimated standard error of the mean
of the simulated quantile periodograms}
}}

\examples{
## This script can be used to create and store a QuantileSD object

\dontrun{
## Parameters for the simulation:
R <- 50                      # number of independent repetitions;
                             # R should be much larger than this in practice!
N <- 2^8                     # number of Fourier frequencies in [0,2pi)
ts <- ts1                    # time series model
levels <- seq(0.1,0.9,0.1)   # quantile levels
type <- "copula"             # copula, not Laplace, spectral density kernel
seed.init <- 2581            # seed for the pseudo random numbers

## Simulation takes place once the constructor is invoked
qsd <- quantileSD(N=N, seed.init = 2581, type = type,
    ts = ts, levels.1=levels, R = R)

## The simulated copula spectral density kernel can be called via
V1 <- getValues(qsd)

## It is also possible to fetch the result for only a few levels
levels.few <- c(0.2,0.5,0.7)
V2 <- getValues(qsd, levels.1=levels.few, levels.2=levels.few)

## If desired additional repetitions can be performed to yield a more precise
## simulation result by calling; here the number of independent runs is doubled.
qsd <- increasePrecision(qsd,R)

## Often the result will be stored for later usage.  
save(qsd, file="QAR1.rdata")

## Take a brief look at the result of the simulation
plot(qsd, levels=levels.few)

## When plotting more than only few levels it may be a good idea to plot to
## another device; e. g., a pdf-file
K <- length(levels)
pdf("QAR1.pdf", width=2*K, height=2*K)
  plot(qsd)
dev.off()

## Now we analyse the multivariate process (eps_t, eps_{t-1}) from the
## introduction of Barunik&Kley (2015). It can be defined as
ts_mult <- function(n) {
  eps <- rnorm(n+1)
  return(matrix(c(eps[2:(n+1)], eps[1:n]), ncol=2))
}

## now we determine the quantile cross-spectral densities
qsd <- quantileSD(N=N, seed.init = 2581, type = type,
    ts = ts_mult, levels.1=levels, R = R)

## from which we can for example extract the quantile coherency
Coh <- getCoherency(qsd, freq = 2*pi*(0:64)/128)

## We now plot the real part of the quantile coherency for j1 = 1, j2 = 2,
## tau1 = 0.3 and tau2 = 0.6
plot(x = 2*pi*(0:64)/128, Re(Coh[,1,3,2,6]), type="l")
}
}
\references{
Dette, H., Hallin, M., Kley, T. & Volgushev, S. (2015).
Of Copulas, Quantiles, Ranks and Spectra: an \eqn{L_1}{L1}-approach to
spectral analysis. \emph{Bernoulli}, \bold{21}(2), 781--831.
[cf. \url{http://arxiv.org/abs/1111.7205}]

Kley, T., Volgushev, S., Dette, H. & Hallin, M. (2016).
Quantile Spectral Processes: Asymptotic Analysis and Inference.
\emph{Bernoulli}, \bold{22}(3), 1770--1807.
[cf. \url{http://arxiv.org/abs/1401.8104}]

Barunik, J. & Kley, T. (2015).
Quantile Cross-Spectral Measures of Dependence between Economic Variables.
[preprint available from the authors]
}
\seealso{
Examples for implementations of functions \code{ts} can be found at:
\code{\link{ts-models}}.
}
\keyword{S4-classes}
