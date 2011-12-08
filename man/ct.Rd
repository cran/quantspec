\name{ct}
\alias{ct}
\title{Determine column index for a combination (tau1, tau2)}
\description{
    Determines the column to access in the Laplace periodogram (represented as a matrix) that corresponds to a combination (tau1, tau2).
}
\usage{
ct(i1, i2, n)
}
\arguments{
  \item{i1}{Index of tau1.}
  \item{i2}{Index of tau2.}
  \item{n}{Length of vector \code{taus}.}
}
\details{
    For each value of omega the Laplace periodogram is a Hermitian matrix.
    The values in the matrix are stored only for the indices \code{i1 <= i2}.
    The objects returned by the functions \code{\link{LaplacePeriodogram}}, \code{\link{smoothedLaplacePeriodogram}} and
    read by the function \code{\link{plotLaplacePeriodogram}} are stored as a matrix with one column for each combination of (tau1, tau2).
}
\value{
    Index of the column to access.
}
\author{
    Tobias Kley
}
\seealso{
    \code{\link{LaplacePeriodogram}, \link{smoothedLaplacePeriodogram}} and \code{\link{plotLaplacePeriodogram}}.
}
\examples{
# Define the quantile orders.
taus <- c(0.05,0.25,0.5,0.75,0.95)

# Determine the rank-based Laplace periodogram kernel
# for the quantile orders taus.
LPG <- LaplacePeriodogram(sunspot.year, taus, showProgressBar=TRUE)

# Return one of the estimated values
paste("L =",LPG[30,ct(2,3,5)])
paste("where tau1 =",taus[2],"and tau2 =",taus[3])
paste("and omega =",rownames(LPG)[30])
}
