% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{ts-models-AR1}
\alias{ts-models-AR1}
\alias{AR1}
\title{Simulation of an AR(1) time series.}
\usage{
AR1(n, a, overhead = 500, innov = rnorm)
}
\arguments{
\item{n}{length of the time series to be returned}

\item{a}{parameter of the model}

\item{overhead}{an integer specifying the ``warmup'' period to reach an
approximate stationary start for the times series}

\item{innov}{a function that generates a random number each time
\code{innov(1)} is called; used to specify the distribution of
the innovations; \code{rnorm} by default}
}
\value{
Returns an AR(1) time series with specified parameters.
}
\description{
Returns a simulated time series \eqn{(Y_t)} that fulfills the following equation:
\deqn{Y_t = a Y_{t-1} + \epsilon_t,}
where \eqn{a} is a parameter and \eqn{\epsilon_t} is independent white
noise with marginal distribution specified by the parameter \code{innov}.
}
\examples{
plot(AR1(100, a=-0.7), type="l")
}
