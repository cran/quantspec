################################################################################
# Analysis of yearly sunspot data
################################################################################

# Define the quantile orders.
taus <- c(0.05,0.25,0.5,0.75,0.95)

# Determine the rank-based Laplace periodogram kernel
# for the quantile orders taus.
LPG <- LaplacePeriodogram(sunspot.year, taus, showProgressBar=TRUE)

# Then, plot on screen ...
plotLaplacePeriodogram(LPG=LPG, CL=c(2,3,4), hRange=TRUE, taus=taus)
plotLaplacePeriodogram(LPG=LPG, CL=c(2,3,4), hRange=TRUE, taus=taus)

# Determine the smoothed, rank-based Laplace periodogram kernel
# using weights from a Daniell kernel.
smoothedLPG <- smoothedLaplacePeriodogram(LPG,taus,kernel("daniell",c(3,2)))

# Plot (for the values 0.25, 0.5 and 0.75,
# as a 3x3-matrix plot) on screen ...
plotLaplacePeriodogram(LPG=smoothedLPG, CL=c(1,3,5), hRange=TRUE, taus=taus)

################################################################################
# Analysis of S&P 500 returns
################################################################################

require(FinTS)
data(d.sp9003lev)

# Determine (approximate) returns
sp500 <- diff(log(d.sp9003lev), lag = 1, differences = 1)

# Define quantile orders and determine rank-based Laplace periodograms.
taus <- c(0.05,0.5,0.95)
LPG <- LaplacePeriodogram(sp500, taus, showProgressBar=TRUE)

# Then, plot on screen ...
plotLaplacePeriodogram(LPG=LPG, hRange=TRUE, taus=taus)

# Determine the smoothed, rank-based Laplace periodogram kernel
# using weights from a Daniell kernel.
smoothedLPG <- smoothedLaplacePeriodogram(LPG,taus,kernel("daniell",c(30,25)))

# Plot on screen ...
plotLaplacePeriodogram(LPG=smoothedLPG, hRange=TRUE, hOffset=TRUE, taus=taus)