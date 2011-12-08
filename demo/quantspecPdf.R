################################################################################
# Analysis of yearly sunspot data
################################################################################

# Define the quantile orders.
taus <- c(0.05,0.25,0.5,0.75,0.95)

# Determine the rank-based Laplace periodogram kernel
# for the quantile orders taus.
LPG <- LaplacePeriodogram(sunspot.year, taus, showProgressBar=TRUE)

# Plot (the full (5x5) matrix) to a pdf-file.
pdf("sunspot_rbLPG.pdf",width=5*4, height=5*3)
   plotLaplacePeriodogram(LPG=LPG, hRange=TRUE, taus=taus,
                 oma=c(5,5,5,5), mar=c(5,5,1,0)+0.1)
dev.off()

# Determine the smoothed, rank-based Laplace periodogram kernel
# using weights from a Daniell kernel.
smoothedLPG <- smoothedLaplacePeriodogram(LPG,taus,kernel("daniell",c(3,2)))

# Plot (the full (5x5) matrix) to a pdf-file.
pdf("sunspot_rbLPG_smoothed.pdf",width=5*4, height=5*3)
   plotLaplacePeriodogram(LPG=smoothedLPG, hRange=TRUE, taus=taus,
                 oma=c(5,5,5,5), mar=c(5,5,1,0)+0.1)
dev.off()

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

# Plot to a pdf-file.
pdf("sp500_rbLPG.pdf",width=5*4, height=5*3)
  plotLaplacePeriodogram(LPG=LPG, hRange=TRUE, taus=taus,
                oma=c(5,5,5,5), mar=c(5,5,1,0)+0.1)
dev.off()

# Determine the smoothed, rank-based Laplace periodogram kernel
# using weights from a Daniell kernel.
smoothedLPG <- smoothedLaplacePeriodogram(LPG,taus,kernel("daniell",c(30,25)))

# Plot to a pdf-file.
pdf("sp500_rbLPG_smoothed.pdf",width=5*4, height=5*3)
   plotLaplacePeriodogram(LPG=smoothedLPG, hRange=TRUE, hOffset=TRUE, taus=taus,
                 oma=c(5,5,5,5), mar=c(5,5,1,0)+0.1)
dev.off()
