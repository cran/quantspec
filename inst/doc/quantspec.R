### R code from vignette source 'quantspec.Rnw'

###################################################
### code chunk number 1: quantspec.Rnw:423-424
###################################################
library(quantspec)


###################################################
### code chunk number 2: quantspec.Rnw:430-431 (eval = FALSE)
###################################################
## help(quantspec)       # alternatively: package?quantspec


###################################################
### code chunk number 3: quantspec.Rnw:439-441 (eval = FALSE)
###################################################
## help(qRegEstimator)
## help(QRegEstimator)   # alternatively: class?QRegEstimator


###################################################
### code chunk number 4: quantspec.Rnw:450-452
###################################################
Y  <- rnorm(8)
bn <- qRegEstimator(Y, levels = c(0.25,0.5,0.75))


###################################################
### code chunk number 5: quantspec.Rnw:459-460
###################################################
bn


###################################################
### code chunk number 6: quantspec.Rnw:472-474
###################################################
getFrequencies(bn)
getParallel(bn)


###################################################
### code chunk number 7: quantspec.Rnw:483-484
###################################################
getValues(bn, levels = c(0.25,0.5))


###################################################
### code chunk number 8: quantspec.Rnw:492-493 (eval = FALSE)
###################################################
## help("getValues-FreqRep")


###################################################
### code chunk number 9: plotbn (eval = FALSE)
###################################################
## dn <- clippedFT(rnorm(32), levels = seq(0.05,0.95,0.05))
## plot(dn, frequencies = 2*pi*(0:64)/32, levels = c(0.25,0.5))


###################################################
### code chunk number 10: quantspec.Rnw:513-514
###################################################
dn <- clippedFT(rnorm(32), levels = seq(0.05,0.95,0.05))
plot(dn, frequencies = 2*pi*(0:64)/32, levels = c(0.25,0.5))


###################################################
### code chunk number 11: quantspec.Rnw:550-553 (eval = FALSE)
###################################################
## demo(sp500)
## demo(wheatprices)
## demo("qar-simulation")


###################################################
### code chunk number 12: plotSP (eval = FALSE)
###################################################
## library(zoo)
## plot(sp500,            xlab = "time t", ylab = "", main = "")
## acf(coredata(sp500),   xlab = "lag k",  ylab = "", main = "")
## acf(coredata(sp500)^2, xlab = "lag k",  ylab = "", main = "")


###################################################
### code chunk number 13: quantspec.Rnw:602-607
###################################################
def.par <- par(no.readonly = TRUE) # save default, for resetting...
layout(matrix(c(1,1,2,3), nrow=1))
par(mar=c(5,2,2,1))
library(zoo)
plot(sp500,            xlab = "time t", ylab = "", main = "")
acf(coredata(sp500),   xlab = "lag k",  ylab = "", main = "")
acf(coredata(sp500)^2, xlab = "lag k",  ylab = "", main = "")
par(def.par)  #- reset to default


###################################################
### code chunk number 14: SPquantilePG (eval = FALSE)
###################################################
## CR <- quantilePG(sp500, levels.1 = c(0.05,0.5,0.95),
##     type = "clipped", type.boot = "mbb", B = 250, l = 32)
## freq <- getFrequencies(CR)
## plot(CR, levels = c(0.05,0.5,0.95),
##     frequencies = freq[freq > 0 & freq <= pi],
##     ylab = expression({I[list(n,R)]^{list(tau[1],tau[2])}}(omega)))


###################################################
### code chunk number 15: SPquantilePG2 (eval = FALSE)
###################################################
## plot(CR, levels = c(0.05,0.5,0.95),
##     frequencies = freq[freq > 0 & freq <= pi/5],
##     ylab = expression({I[list(n,R)]^{list(tau[1],tau[2])}}(omega)))


###################################################
### code chunk number 16: quantspec.Rnw:675-676
###################################################
CR <- quantilePG(sp500, levels.1 = c(0.05,0.5,0.95),
    type = "clipped", type.boot = "mbb", B = 250, l = 32)
freq <- getFrequencies(CR)
plot(CR, levels = c(0.05,0.5,0.95),
    frequencies = freq[freq > 0 & freq <= pi],
    ylab = expression({I[list(n,R)]^{list(tau[1],tau[2])}}(omega)))


###################################################
### code chunk number 17: quantspec.Rnw:685-686
###################################################
plot(CR, levels = c(0.05,0.5,0.95),
    frequencies = freq[freq > 0 & freq <= pi/5],
    ylab = expression({I[list(n,R)]^{list(tau[1],tau[2])}}(omega)))


###################################################
### code chunk number 18: quantspec.Rnw:707-708 (eval = FALSE)
###################################################
## help(kernels)


###################################################
### code chunk number 19: SPsmoothedPG (eval = FALSE)
###################################################
## sPG <- smoothedPG(CR, weight=kernelWeight(W=W1, bw=0.07))
## plot(sPG, levels = c(0.05,0.5,0.95), type.scaling = "individual",
##     frequencies = freq[freq > 0 & freq <= pi], ptw.CIs = 0.1,
##     ylab = expression(hat(G)[list(n,R)](list(tau[1],tau[2],omega))))


###################################################
### code chunk number 20: SPsmoothedPGboot (eval = FALSE)
###################################################
## plot(sPG, levels = c(0.05,0.5,0.95), type.scaling = "real-imaginary",
##     ptw.CIs = 0.1, type.CIs = "boot.full",
##     frequencies = freq[freq > 0 & freq <= pi],
##     ylab = expression(hat(G)[list(n,R)](list(tau[1],tau[2],omega))))


###################################################
### code chunk number 21: quantspec.Rnw:745-746
###################################################
sPG <- smoothedPG(CR, weight=kernelWeight(W=W1, bw=0.07))
plot(sPG, levels = c(0.05,0.5,0.95), type.scaling = "individual",
    frequencies = freq[freq > 0 & freq <= pi], ptw.CIs = 0.1,
    ylab = expression(hat(G)[list(n,R)](list(tau[1],tau[2],omega))))


###################################################
### code chunk number 22: quantspec.Rnw:756-757
###################################################
plot(sPG, levels = c(0.05,0.5,0.95), type.scaling = "real-imaginary",
    ptw.CIs = 0.1, type.CIs = "boot.full",
    frequencies = freq[freq > 0 & freq <= pi],
    ylab = expression(hat(G)[list(n,R)](list(tau[1],tau[2],omega))))


###################################################
### code chunk number 23: quantspec.Rnw:774-775 (eval = FALSE)
###################################################
## help("plot-SmoothedPG")


###################################################
### code chunk number 24: quantspec.Rnw:809-810 (eval = FALSE)
###################################################
## help("ts-models")


###################################################
### code chunk number 25: csdQAR (eval = FALSE)
###################################################
## csd <- quantileSD(N=2^9, seed.init = 2581, type = "copula",
##         ts = ts1, levels.1 = c(0.25, 0.5, 0.75), R = 100, quiet=TRUE)
## plot(csd, ylab = expression(f[list(q[tau[1]],q[tau[2]])](omega)))


###################################################
### code chunk number 26: csdQARhighprec (eval = FALSE)
###################################################
## csd <- quantileSD(N=2^12, seed.init = 2581, type = "copula",
##     ts = ts1, levels.1 = c(0.25, 0.5, 0.75), R = 50000)
## save(csd, file="csd-qar1.rdata")


###################################################
### code chunk number 27: quantspec.Rnw:844-845 (eval = FALSE)
###################################################
## help("increasePrecision-QuantileSD")


###################################################
### code chunk number 28: csdQARhighprecPlot (eval = FALSE)
###################################################
## load("csd-qar1.rdata")
## plot(csd, frequencies = 2*pi*(1:2^8)/2^9,
##     ylab = expression(f[list(q[tau[1]],q[tau[2]])](omega)))


###################################################
### code chunk number 29: quantspec.Rnw:862-863
###################################################
csd <- quantileSD(N=2^9, seed.init = 2581, type = "copula",
        ts = ts1, levels.1 = c(0.25, 0.5, 0.75), R = 100, quiet=TRUE)
plot(csd, ylab = expression(f[list(q[tau[1]],q[tau[2]])](omega)))


###################################################
### code chunk number 30: quantspec.Rnw:891-896 (eval = FALSE)
###################################################
## sCR <- smoothedPG(ts1(512), levels.1 = c(0.25,0.5,0.75),
##     weight=kernelWeight(W=W1, bw=0.1))
## plot(sCR, qsd = csd,
##     ylab = bquote(paste(hat(G)[list(n,R)](list(tau[1],tau[2],omega)),
##     " and ", f[list(q[tau[1]],q[tau[2]])](omega))))


###################################################
### code chunk number 31: quantspec.Rnw:910-924 (eval = FALSE)
###################################################
## set.seed(2581)
## ts <- ts1
## N <- 128
## R <- 5000
## 
## freq <- 2*pi*(1:16)/32
## levels <- c(0.25, 0.5, 0.75)
## 
## J <- length(freq)
## K <- length(levels)
## 
## sims  <- array(0, dim=c(4,R,J,K,K))
## 
## weight <- kernelWeight(W=W1, bw=0.3)


###################################################
### code chunk number 32: quantspec.Rnw:943-949
###################################################
set.seed(020581)
sCR <- smoothedPG(ts1(512), levels.1 = c(0.25,0.5,0.75),
        weight=kernelWeight(W=W1, bw=0.1))
plot(sCR, qsd = csd,
    ylab = bquote(paste(hat(G)[list(n,R)](list(tau[1],tau[2],omega)),
    " and ", f[list(q[tau[1]],q[tau[2]])](omega))))


###################################################
### code chunk number 33: quantspec.Rnw:961-974 (eval = FALSE)
###################################################
## for (i in 1:R) {
##   Y <- ts(N)
## 
##   CR  <- quantilePG(Y, levels.1=levels, type="clipped")
##   LP  <- quantilePG(Y, levels.1=levels, type="qr")
##   sCR <- smoothedPG(CR, weight=weight)
##   sLP <- smoothedPG(LP, weight=weight)
## 
##   sims[1,i,,,] <- getValues(CR, frequencies=freq)[,,,1]
##   sims[2,i,,,] <- getValues(LP, frequencies=freq)[,,,1]
##   sims[3,i,,,] <- getValues(sCR, frequencies=freq)[,,,1]
##   sims[4,i,,,] <- getValues(sLP, frequencies=freq)[,,,1]
## }


###################################################
### code chunk number 34: quantspec.Rnw:986-990 (eval = FALSE)
###################################################
## trueV <- getValues(csd, frequencies=freq)
## SqDev <- array(apply(sims, c(1,2),
##         function(x) {abs(x-trueV)^2}), dim=c(J,K,K,4,R))
## rimse <- sqrt(apply(SqDev, c(2,3,4), mean))


