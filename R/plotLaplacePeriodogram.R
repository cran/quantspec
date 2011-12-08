plotLaplacePeriodogram <-
function(LPG, taus, F=1:length(LPG[,1]), CL=1:length(taus),
                          hRange=FALSE, hOffset=FALSE,
                          ylabel=expression({{hat(f)}[n]^{list(tau[1],tau[2])}}(omega)),
                          oma=c(2.5,2.5,2.5,2.5),
                          mar=c(4.5,4.5,1,0)+0.1,
                          cex.lab=1.5) {
  
  # Verify if all parameters are valid

  if (!(is.vector(taus) && is.numeric(taus) && prod(taus>0) && prod(taus<1))) {
    stop("'taus' needs to be specified as a vector of quantile orders")
  }
  
  if (!(dim(LPG)[2] == (length(taus)+1)*length(taus)/2)) {
    stop("length of vector 'taus' does not correspond to number of columns in LPG")
  }

  if (!(is.vector(CL) && prod(is.wholenumber(CL)) && prod(CL > 0) && prod(CL <= length(taus)))) {
    stop(paste("'CL' needs to be specified as a vector of indices between 1 and",length(taus)))
  }
  
  if (!(is.vector(F) && is.integer(F))) {
    stop("'F' needs to be specified as a vector of integers")
  } else {
    omegas <- round(as.numeric(rownames(LPG))*dim(LPG)[1]/pi)
    if (!is.subset(F,omegas)) {
      stop("'F' needs to be a subset of 'omegas' in 'LPG'")
    }
  }
  
  # Define and initiate auxiliary variable.
  ltau <- length(taus)

  # Set graphics parameters.
  par(mfcol=c(length(CL),length(CL)),oma=oma,mar=mar,cex.lab=cex.lab)

  # Create Matrix of values to plot
  # Used to determine xlim and ylim
  M <- matrix(c(Re(LPG[F,]),Im(LPG[F,])),nrow=length(F))

  # For each combination i1, i2 create a plot
  for (i1 in CL) {
    for (i2 in CL) {

      # Labels only for the plots at left and bottom
      if (i1 == CL[1]) {
        yl <- substitute(paste(tau[1],"=",k),list(k=taus[i2]))
      } else {yl <- ""}
      if (i2 == CL[length(CL)]) {
        xl <- substitute(paste(tau[2],"=",k),list(k=taus[i1]))
      } else {xl <- ""}

      # Arrange points to be ploted as X,Y matrix
      # Frequencies to be plotted       
      if (i2 >= i1) {
        ctau <- ct(i1,i2,ltau)
        # Real parts of the estimate
        Est <- matrix(c(as.numeric(rownames(LPG[F,]))/(2*pi),Re(LPG[F,ctau])),ncol=2)
      } else {
        ctau <- ct(i2,i1,ltau)
        # Imaginary parts of the estimate
        Est <- matrix(c(as.numeric(rownames(LPG[F,]))/(2*pi),Im(LPG[F,ctau])),ncol=2)
        ctau <- ctau+length(LPG[1,])
      }

      # Determine ylimits
      if (hOffset) {
        ylimits <- c(min(M),max(M))
      } else {
        if (hRange) {
          range <- max(apply(M,2,max)-apply(M,2,min))-max(M[,ctau])+min(M[,ctau])
          ylimits <- c(min(M[,ctau])-range/2,max(M[,ctau])+range/2)
        } else {
          ylimits <- c(min(M[,ctau]),max(M[,ctau]))
        }
      }

      xmin <- as.numeric(rownames(LPG)[min(F)])/(2*pi)
      xmax <- as.numeric(rownames(LPG)[max(F)])/(2*pi)
      plot(x=c(xmin,xmax),y=ylimits,type="n", xlab=xl, ylab=yl)
      lines(Est)
    }
  }
  mtext(expression(omega),outer=TRUE,side=1,line=0)
  mtext(ylabel,outer=TRUE,side=2,line=0)
   
  par(mfcol=c(1,1),oma=oma,mar=c(5, 4, 4, 2) + 0.1,cex.lab=1)
}
