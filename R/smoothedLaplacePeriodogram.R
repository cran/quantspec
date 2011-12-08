smoothedLaplacePeriodogram <-
function(LPG, taus, W) {
 
  # Verify if all parameters are valid
  
  if (!is.vector(W) && (length(W) %% 2) == 0) {
    if (is.tskernel(W)) {
      W <- W$coef
      W <- c(W[length(W):2],W[1:length(W)])
    } else {
      stop("'W' needs to be specified as a vector with an odd number of entries or as a tskernel object")
    }
  }
  
  if (!(is.vector(taus) && is.numeric(taus) && prod(taus>0) && prod(taus<1))) {
    stop("'taus' needs to be specified as a vector of quantile orders")
  }
  
  if (!(dim(LPG)[2] == (length(taus)+1)*length(taus)/2)) {
    stop("length of vector 'taus' does not correspond to number of columns in LPG")
  }
  
  # Define and initiate auxiliary variables.
  lomega <- length(LPG[,1])
  ltau <- length(taus)
  lW <- length(W)
  res <- matrix(rep(0,(lomega*ltau*(ltau+1)/2)),ncol=(ltau*(ltau+1)/2))
  # Normalize vector of weights
  W <- W / sum(W)
   
  # Extend LPG in the beginning and at the end using the symmetry.
  m <- (lW-1)/2
  extendedLPG <- matrix(c(t(Conj(LPG[(m-1):1,])),Conj(LPG[1,]),t(LPG[1:lomega,]),
    t(Conj(LPG[(lomega):(lomega-m),]))),ncol=(ltau*(ltau+1)/2),byrow=T)
   
  # Calculate the weighted averages for each frequency.
  for (i in 1:lomega) {
    res[i,] <- W %*% extendedLPG[i:(i+2*m),]
  }

  # Add column and row names and return the result.
  colnames(res) <- colnames(LPG)
  rownames(res) <- rownames(LPG)
  res
}
