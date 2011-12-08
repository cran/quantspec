# Taken from quantreg-package and adapted.
".onAttach" <- function(lib, pkg) {
    if(interactive() || getOption("verbose"))
  packageStartupMessage("Package quantspec loaded.\n     To cite, see citation(\"quantspec\").\n     For demos, see demo(\"quantspecOnScreen\") and demo(\"quantspecPdf\").")
}

LaplacePeriodogram <-
function (X, taus, omegas=1:(ceiling(length(X)/2)-1), fromRanks=TRUE, showProgressBar=FALSE) {

  # Verify if all parameters are valid
  if (!is.vector(X) & !is.ts(X) & !is.zoo(X)) {
    stop("'X' needs to be specified as a vector, a ts or a zoo object")
  }
  
  if (is.ts(X)) {
    X <- X[1:(length(X))]
  }
  
  if (is.zoo(X)) {
    X <- coredata(X)
  }
  
  if (!(is.vector(taus) && is.numeric(taus) && prod(taus>0) && prod(taus<1))) {
    stop("'taus' needs to be specified as a vector of quantile orders")
  }
  
  if (!(is.vector(omegas) && prod(is.wholenumber(omegas)))) {
    stop("'omegas' needs to be specified as a vector of integers")
  } else {
    if (!(prod(omegas>=0) * prod(omegas<=floor(length(X)/2)))) {
      stop(paste("all values in omegas need to be integeres between 1 and",floor(length(X)/2)))
    }
  }
  
  # Convert Data to standardized ranks
  if (fromRanks) {
    X <- rank(X)/length(X)
  }
  
  # Initialize Progress Bar
  if (showProgressBar) {
    maxPb <- length(omegas)+1
    pb <- tkProgressBar(title = "Calculate Laplace Periodogram", min=0, max=maxPb, width = 400)
  }
  
  # Define and initiate auxiliary variables.
  n <- length(X)
  ltau <- length(taus)
  res <- rep(floor(n/2)*ltau*(ltau+1)/2,0)
  ctau <- 1
  ires <- 1

  # Then calculate the Laplace Periodogram at all frequencies in omegas.
  for (i in omegas) {
    if (showProgressBar) {
      iPb <- getTkProgressBar(pb)+1
      setTkProgressBar(pb, iPb, label=paste(round(100*iPb/maxPb,0),"% done"))
    }
    # Define the harmonic regressors.
    omega <- (2 * pi * i) / n
    D <- cos(omega*1:n)
    S <- sin(omega*1:n)

    # Then performe the quantile regression.
    qregSol <- coef(rq(X ~ 1 + D + S, taus))
    if (length(taus)>1) {
      qregSol <- qregSol[2:3,]
    } else {
      qregSol <- matrix(qregSol[2:3],ncol=1)
    }

    # Then for each combination of taus ...
    for (i1 in 1:ltau) {
      for (i2 in i1:ltau) {
        B1 <- qregSol[,i1]
        B2 <- qregSol[,i2]
        res[ires] <- n/4 * complex(real = t(B1) %*% B2, imaginary = B1[2]*B2[1]-B1[1]*B2[2])
        ires <- ires+1
      }
    }
  }
   
  if (showProgressBar) {
    close(pb)
  }

  # At last prepare the row and column labels
  cnames <- rep(ltau*(ltau+1)/2,0)
  for (i1 in 1:ltau) {
    for (i2 in i1:ltau) {
      cnames[ctau] <- paste(taus[i1],taus[i2],sep="-")
      ctau <- ctau+1
    }
  }
  rnames=2*pi/n*omegas
   
  # Then return the result as a matrix.
  matrix(res,ncol=(ltau*(ltau+1)/2),nrow=length(omegas),byrow=T,dimnames=list(rnames,cnames))
}
