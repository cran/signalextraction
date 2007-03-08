## Computating the spectral estimate: periodogram for stationary series,
## and pseudo-periodogram for integrated processes
periodogram <- function(x, dd, n.pg)
  {
    ## Preparations
    n.fit  <- length(x)
    xx     <- x[((n.fit-n.pg+1):n.fit)-dd]
    npg2   <- n.pg/2
    perall <- 0*0:npg2
    
    ## Case without a seasonal component    
    if (dd < 3)
      {
        for (j in (1:npg2))
          {
            term1 <- abs(xx%*%exp((1:(2*npg2))*1.i*j*pi/npg2))^2
            term2 <- abs(1-exp(j*1.i*pi/npg2))^(2*dd)
            perall[j+1] <- term1/(1-min(dd,1)+min(1,dd)*term2)     
          }
      }

    ## Case with a seasonal component, special treatment for Pi/6
    if (dd >= 3)
      {
        for (j in (1:npg2)[(-npg2/6)*(1:6)])
          {
            term1 <- abs(xx%*%exp((1:(2*npg2))*1.i*j*pi/npg2))^2
            term2 <- abs(1-exp(j*1.i*pi/npg2))^2
            term3 <- abs(1-exp(12*j*1.i*pi/npg2))^2
            perall[j+1] <- term1/(term2*term3)                
          }
        perall[(npg2/6)*(1:6)+1] <- max(perall)*100000
      }

    ## Output
    return(perall)
  }

