## Computation of AR- and MA-coefficients given poles and zeroes
## of the one-sided filter
parret <- function(z, zbar, p, pbar, tff1)
  {
    ## Some definitions
    z1      <- c(z,zbar)
    p1      <- c(p,pbar)
    ar.coef <- numeric(length(z1)+1)
    ma.coef <- numeric(length(z1)+1)
    butb    <- 1:(length(z1)+1)
    buta    <- 1:(length(z1)+1)

    ## Computation of buta and butb
    for (j in 1:(length(z1)))
      {
        butb[j+1] <- -butb[j]*z1[j]
        buta[j+1] <- -buta[j]*p1[j]
        
        if (j>1) 
          {
            for (k in j:2)
              {
                butb[k] <- butb[k]-butb[k-1]*z1[j]
                buta[k] <- buta[k]-buta[k-1]*p1[j]
              }
          }
      }

    ## Computation of ar.coef and ma.coef
    for (j in 1:(length(z1)+1))
      {
        ar.coef[j] <- Re(buta[length(z1)+2-j])
        ma.coef[j] <- Re(butb[length(z1)+2-j])/(ar.coef[1]*tff1)
      }

    ## Output
    return(list(ar.coef=ar.coef/ar.coef[1], ma.coef=ma.coef))
  }

