## MA-coefficients symmetric filter and output (trend signal approximation) 
idefi <- function(x, lsf, sb, pb)    
  {
    ## Definition of gammac
    gammac        <- numeric((2*lsf)+1)     
    gammac[lsf+1] <- (pb+sb)/2
    weli          <- (2:(lsf+1))+lsf
    term1         <- -(1./(pi*(pi*sb-pi*pb)))
    term2         <- (cos((1:lsf)*pi*sb)-cos((1:lsf)*pi*pb))/((1:lsf)^2)
    gammac[weli]  <- term1*term2
    gammac[1:lsf] <- gammac[(2*lsf+1):(lsf+2)]
    gammac        <- gammac/sum(gammac)

    ## Filtering
    n.fit <- length(x)
    zttre <- x

    for (j in (1+lsf):(n.fit-lsf))  
      {
        zttre[j] <- x[j+(-lsf:lsf)]%*%gammac
      }

    ## Symmetry
    zttre[1:lsf]             <- zttre[lsf+1]
    zttre[(n.fit-lsf+1):n.fit] <- zttre[n.fit-lsf]

    ## Ausgabe
    return(list(zttre, gammac))
  }
