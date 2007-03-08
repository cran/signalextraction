filtret <- function(ar1, ma1, n.fit, xm, gammac, iaf, quart)
  {
    ## Definitions
    initlen     <- 1400
    plen        <- length(ar1)
    xi          <- xm
    xi[1:n.fit] <- xm[n.fit:1]

    ## Fitting an Arima model
    fit <- arima(xi, include.mean=FALSE, order=c(0,1,1), method="ML",
                 seasonal=list(order=c(0,1,1),period=ifelse(quart,4,12)))

    ## Prediction
    fore <- predict(fit, n.ahead = initlen)$pred  
    xi   <- c(xi,fore)
    xe   <- xi[(n.fit+initlen):1] 
    ye   <- xe

    ## Remodeling
    for (i in plen:(n.fit+initlen))
      {
        term1 <- -ye[(i-1):(i-plen+1)]%*%ar1[2:plen]
        term2 <- xe[i:(i-plen+1)]%*%ma1
        ye[i] <- term1+term2
      }

    ## If iaf == TRUE then combine the filters. Important: iaf is defunct in
    ## the main routine and hard-coded to iaf == FALSE. So this piece of code
    ## below is normally not run. 
    if (iaf)
      {
        prot <- (length(gammac)-1)/2
        xe   <- ye
        for (i in (prot+1):(n.fit+initlen))
          {
            ye[i] <- xe[i-0:prot]%*%gammac[(prot+1):length(gammac)]     
          }
      }

    ## Output
    return(ye[initlen+1:n.fit])
  }

