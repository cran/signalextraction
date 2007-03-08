## Filter Optimization
funcret <- function(parm, al) 
  {
    ## Constrain parameter values to meaningful settings: otherwise the
    ## optimization may break down
    sp   <- sign(parm)
    parm <- sp*apply(cbind(abs(parm),1000000*rep(1,length(parm))),1,min)

    ## Half of the frequency ordinates
    npg2  <- al$n.pg/2

    ## Keeping the filter stable
    weli <- which(abs(parm[al$nnull+1+1:al$lim])<al$pbd)
    parm[weli+al$nnull+1] <- al$pbd
    
    ## Construction of zeros and poles
    if (sum(al$parw)>0)
      {
        arg   <- if (al$quart) (1:2)*pi/2 else (1:6)*pi/6 
        zarg  <- c(parm[(al$nnull+1)], arg[al$parw])
            
        ## If the norm of the zero is ok, set the parameter, else 1
        temp  <-  ifelse(al$parh,parm[1+1:length(al$parh)][al$parh],1) 
        zabs  <-  abs(c(parm[1],temp[al$parw]))         

        z     <<- zabs*exp(1.i*zarg)  
        zbar  <<- zabs*exp(-1.i*zarg)

        parmp <-  parm[(al$nnull+1)+2:al$nnull]
        
        p     <<- abs(c(parm[al$nnull+2],parmp[al$parw]))*exp(zarg*1.i)
        pbar  <<- abs(c(parm[al$nnull+2],parmp[al$parw]))*exp(-zarg*1.i)
        
      }
    if (sum(al$parw)==0) 
      {
        z    <<- abs(parm[1])*exp(parm[al$nnull+1]*1.i)
        zbar <<- abs(parm[1])*exp(-parm[al$nnull+1]*1.i)
    
        p    <<- abs(parm[al$nnull+2])*exp(parm[al$nnull+1]*1.i)    
        pbar <<- abs(parm[al$nnull+2])*exp(-parm[al$nnull+1]*1.i)
      }
    
    ## Avoid that poles at seasonal frequencies are closer than nulls
    z    <<- apply(rbind(z, p), 2, trsf, al$quart)*z
    zbar <<- apply(rbind(zbar, pbar), 2, trsf, al$quart)*zbar
    
    ## Transfer function of the boundary filter
    temp1  <- apply(c(z,zbar)-t(al$mex[,1:((sum(al$parw)+1)*2)]),2,prod)
    temp2  <- apply(c(p,pbar)-t(al$mex[,1:((sum(al$parw)+1)*2)]),2,prod)
    trffkt <- temp1/temp2
    
    ## Convolution of transfer function and ideal asymmetric filter
    temp <- exp(-(0:npg2)*1.i*0*pi/npg2) * (1-al$iaf+al$iaf*al$asid)
    trffkt[1:(npg2+1)] <- trffkt[1:(npg2+1)]*temp
    
    ## Optimize the standardization constant
    tff1 <<- ifelse(al$i0, parm[length(parm)], Re(trffkt[1]))

    ## Removing negative values (for aesthetical reasons)
    if (Re(trffkt[1]/tff1)<0) tff1 <<- -tff1
    amp <<- abs(trffkt)/tff1

    ## Correction of the constant for short series and small amplitudes
    if ((length(amp) < 50) & (amp[1] < 0.75))
      {
        tff1 <<- sign(tff1)*abs(trffkt)[1]/0.75
        amp  <<- abs(trffkt)/abs(tff1)
      }
    temp <-  Arg(trffkt[2:(npg2+1)])*(npg2)/((1:npg2)*pi)
    
    ## Analytical computation of the delay at frequency 0
    prot <- (length(al$gammac)-1)/2
    pha  <<- -Re((rep(1,(sum(al$parw)+1)*2)%*%c(1/(z-1),
              1/(zbar-1))-rep(1,(sum(al$parw)+1)*2)%*%
              c(1/(p-1),1/(pbar-1))))+al$iaf*
              al$gammac[(prot+1):length(al$gammac)]%*%
              (0:prot)/sum(al$gammac[(prot+1):length(al$gammac)])
    pha  <<- c(pha,temp)
    
    ## Special treatment for I(2)-filters
    if (al$i2)
      {
        ## Convolution, transfer function and constant
        p0     <-  parm[length(parm)-2]
        z0     <-  (p0+pha[1]*(p0-1))/(1+pha[1]*(p0-1))
        trffkt <-  (z0-al$mex[,1])/(p0-al$mex[,1])*trffkt 
        tff1   <<- ifelse(al$i0, parm[length(parm)], Re(trffkt[1]))
        tff1   <<- abs(tff1)

        ## Removing negative values
        if (Re(trffkt[1]/tff1)<0)  tff1<<- -tff1
        amp <<- abs(trffkt)/tff1

        ## Correction of the constant
        if ((length(amp)<50) & (amp[1]<0.75))
        {
          tff1 <<- sign(tff1)*abs(trffkt)[1]/0.75
          amp  <<- abs(trffkt)/abs(tff1)
        }
        pha <<- c(1,Arg(trffkt[2:length(trffkt)]/tff1)*(length(trffkt)-1)/
                  ((1:(length(trffkt)-1))*pi))
        ## Computation of the delay at frequency 0
        pha[1] <<- -Re((rep(1,(sum(al$parw)+1)*2+1)%*%
                    c(1/(c(z0,z)-1),1/(zbar-1))-rep(1,(sum(al$parw)+1)*
                    2+1)%*%c(1/(c(p0,p)-1),1/(pbar-1))))+al$iaf*
                    al$gammac[(prot+1):length(al$gammac)]%*%
                    (0:prot)/sum(al$gammac[(prot+1):length(al$gammac)])
        
        ## Expand zeros and poles for computing AR- and MA-parameters
        z <<- c(z0,z)
        p <<- c(p0,p)
      }
    
    ## Preparing the output
    if (length(parm) > 2*al$nnull+3)
      {
        temp <- max(abs(trffkt)/abs(trffkt[1]))
        if (max(abs(trffkt[1:((length(trffkt)-1)/6)]/trffkt[1]))>al$limamp |
            min(abs(p))<al$pbd)
          {
            out <- 1e+90
          } else
          {
            temp <- (al$peclu%*%(abs(abs(trffkt)/tff1-al$ttr)^2*al$wgts)+
                     2*al$phsc*al$peclu%*%(al$wgts*abs(trffkt/tff1)*
                     al$ttr*(1-cos(Arg(trffkt)))))/(2*npg2^2)
            out  <- min(1e+8, temp)
          }
      } else
      {
        out <- 1e+8
      }

    ## Output
    out
  }
