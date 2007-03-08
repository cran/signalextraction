plot.dfa <- function(x,...)
  {
    ## Some derived definitions
    i.anf <- floor(1/6*(length(x$perall)-6))-1
    i.end <- length(x$argli$x)
    temp  <- length(x$argli$x) - x$argli$d
    n.pg  <- ifelse(x$argli$quart, floor(temp/4)*4, floor(temp/12)*12)

    ## Some adjustments for plotting the periodogram
    if (x$argli$d>0)
      {
        yfdiff <- diff(x$xf, differences=1)
        per1   <- periodogram(yfdiff, 1, n.pg)
      }
    if (x$argli$d<=0)
      {
        per1   <- periodogram(x$xf, x$argli$d, n.pg)
      }
    
    ## Grafik-Output
    par(mfrow=c(3,2))

    ## Allgemeine Definitionen
    pi6.lab <- c("0","pi/6","2pi/6","3pi/6","4pi/6","5pi/6","pi")
        
    ## Plot 1
    titl <- "Original and filtered series"
    pars <- list(xlab="", ylab="", main=titl, col=c("black","red"))
    ts.plot(ts(x$argli$x[i.anf:i.end]), ts(x$xf[i.anf:i.end]),gpars=pars)

    ## Plot 2
    titl <- "Filtered series"
    pars <- list(xlab="", ylab="", main=titl, col="red")
    ts.plot(ts(x$xf[i.anf:i.end]), gpars=pars)

    ## Plot 3
    titl <- "Periodogram input"
    plot(x$perall, type="l", axes=FALSE, main=titl, xlab="", ylab="")
    achsen(pi6.lab, n.pg)

    ## Plot 4
    titl <- "Periodogram output"
    what <- c(rep(NA,4),(per1[4:(n.pg/2+1)]))
    plot(what, type="l", axes=FALSE, main=titl, xlab="", ylab="")
    achsen(pi6.lab, n.pg)

    ## Plot 5
    titl <- "Amplitude boundary filter"
    plot(x$amp, type="l", axes=FALSE, main=titl, xlab="", ylab="")
    achsen(pi6.lab, n.pg)

    ## Plot 6
    titl <- "Delay boundary filter"
    plot(x$pha, type="l", axes=FALSE, main=titl, xlab="", ylab="")
    achsen(pi6.lab, n.pg)
  }
