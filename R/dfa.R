dfa <- function(x, quart = FALSE, d = 0, pb = 1/14, sb = 1/7, tpfilter
                 = FALSE, lambda = 3, expweight = 1.5, pbd = if
                 ((length(x) < 100)) 1.08 else 1.03, limamp = if
                 (!tpfilter) 1.5 else 3, i2 = TRUE, n.loops = 10,
                 verbose = 1)
  {
    ## Renaming arguments to follow the old conventions
    dd    <- d
    fwing <- tpfilter
    phsc  <- lambda
    fwe   <- expweight

    ## Function call with new variable names
    argli <- list(x=x, quart=quart, d=dd, tpfilter=tpfilter, lambda=lambda,
                   expweight=expweight, pbd=pbd, pb=pb, sb=sb, n.loops=n.loops,
                   verbose=verbose)

    ## Checking the input for conformity
    check(argli)

    ## Additional (derived) variable definitions
    n.fit   <- length(x)
    iaf     <- FALSE
    lsf     <- ifelse(quart, 10, 30)
    nnull   <- ifelse(quart,  3,  7)
    saisopt <- 30

    ## Verbose controlling
    if (verbose == 0)
      {
        trac   <- 0
        report <- 1
      }

    if (verbose == 1)
      {
        trac   <- 0
        report <- 1
        n.opt  <- 2*n.loops + 3*floor((n.loops+1)/2) + 3
      }

    if (verbose == 2)
      {
        trac   <- 6
        report <- 1
      }

    ## different optimizations: filter constraints and pseudo periodogram
#    i2 <- TRUE
    if (dd==0) i0 <- TRUE
    if (dd>=1) i0 <- FALSE

    ## Definitions for the symmetrical transfer function
    ## (ub and lb used to be hard coded, but are now arguments again)
    ub <- sb
    lb <- pb

    ## Initialization for the ideal asymmetric filter
    zabs  <- c(ifelse(iaf,1.1,1.5), rep(1.0, nnull-1))
    pabs  <- c(ifelse(iaf,1.2,1.1), rep(ifelse(iaf,1.3,1.5),nnull-1))
    parma <- c(zabs, 0, pabs, 2.0, 0)

    ## How many observations are used for periodogram computation?
    temp <- n.fit - dd
    n.pg <- ifelse(quart, floor(temp/4)*4, floor(temp/12)*12)
    npg2 <- n.pg/2

    ## Exponential functions for the boundary filter
    mex <- matrix(data=0, nrow=npg2+1, ncol=2*nnull+1)
    for (j in 0:npg2)
      {
        mex[j+1,] <- rep(exp(j*1.i*pi/npg2), nnull*2+1)
      }

    ## Compute the periodogram for the series
    if (dd==0)
      {
        perall <- periodogram(x, dd, n.pg)
      }
    if (dd > 0)
      {
        if (dd < 3)
          {
            xdiff  <- diff(x, differences = dd)
            perall <- periodogram(xdiff, dd, n.pg)
          }
        if (dd >= 3)
          {
            xdiff  <- diff(x, differences=1)
            if (quart)
              {
                xdiff <- diff(xdiff, lag=4, differences=1)
              }
            if (!quart)
              {
                xdiff <- diff(xdiff, lag=12, differences=1)
              }
            perall <- periodogram(xdiff, dd, n.pg)
          }
      }

    ## Renormalizing the periodogram
    if (dd==13)
      {
        redvec <- perall[-(((length(perall)-1)/6)*(1:6)+1)]
        peclu  <- perall*(10000/sum(redvec))
      }
    if (dd!=13)
      {
        peclu  <- perall#@*(10000/sum(perall))
      }

    ## Ideal asymmetrical filter
    gammac <- idefi(x, lsf, ub, lb)[[2]]
    asid   <- (0:(n.pg/2))*1.i
    for (j in 0:(n.pg/2))
      {
        term1     <- gammac[(lsf+1):(2*lsf+1)]
        term2     <- complex(arg=j*(0:lsf)*2*pi/n.pg)
        asid[j+1] <- term1%*%term2
      }

    ## Symmetrical transfer function
    ttr       <- as.numeric(((1:(npg2+1)) <= npg2*ub))
    weli      <- floor((npg2*ub+1):(npg2*lb+1))
    term1     <- npg2*ub+1-((npg2*lb+1):(npg2*ub+1))
    ttr[weli[1]+weli[length(weli)]-weli] <- term1/(npg2*ub-npg2*lb)

    ## Definition of weights
    wgts <- 1
    if (fwing)
      {
        temp <- c(rep(1, n.pg*sb/2+1), (1:(npg2-n.pg*sb/2)))
#@        wgts <- (0:npg2)^2 * exp(fwe*log(0.001+temp))
        lof<-sum((1:(npg2+1))<=npg2*ub)
        wgts<-c(rep(1,lof),exp(fwe*log(0.001+1+(0:npg2)))[1:(npg2+1-lof)]) #altes Baro

      }

    ## Adding a null
    ifelse(quart, parw <- rep(TRUE,2), parw <- rep(TRUE,6))
    temp <- ifelse(quart, 2, 6)
    maxt <- max(peclu[1:(npg2/temp-2)])
    parh <- (peclu[(1:temp)*npg2/temp+1]<maxt/saisopt)&parw

    ## If we have frequency weighting, a null is set a Pi
    if (!quart & fwing) parh[6] <- FALSE

    ## For I(2) filters, set a pole at frequency zero
    lim <- ifelse(i2, 1+nnull, nnull)

    ## Configure the argument list for funcret()
    al <- list(n.pg = n.pg, peclu = peclu, nnull = nnull, pbd = pbd,
               phsc = phsc, i2 = i2, i0 = i0, asid = asid, gammac =
               gammac, iaf = iaf, quart = quart, mex = mex, ttr = ttr,
               wgts = wgts, parh = parh, parw = parw, lim = lim,
               limamp = limamp)

    ## First round of filtering to determine the scaling constant
    al$i0   <- FALSE
    funcret(parma, al)
    parma   <- c(parma, tff1)
    al$i0   <- i0
    der     <- 1e+90
    parmi   <- parma
    sdf     <- NULL

    ## Multiple intialisations using the original parameters
    for (i in 1:n.loops)
      {
        ## Stochastic annealing
        info("Stochastic Annealing", 2*(i-1)+1, verbose, n.opt)

        co   <- list(REPORT=report, trace=trac, maxit=1000)
        pobj <- optim(parmi, funcret, method="SANN", control=co, al=al)

        ## Nelder-Mead for 1st loop or if annealing solution is unique
        if (i==1 || any(abs((pobj$value-sdf)/sdf) > 1e-5))
          {
            info("Nelder-Mead", 2*(i-1)+2, verbose, n.opt)
            parme <- pobj$par
            sdf   <- c(sdf, pobj$value)
            co   <- list(REPORT=report, trace=trac, maxit=1500)
            pobj  <- optim(parme, funcret, method="Nelder-Mead", control=co, al=al)

          }

        ## Check for best value and keep corresponding parameters
        if (pobj$value < der)
          {
            der   <- pobj$value
            parma <- pobj$par
          }
      }

    ## Use the best initialization and continue optimizing
    for (i in 1:((n.loops+1)/2))
      {
        info("Stochastic Annealing", 2*n.loops+(i-1)*3+1, verbose, n.opt)
        co    <- list(REPORT=report, trace=trac, maxit=1000)
        pobj  <- optim(parma, funcret, method="SANN", control=co, al=al)
        parma <- pobj$par

        info("Nelder-Mead", 2*n.loops+(i-1)*3+2, verbose, n.opt)
        co    <- list(REPORT=report, trace=trac, maxit=1500)
        pobj  <- optim(parma, funcret, method="Neld", control=co, al=al)
        parma <- pobj$par

        info("BFGS", 2*n.loops+(i-1)*3+3, verbose, n.opt)
        co    <- list(REPORT=report, trace=trac, maxit=100)
        pobj  <- optim(parma, funcret, method="BFGS", control=co, al=al)
        parma <- pobj$par
      }

    ## The last 3 optimizations: BFGS, Nelder-Mead and BFGS again
    info("BFGS", 2*n.loops+3*floor((n.loops+1)/2)+1, verbose, n.opt)
    co    <- list(REPORT=report, trace=trac, maxit=100)
    pobj  <- optim(parma, funcret, method="BFGS", control=co, al=al)
    parma <- pobj$par

    info("Nelder-Mead", 2*n.loops+3*floor((n.loops+1)/2)+2,verbose,n.opt)
    co    <- list(REPORT=report, trace=trac, maxit=30000)
    pobj  <- optim(parma, funcret, method="Neld", control=co, al=al)
    parma <- pobj$par

    info("BFGS", 2*n.loops+3*floor((n.loops+1)/2)+3, verbose, n.opt)
    co    <- list(REPORT=report, trace=trac, maxit=1000)
    pobj  <- optim(parma, funcret, method="BFGS", control=co, al=al)
    parma <- pobj$par

    ## Storing the final target function value
    revdfa <- funcret(parma, al)

    ## Compute some additional parameters
    cf      <- parret(z, zbar, p, pbar, tff1)
    ar.coef <- cf$ar.coef
    ma.coef <- cf$ma.coef

    ## In case of an I(2) process, add a pole combination at frequency 0
    lim <- ifelse(i2,(2*length(zbar)+2),(2*length(z)+1))

    ## Compute the filter
    ar1 <- ar.coef[1:lim]
    ma1 <- ma.coef[1:lim]
    yf  <- filtret(ar1, ma1, n.fit, x-mean(x), gammac, iaf, quart)

    ## Definition of the output class
    out <- list(argli=argli, xf=yf+mean(x), critval=revdfa, ar.coef=
                ar.coef, ma.coef=ma.coef, perall=perall, amp=amp, pha=pha)
    class(out) <- "dfa"

    ## Return value
    out
  }
