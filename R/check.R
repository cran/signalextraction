check <- function(al)
  {
    ## Check whether the input data are numeric
    if (!is.numeric(al$x))
      {
        stop ("Please supply a numerical vector for argument x")
      }

    ## Check if input data are a time series. If yes, abort
    if (is.ts(al$x))
      {
        text <- paste("Objects of class \"ts\" are not allowed as input.",
                      "Please convert it into a numerical vector")
        stop (text)
      }
    
    ## Check whether the data contain NA values
    if (any(is.na(al$x)))
      {
        stop ("Data contain NA values. This is not allowed.")
      }

    ## Check whether the data are long enough
    if ((length(al$x)<60&!al$quart)|(length(al$x)<22&al$quart))
      {
        warning (paste("Only", length(al$x), "observations. These are",
                       "too few for a meaningful analysis."))
      }

    ## Check whether quart is logical
    if (!is.logical(al$quart))
      {
        stop ("Please supply a logical variable for argument 'quart'")
      }

    ## Check whether d is within the allowed weight range
    if (!(al$d %in% c(0,1,2)))
      {
        stop ("The integration order d must be equal to 0, 1 or 2")
      }

    ## Check whether n.loops has a reasonable value
    if (al$n.loops<1 | al$n.loops>30 | al$n.loops!=floor(al$n.loops))
      {
        stop ("The argument 'n.loops' has to be an integer between 1 and 30")
      }

    ## Check whether lambda is within the allowed range
    if (al$lambda<1 | al$lambda>20)
      {
        stop("The argument 'lambda' for phase restriction has to be in [1,20]")
      }

    ## Check whether pbd has a reasonable value
    if (al$pbd < 1)
      {
        stop ("Argument 'pbd' needs to exceed a value of 1")
      }

    ## Checking the input variable tpfilter
    if (!is.logical(al$tpfilter))
      {
        stop ("Please supply a logical variable for argument 'tpfilter'")
      }

    ## Issue a warning if tpfilter=TRUE, but not d=0 and i2=TRUE
    if (al$tpfilter==TRUE & !(al$d==0 & al$i2==TRUE))
      {
        warning("If tpfilter==TRUE, then we recommend d=0 and i2=TRUE!")
      }

    ## Checking whether expweight is within the allowed range
    if (0.5 > al$expweight | al$expweight > 2)
      {
        stop ("Argument 'expweight' should have a value in [0.5, 2]")
      }

    ## Checking whether verbose has an allowed value
    if (!(al$verbose %in% c(0,1,2)))
      {
        stop ("Argument 'verbose' must be 0, 1 or 2")
      }

    ## Checking whether pb is between 0 and pi
    if (al$pb<0 | al$pb>pi)
      {
        stop ("Argument 'pb' should have a value in [0, pi]")
      }

    ## Checking whether sb is between 0 and pi
    if (al$sb<0 | al$sb>pi)
      {
        stop ("Argument 'sb' should have a value in [0, pi]")
      }

    ## Checking whether pb < sb
    if (al$pb>=al$sb)
      {
        stop ("The pass band pb has to be smaller than the stop band sb")
      }

    ## Checking whether limamp is within the recommended range
    if (al$tpfilter==FALSE & (al$limamp < 1.5 | al$limamp > 2))
      {
        stop ("For level estimation, argument 'limamp' should be in [1.5, 2]")
      }

    if (al$tpfilter==TRUE & (al$limamp < 3 | al$limamp > 5))
      {
        stop ("For turning point estimation, 'limamp' should be in [3, 5]")
      }

    ## Checking whether i2 is logical
    if (!is.logical(al$i2))
      {
        stop ("Please supply a logical variable for argument 'i2'")
      }
  }
