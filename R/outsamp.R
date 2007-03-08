outsamp <- function(object, newdata=NULL, sequel=TRUE, ...)
  {
    ## If no new data are given, just return the in-sample filter output
    if (is.null(newdata)) out <- object$xf

    ## If there are new data, use filtret for their prediction
    if (!is.null(newdata))
      {
        ## If the new data are a sequel to the original series, combine
        if (sequel) newdata <- c(object$argli$x, newdata)

        ## Compute and return the filtered values
        o1  <- filtret(object$ar.coef, object$ma.coef, length(newdata),
                       newdata-mean(newdata),NULL,FALSE,object$argli$qua)
        out <- o1+mean(newdata)

        ## If the new data are a sequel, strip off the first part again
        if (sequel) out <- out[(length(object$argli$x)+1):length(out)]
      }

    ## Return
    out

    ## Comment: the 5th argument of filtret() is set to 'NULL' here. It
    ##          used to be equal to 'object$gammac', but since it was
    ##          decided that argument 'iaf' is defunct in the main
    ##          routine dfa(), 'gammac' is no longer necessary in
    ##          filtret(), and is no longer an output argument of dfa().
    ##
    ##          The 6th argument of filtret() is set to 'FALSE' here. It
    ##          used to be equal to 'object$argli$iaf'. Again, the reason
    ##          is that argument 'iaf' is defunct now and thus no longer
    ##          an element of the argument list of the dfa class.
  }
    
