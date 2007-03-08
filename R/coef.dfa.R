coef.dfa <- function(object, ...)
  {
    out           <- cbind(object$ar.coef, object$ma.coef)
    colnames(out) <- c("AR-coefficients", "MA-coefficients")
    out
  }
