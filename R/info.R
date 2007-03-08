info <- function(text, opti.nr, verbose, n.opt)
  {
    if (verbose == 1)
      {
        nr <- format(1:n.opt)
        cat("Optimization", nr[opti.nr], "of", n.opt, ":", text, "\n")
      }
  }
