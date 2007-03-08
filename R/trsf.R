trsf <- function(col, quart)
  {
    out <- 1
    if (abs(Arg(col[2])) > pi/ifelse(quart,2,6)-1.e-4)
      {
        out <- (min(abs(col[1]),abs(col[2]))/abs(col[1]))
      }
    out
  }
