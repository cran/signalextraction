achsen <- function(text, n.pg)
  {
    axis(1, at=0:6*n.pg/12+1, labels=text)
    axis(2)
    box()
  }
