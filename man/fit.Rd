\name{fit}
\alias{fit}
\docType{data}
\title{Example of an object of class dfa}
\description{This is an example of an \R object of \code{\link{class}}
  \code{"dfa"}, i.e. the result of the assignment \code{fit <- dfa(x)}
  on the business survey dataset \code{x}. Its purpose is to enable
  illustrative and meaningful examples without running the CPU-time
  intensive optimization routine.} 
\usage{data(fit)}
\format{An object of \code{\link{class}} \code{"dfa"}. For an
  explanation of its contents, see the help file of \code{\link{dfa}}.}
\source{Obtained from \code{set.seed(21); data(x); fit <- dfa(x)}}
\references{Marc Wildi, \emph{Real-Time Signal Extraction (Beyond
    Maximum Likelihood Principles)}, Springer. To appear in 2007.}
\examples{
data(fit)

## Instead of calling data(fit), one could run (time consuming)
## set.seed(21)
## data(x)
## fit <- dfa(x)

str(fit)
plot(fit)}
\keyword{datasets}
