\name{coef.dfa}
\alias{coef.dfa}
\title{ARMA-coefficients for the Direct Filter Approach's Output}
\description{The one-sided filter that is obtained from
  \code{\link{dfa}} is translated into an ARMA(16,16)-model. These
  ARMA-coefficients are returned here.} 
\usage{coef.dfa(object, ...)}
\arguments{\item{object}{An \R object of \code{\link{class}}
             \code{"dfa"}, typically the result of \code{\link{dfa}()}.}
           \item{...}{Further arguments passed to or from other
	     methods.}}
\value{A matrix with 2 columns of 16 entries each. The first column
  contains the AR-coefficients, whereas the second is filled with the
  MA-coefficients.}
\author{Marc Wildi & Marcel Dettling, <[wia/dem]@zhwin.ch>}
\references{Marc Wildi, \emph{Real-Time Signal Extraction (Beyond
  Maximum Likelihood Principles)}, Springer. To appear in 2007.}
\seealso{\code{\link{dfa}}}
\examples{
data(fit)

## Instead of calling data(fit), one could run (time consuming)
## set.seed(21)
## data(x)
## fit <- dfa(x)

coef(fit)
plot(coef(fit))}
\keyword{ts}
