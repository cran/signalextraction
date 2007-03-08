\name{fitted.dfa}
\alias{fitted.dfa}
\title{Fitted Values of the Direct Filter Approach's Output}
\description{This is the filtered real time series, i.e. the main output
  of the function \code{\link{dfa}}.} 
\usage{fitted.dfa(object, ...)}
\arguments{\item{object}{An \R object of \code{\link{class}}
             \code{"dfa"}, typically the result of \code{\link{dfa}()}.}
           \item{...}{Further arguments passed to or from other
	     methods}}
\value{Filter output}
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

fitted(fit)
plot(fitted(fit), type="l")}
\keyword{ts}
