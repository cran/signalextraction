\name{outsamp}
\alias{outsamp}
\title{Out of Sample Method for the Direct Filter Approach}
\description{Out of sample values based on the direct filter approach.} 
\usage{outsamp(object, newdata=NULL, sequel=TRUE, ...)}
\arguments{\item{object}{An \R object of \code{\link{class}}
    \code{"dfa"}, typically the result of \code{\link{dfa}()}.}
           \item{newdata}{For out-of-sample values, this has to be
	     a numerical vector. It can be of arbitrary length but has
	     to contain further observations of the time series x. If
	     \code{newdata} is omitted (and therefore equals
	     \code{NULL}), then the in-sample fitted values are
	     returned.}
	   \item{sequel}{Logical, defaults to \code{TRUE} and only has
	     an effect if there are \code{newdata}. If set to
	     \code{TRUE}, it is assumed that the new data are a direct
	     sequel to the original series and thus, these original
	     values are used for the asymmetric filter. If \code{sequel}
	     is \code{FALSE}, then the observations prior to the start
	     of the new series are estimated by a backcast.}
	   \item{...}{Further arguments passed to or from other
	     methods.}}
\details{This function yields a so-called nowcast for a time series, by
  applying the asymmetrical filter obtained from \code{\link{dfa}}.
  Initial values, at the beginning of a time series, are estimated by
  backcasting it. This function, \code{outsamp} can also be used to
  produce out-of-sample nowcasts, where the asymmetrical filter is
  applied to a time series that was not involved in its estimation.}
\value{Output is a numerical vector of the same length as the input
  vector. It contains the filtered time series.}
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

outsamp(fit)
plot(outsamp(fit))
plot(outsamp(fit, newdata=rnorm(100), sequel=FALSE))}
\keyword{ts}
