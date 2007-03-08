\name{plot.dfa}
\alias{plot.dfa}
\title{Graphical Visualization of the Direct Filter Approach's Output}
\description{Yields a plot with 6 panels, i.e. an overlay of the
  original series with the filter output, and detailed plots of the
  filter output, periodogram of the input and output series, as well as
  the amplitude and time shift (delay) functions of the one-sided
  filter.}   
\usage{plot.dfa(x, ...)}
\arguments{\item{x}{An \R object of \code{\link{class}} \code{"dfa"},
    typically the result of \code{\link{dfa}()}.}
           \item{...}{Further arguments passed to or from other
	     methods.}}
\details{Please consult the reference given below for details about the
  interpretation of these plots.}
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

str(fit)
plot(fit)}
\keyword{ts}
\keyword{hplot}
