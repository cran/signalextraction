\name{dfa}
\alias{dfa}
\title{Direct Filter Approach}
\description{The Direct Filter Approach (DFA) provides efficient
  estimates of signals at the current boundary of time series in
  real time. For that purpose, one-sided ARMA-filters are computed by
  minimizing customized error criteria. The DFA can be used for
  estimating either the level or turning-points of a series, owing to
  the fact that that both criteria are incongruent and cannot be
  optimized simultaneously. In the context of real time turning-point
  detection, various risk profiles can be operationalized, which account
  for the speed and/or the reliability of the one-sided filter.}
\usage{dfa(x, quart = FALSE, d = 0, pb = 1/14, sb = 1/7, tpfilter =
    FALSE, lambda = 3, expweight = 1.5, pbd = if((length(x)<100)) 1.08
    else 1.03, limamp = if(!tpfilter) 1.5 else 3, i2 = TRUE, n.loops =
    10, verbose = 1)}
\arguments{
  \item{x}{Numerical vector, containing a time series with at least 61
    monthly or 23 quarterly entries. Missing values are not
    allowed.}
  \item{quart}{Logical, describes whether argument \code{x} contains
    quarterly data. If so, set \code{quart=TRUE}, for monthly data the
    default value \code{quart=FALSE} is appropriate.}
  \item{d}{Numerical, defaults to 0. Corresponds to the integration
    order of the series. It may take the values \{0, 1, 2,\}. If the
    series is bounded, no differencing (d=0) is necessary. For trending
    series the optimization criterion is biased, see section 6.1 in
    Wildi (2007). Taking first order differences (d=1) in such a case
    enables to reestablish efficiency (unbiasedness) by a transformation
    of the pseudo-periodogram.}
  \item{pb}{Numerical, takes values between 0 and \eqn{\pi}, defaults to
    \eqn{pi/14} and determines the pass band. Together with argument
    \code{sb}, it describes the shape of the symmetric transfer
    function.}
  \item{sb}{Numerical, takes values between 0 and \eqn{\pi}, defaults to
    \eqn{pi/7} and determines the stop band. Together with argument
    \code{pb}, it describes the shape of the symmetric transfer
    function.}
  \item{tpfilter}{Logical, defaults to \code{TRUE}, which makes the
    \code{dfa}-routine a turning point filter. In this case, the two
    variables \code{lambda} and \code{expweight} account for speed
    and/or reliability of the filter, see section 5.3 in Wildi
    (2007). Else, if \code{tpfilter=FALSE}, the \code{dfa}-routine
    corresponds to a best level filter.}
  \item{lambda}{Numerical, defaults to 3, but only affects the output if
    \code{tpfilter} ist set to \code{TRUE}. Argument \code{lambda} is a
    Lagrange parameter for phase restriction that corresponds to
    \eqn{\chi} in formula 5.3 of Wildi (2007). Larger values induce
    smaller time delays, but noisier signal estimates. Typical values
    for \code{lambda} are in [1,20].} 
  \item{expweight}{Numerical, defaults to 1.5, but only affects the
    output if \code{tpfilter=TRUE}. Reasonable values are in [0.5,2].
    Argument \code{expweight} determines the shape of the 
    frequency weighting function \eqn{W(\omega)} in formula 5.3 of Wildi
    (2007). More precisely, \eqn{W(\omega)=abs(\omega)^{expweight}}. Larger
    values emphasize the stop band of the filter. Therefore
    high-frequency components are damped more effectively. This results
    in increased reliability.}
  \item{pbd}{Numerical, takes values > 1 and defaults to 1.08 for series
    with less than 100 observations, and 1.03 else. This argument is a
    regularity constraint. Moduli of poles of the ARMA filter are
    constrained to exceed \code{pbd}. Larger values of \code{pbd}
    generally imply a smoother transfer function.} 
  \item{limamp}{Numerical, defaults to 1.5 for turning point estimation,
    and to 3 for best level estimation. This argument is a regularity
    constraint that constrains the amplitude to be smaller than the
    product of limamp and A(0). It should be larger than 1, and we have
    found that values between 1.5 and 2 are fine in the case of level
    approximation \code{tpfilter=FALSE}, whereas values between 3 and 5
    are reasonable constraints in the case of turning point estimation.}
  \item{i2}{Logical, defaults to \code{TRUE}. If \code{TRUE}, it forces
    the time delay of the one-sided filter to vanish at frequency
    zero. In practice, it is often the case that the use of this
    constraint leads to better estimates, especially if turning points
    are of interest, i.e. if \code{tpfilter=TRUE}.}  
  \item{n.loops}{Numerical, defaults to 10. It is the number of initial
    Stochastic Annealing proposals for the nonlinear optimization in
    computing the filter, of which the best is chosen and
    optimized further. A bigger number usually leads to a more precise
    solution, but the code takes longer to run.}
  \item{verbose}{Numerical, defaults to 1 and steers the amount of text
    output in the optimization routine. Allowed values are 0 (no output
    at all), 1 (minimal output for run-time control) and 2 (full output
    for tracking the optimization steps).}
}
\details{Please note that we here refer to Wildi (2007) for further
  details. Depending on how the arguments are set, the main function
  \code{dfa} minimizes one of the error criteria presented in the
  book. This is either the one given in formula 3.7 (for level
  estimation on stationary data: \code{d}=0, \code{tpfilter=FALSE}),
  6.7 (for level estimation on non-stationary trending data:
  \code{d}=1, \code{tpfilter=TRUE}) or 5.4 (detection of turning
  points, \code{tpfilter=TRUE}). The level-criteria 3.7 and 6.7 mate the 
  structure of the underlying estimation problem because one can show
  that they correspond to efficient estimates of the (unknown)
  mean-square real-time filter error. Criterion 5.4 is a generalization
  (for \eqn{\chi}=1 and W(\eqn{\omega})= 1 the preceding level
  criteria result) which enables to improve speed (smaller time delay in
  the pass-band) and reliability (better damping in the stop-band) of
  the filter. One can show that it improves level characteristics in the
  vicinity of turning points and that delays are weighted more heavily
  than anticipations, see sections 5.2 and 5.3 in Wildi (2007).} 
\value{An object of class \code{dfa}, containing
  \item{argli}{The function call in a list with values for all arguments}
  \item{xf}{The filtered real time series}
  \item{critval}{Corresponds to the minimized criterion value (either of the
    formulas 3.7, 5.4 or 6.7 in Wildi (2007), according to the arguments
    in the function call). In the case of level estimation, this value
    is also an estimate of the mean-square filter error (computed in the
    frequency domain).} 
  \item{ar.coef}{AR-coefficients of the one-sided filter}
  \item{ma.coef}{MA-coefficients of the one-sided filter}
  \item{perall}{The periodogram (for \code{d}=0) or pseudo-periodogram
    (for \code{d=1}) of the input series}
  \item{amp}{Amplitude function of the one-sided real time filter}
  \item{pha}{Time delay (phase divided by frequency) of the one-sided
    real time filter}
}
\references{Marc Wildi, \emph{Real-Time Signal Extraction (Beyond
  Maximum Likelihood Principles)}, Springer. To appear in 2007.}
\author{Marc Wildi & Marcel Dettling, <[wia/dem]@zhwin.ch>}
\seealso{\code{\link{coef.dfa}}, \code{\link{fitted.dfa}},
  \code{\link{plot.dfa}}, \code{\link{outsamp}}}
\examples{
data(fit)

## Instead of calling data(fit), one could run (time consuming)
## set.seed(21)
## data(x)
## fit <- dfa(x)

plot(fit)
}
\keyword{ts}
\keyword{smooth}
