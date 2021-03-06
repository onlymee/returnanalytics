\name{SemiDeviation}
\alias{SemiDeviation}
\alias{SemiVariance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ deviation below the mean of the return distribution }
\description{
Semideviation is the portion of the return stream below the mean return.
}
\usage{
SemiDeviation(R)
SemiVariance(R)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
}
\details{
Semidiviation or SemiVariance is a popular alternative downside risk measure that may be used in place of standard deviation or variance.

This function is implemented as a wrapper of DownsideDeviation with MAR=mean(R).

In many functions like Markowitz optimization, semideviation may be substituted directly, and the covariance matrix may be constructed from semideviation rather than from variance.
}
\value{
vector of returns below the mean return
}
\references{
\url{http://en.wikipedia.org/wiki/Semivariance}
}
\author{ Peter Carl }
\note{}
\seealso{
    \code{\link{DownsideDeviation}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
