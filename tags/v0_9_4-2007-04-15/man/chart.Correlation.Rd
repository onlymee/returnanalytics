\name{chart.Correlation}
\alias{chart.Correlation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ correlation matrix chart }
\description{
Visualization of a Correlation Matrix. On top the (absolute) value of the correlation plus the result of the cor.test as stars. On botttom, the bivariate scatterplots, with a fitted line
}
\usage{
chart.Correlation(x, y, histogram = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data for the x axis, can take matrix,vector, or timeseries }
  \item{y}{ data for the y axis, can take matrix,vector, or timeseries }
  \item{histogram}{ TRUE/FALSE whether or not to display a histogram }
}
\value{
correlation chart
}
\references{ }
\author{ Peter Carl }
\note{
based on plot at  \url{http://addictedtor.free.fr/graphiques/sources/source_137.R}
}
\seealso{
    \code{\link{table.Correlation}}
    \code{\link{chart.Correlation.color}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
