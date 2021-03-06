\name{chart.RelativePerformance}
\alias{chart.RelativePerformance}
\concept{relative performance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ relative performance chart between multiple return series }
\description{
Plots a time series chart that shows the ratio of the cumulative performance for two assets at each point in time and makes periods of under- or out-performance easier to see. 
}
\usage{
chart.RelativePerformance(Ra, Rb, main = "Relative Performance", xaxis = TRUE, colorset = (1:12), legend.loc = NULL, ylog = FALSE, elementcolor = "darkgray", lty = 1, cex.legend=0.7, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{main}{ set the chart title, same as in \code{plot} }
  \item{xaxis}{ if true, draws the x axis }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{elementcolor}{ provides the color for drawing less-important chart elements, such as the box lines, axis lines, etc. replaces \code{darken} }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{ylog}{ TRUE/FALSE set the y-axis to logarithmic scale, similar to \code{\link{plot}}, default FALSE }
  \item{cex.legend}{ the magnification to be used for sizing the legend relative to the current setting of 'cex'.  }
  \item{lty}{ set the line type, same as in \code{\link{plot}} }
  \item{\dots}{ any other passthru parameters }
}
\details{
To show under- and out-performance through different periods of time, a time series view
is more helpful. The value of the chart is less important than the slope of the line. If
the slope is positive, the first asset (numerator) is outperforming the second, and vice versa.
May be used to look at the returns of a fund relative to each member of the peer
group and the peer group index.
Alternatively, it might be used to assess the peers individually against an asset class or peer group index.
}

\author{ Peter Carl }
\seealso{
\code{\link{Return.relative}}
}
\examples{
data(managers)
chart.RelativePerformance(managers[, 1:6, drop=FALSE], managers[, 8, drop=FALSE], colorset=rich8equal, legend.loc="bottomright", main="Relative Performance to S&P")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
