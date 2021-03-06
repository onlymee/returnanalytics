DownsideDeviation <-
function (R, MAR = 0, method=c("subset","full"))
{ # @author Peter Carl

    # DESCRIPTION:
    # Downside deviation, similar to semi deviation, eliminates positive returns
    # when calculating risk.  To calculate it, we take the returns that are less
    # than the target (or Minimum Acceptable Returns (MAR)) returns and take the
    # differences of those to the target.  We sum the squares and divide by the
    # total number of returns to get a below-target semi-variance.

    # This is also useful for calculating semi-deviation by setting
    # MAR = mean(x)

    method = method[1] 

    if (is.vector(R)) {
        R = na.omit(R)

        if(!is.null(dim(MAR)))
            MAR = mean(checkData(MAR, method = "vector"))
        # we have to assume that Ra and a vector of Rf passed in for MAR both cover the same time period

        r = subset(R, R < MAR)

        switch(method,
            full   = {len = length(R)},
            subset = {len = length(r)} #previously length(R)
        ) # end switch
        result = sqrt(sum((r - MAR)^2)/len)
        return(result)
    }
    else {
        R = checkData(R, method = "matrix")
        result = apply(R, MARGIN = 2, DownsideDeviation, MAR = MAR, method = method)
        dim(result) = c(1,NCOL(R))
        colnames(result) = colnames(R)
        rownames(result) = paste("Downside Deviation (MAR = ", round(MAR*100,1),"%)", sep="")
        return(result)
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2010 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################