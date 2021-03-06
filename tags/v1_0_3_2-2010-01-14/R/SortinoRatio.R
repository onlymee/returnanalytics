SortinoRatio <-
function (R, MAR = 0)
{ # @author Brian G. Peterson
  # modified from function by Sankalp Upadhyay <sankalp.upadhyay [at] gmail [dot] com> with permission

    # Description:
    # Sortino proposed to better account for skill and excess peRformance
    # by using only downside semivariance as the measure of risk.

    # R     return vector
    # MAR   minimum acceptable return
    # Function:
    R = checkData(R)

    sr <-function (R, MAR)
    {
        SR = mean(Return.excess(R, MAR), na.rm=TRUE)/DownsideDeviation(R, MAR)
        SR
    }

    result = apply(R, MARGIN = 2, sr, MAR = MAR)
    dim(result) = c(1,NCOL(R))
    colnames(result) = colnames(R)
    rownames(result) = paste("Sortino Ratio (MAR = ", round(MAR,1),"%)", sep="")
    return (result)
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