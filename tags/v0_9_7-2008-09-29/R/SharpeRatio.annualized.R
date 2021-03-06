`SharpeRatio.annualized` <-
function (Ra, rf = 0, scale = 12)
{ # @author Peter Carl

    # DESCRIPTION:

    # Using an annualized Sharpe Ratio is useful for comparison.  The annualized
    # Sharpe ratio is computed by dividing the annualized mean monthly excess
    # return by the annualized monthly standard deviation of excess return.

    # @todo: monthly standard deviation of ***excess*** return

    # Inputs:
    # R: in this case, the function anticipates having a return stream as input,
    #     rather than prices.
    # rf: the risk free rate MUST be in the same periodicity as the data going in.

    # FUNCTION:

    Ra = checkData(Ra, method="zoo")
    if(!is.null(dim(rf)))
        rf = checkData(rf, method = "zoo")
    Ra.excess = Return.excess(Ra, rf)
    return((Return.annualized(Ra.excess, scale = scale))/StdDev.annualized(Ra, scale = scale))

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: SharpeRatio.annualized.R,v 1.7 2008-06-02 16:05:19 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.6  2007/08/16 14:12:40  peter
# - added checkData for rf
#
# Revision 1.5  2007/04/09 03:33:19  peter
# - uses checkData
# - uses Return.excess
#
# Revision 1.4  2007/03/14 00:54:06  brian
# - updates to parameters for standardization
#
# Revision 1.3  2007/03/12 15:34:43  brian
# - add equations to documentation
# - standardize on Ra for Returns of asset
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################