`chart.BarVaR` <-
function (R, width = 0, gap = 12, methods = c("none", "ModifiedVaR","GaussianVaR","HistoricalVaR", "StdDev"), clean = c("none", "boudt"), reference.grid = TRUE, xaxis = TRUE, main = "Title", ylab="Value", xlab="Date", date.format = "%m/%y", xlim = NA, ylim = NA, lwd = 1, colorset = 1:12, p=.99, lty = c(1,2,4,5,6), all = FALSE, show.clean = FALSE, show.horizontal = FALSE, show.symmetric = FALSE, legend.loc="bottomleft", ypad=0, legend.cex = 0.8, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of monthly returns in a bar chart.  This is
    # a difficult enough graph to read that it doesn't get much use.  Still,
    # it is useful for viewing a single set of data.

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # width: periods specified for rolling-period calculations

    # Outputs:
    # A timeseries bar chart of the data series
    # Metrics with width=0 are calculated from the start of the timeseries

    # FUNCTION:

    # Transform input data to a timeseries object
    x = checkData(R, method = "zoo")
    #x = na.omit(x)

    # Set up dimensions and labels
    columns = ncol(x)
    rows = nrow(x)
    columnnames = colnames(x)
    rownames = time(x)
    legend.txt = NULL
    symmetric = NULL
    risk.line=TRUE
    # Re-format the dates for the xaxis
#     rownames = format(strptime(as.Date(rownames),format = "%Y-%m-%d"), date.format)

    time(x) = as.Date(time(x)) # this is here because merge.zoo is not behaving as expected when date formats are not consistent
    if(methods[1]=="none"){
        methods=NULL
        risk.line=FALSE
    }

    colors = colorRamp(c(colorset[1],"white"))
    if(length(methods)>1){
        columns = 1 # if there's more than one method specified, then we'll ignore columns other than the first
        colorset = c(colorset[1], rgb(colors(.25),max=255), rgb(colors(.5),max=255), rgb(colors(.75),max=255))
    }
    clean = clean[1]

    risk = zoo(NA,order.by=time(x))
    column.risk = zoo(0,order.by=time(x))

    if (!all)
        columns = 1

    bar.color = colorset[1]
    if (show.clean){
        bar.color = rgb(colors(.75),max=255)
    }

    x.orig = x

    if(show.clean){
        x = Return.clean(x, method=clean) 
    }

    if(risk.line){
        for(column in 1:columns) {
            for(method in methods) {
                switch(method,
                    StdDev = {
                        symmetric = c(symmetric, TRUE)
                        if(width > 0){
                            column.risk = apply.rolling(na.omit(x.orig[,column,drop=FALSE]), width = width, FUN = "sd")
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Rolling ",width,"-month Std Dev",sep=""))
                        }
                        else {
                            column.risk = apply.fromstart(na.omit(x.orig[,column,drop=FALSE]), gap = gap, FUN = "sd")
                            if(column==1)
                                legend.txt = c(legend.txt, "Std Dev")
                        }
                    },
                    GaussianVaR = {
                        symmetric = c(symmetric, TRUE)
                        if(width > 0) {
                            column.risk = apply.rolling(na.omit(x.orig[,column,drop=FALSE]), width = width, FUN = "VaR.CornishFisher", p = p, modified = FALSE, clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Rolling ",width,"-Month Gaussian VaR (1 Mo, ",p*100,"%)",sep=""))
                        }
                        else {
                            column.risk = apply.fromstart(na.omit(x.orig[,column,drop=FALSE]), gap = gap, FUN = "VaR.CornishFisher", p = p, modified = FALSE, clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Gaussian VaR (1 Mo, ",p*100,"%)",sep=""))
                        }
                    },
                    ModifiedVaR = {
                        symmetric = c(symmetric, FALSE)
                        if(width > 0) {
                            column.risk = apply.rolling(na.omit(x.orig[,column,drop=FALSE]), width = width, FUN = "VaR.CornishFisher", p = p, modified = TRUE, clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Rolling ",width,"-Month Modified VaR (1 Mo, ",p*100,"%)",sep=""))
                        }
                        else {
                            column.risk = apply.fromstart(na.omit(x.orig[,column,drop=FALSE]), gap = gap, FUN = "VaR.CornishFisher", p = p, modified = TRUE, clean=clean)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Modified VaR (1 Mo, ",p*100,"%)",sep=""))
                        }
                    },
                    HistoricalVaR = {
                        symmetric = c(symmetric, FALSE)
                        if(width > 0) {
                            column.risk = -1* apply.rolling(na.omit(x.orig[,column,drop=FALSE]), width = width, FUN = "quantile", probs = (1-p)) #hVaR = quantile(x,probs=.01)
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Rolling ",width,"-Month Historical VaR (1 Mo, ",p*100,"%)",sep=""))
                        }
                        else {
                            column.risk = -1* apply.fromstart(na.omit(x.orig[,column,drop=FALSE]), gap = gap, FUN = "quantile", probs = (1-p))
                            if(column==1)
                                legend.txt = c(legend.txt, paste("Historical VaR (1 Mo, ",p*100,"%)",sep=""))
                        }
                    }
                ) # end switch

                risk = merge.zoo(risk,column.risk)
            } # end method loop
        } # end column loop
    }
    else {
        risk = 0
        legend.txt = ""
    }
    if(is.na(ylim[1])){
        ylim = range(c(na.omit(as.vector(x.orig[,1])), na.omit(as.vector(risk)), -na.omit(as.vector(risk))))
        ylim = c(ylim[1]-ypad,ylim[2]) # pad the bottom of the chart for the legend
    }

    chart.TimeSeries(x.orig[,1, drop=FALSE], type = "h", col = bar.color, legend.loc = NULL, ylim = ylim, reference.grid = reference.grid, xaxis = xaxis, main = main, ylab = ylab, xlab = xlab, lwd = lwd, lend="butt", ...)

    if(show.clean) {
        lines(1:rows, x[,1, drop=FALSE], type="h", col=colorset[1], lwd = lwd, lend="butt")
    }

#     symmetric = symmetric[-1]
# add risk.columns here, since we'll want multiple lines when we have multiple methods
# if we have multiple methods, we'll want multiple lty's

    if(risk.line){
        risk.columns = ncol(risk)
        if(length(lty)==1)
            lty = rep(lty, risk.columns)
        for(column in (risk.columns):2) {
            if (show.symmetric && symmetric[column-1]){
                lines(1:rows, risk[,column], col = colorset[column-1], lwd = 1, type = "l", lty=lty[column-1])
            }
        }
        for(column in (risk.columns):2) {
            lines(1:rows, -risk[,column], col = colorset[column-1], lwd = 1, type = "l", lty=lty[column-1])
            if(show.horizontal)
                lines(1:rows, rep(-tail(risk[,2],1),rows), col = colorset[1], lwd=1, type="l", lty=1)
        }
    }

    if(legend.txt[1] != "")
        legend(legend.loc, inset = 0.02, text.col = colorset, col = colorset, cex = legend.cex, border.col = "grey", lwd = 1, lty=lty, bty = "n", legend = legend.txt, horiz=TRUE)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.BarVaR.R,v 1.17 2008-08-19 03:27:17 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.15  2008-08-11 14:06:15  peter
# - added parameter 'clean' to specify data cleaning method for risk estimation
# - added parameter 'show.clean' to show cleaned returns overlaid on original data
# - added parameter 'show.horizontal' to show exceedences to most recent risk value
#
# Revision 1.14  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.13  2007/08/20 21:04:58  peter
# - added as.Date because merge.zoo is not behaving as expected when date
#   formats are not consistent
#
# Revision 1.12  2007/06/25 04:15:59  peter
# - made gap default 12 (months)
#
# Revision 1.11  2007/06/18 03:33:07  brian
# - use switch for method argument, more efficient
#
# Revision 1.10  2007/04/21 14:07:25  peter
# - added 'all' flag: when F only draws first column
#
# Revision 1.9  2007/04/21 01:06:01  peter
# - creates risk lines for each column of data
#
# Revision 1.8  2007/04/13 22:45:18  peter
# - changed how na.omit is applied
#
# Revision 1.7  2007/04/02 21:52:46  peter
# - added removal of NA's
#
# Revision 1.6  2007/03/21 20:49:04  peter
# - fixed issue with ylim when passed a matrix-like object
#
# Revision 1.5  2007/03/20 10:44:46  brian
# - change F to FALSE to pass R CMD check
#
# Revision 1.4  2007/03/20 03:27:00  peter
# - uses apply.rolling and apply.fromstart to calculate risk lines
#
# Revision 1.3  2007/02/26 13:32:37  brian
# - change method VaR to pass "R CMD check"
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################
