grpVarImpPlot <- function(x, sort=TRUE,
                          n.var=min(30, nrow(x$groupImportance)),
                          class=NULL, scale=TRUE, 
                          main=deparse(substitute(x)), ...) {
    if (!inherits(x, "randomForest"))
        stop("This function only works for objects of class `randomForest'")
    imp <- groupImportance(x, class=class, scale=scale, ...)
    ## If there are more than one column, just use the last one column.
    if (ncol(imp) > 1) imp <- imp[, -(1:(ncol(imp) - 1)), drop=FALSE]
    ord <- if (sort) rev(order(imp[,1],
                               decreasing=TRUE)[1:n.var]) else 1:n.var
    xmin <- min(imp[ord, 1])
    dotchart(imp[ord,1], xlab=colnames(imp)[1], ylab="",
             main=main,
             xlim=c(xmin, max(imp[,1])), ...)
    invisible(imp)
}
