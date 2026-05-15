groupImportance <- function(x, class=NULL, scale=TRUE) {
    if (!inherits(x, "randomForest"))
        stop("x is not of class randomForest")
    classRF <- x$type != "regression"
    hasImp <- !is.null(dim(x$groupImportance)) || ncol(x$groupImportance) == 1
    imp <- x$groupImportance
    if (scale) {
        SD <- x$groupImportanceSD
        imp[, ] <-
            imp[, , drop=FALSE] /
                ifelse(SD < .Machine$double.eps, 1, SD)
    }
    if (is.null(class)) {
        ## The average decrease in accuracy measure:
        imp <- imp[, ncol(imp), drop=FALSE]
    } else {
        whichCol <- if (classRF) match(class, colnames(imp)) else 1
        if (is.na(whichCol)) stop(paste("Class", class, "not found."))
        imp <- imp[, whichCol, drop=FALSE]
    }
    imp
}
