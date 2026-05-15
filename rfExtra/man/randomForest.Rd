\name{randomForest}
\alias{randomForest}

\title{Classification and Regression with Random Forest}

\description{
  This function is modified based on the \code{randomForest} package by Andy Liaw 
  and Matthew Wiener to include permutation-based group variable importance 
  measures.
}

\usage{
randomForest(x, y=NULL,  xtest=NULL, ytest=NULL, ntree=500,
             mtry=if (!is.null(y) && !is.factor(y))
             max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))),
             replace=TRUE, classwt=NULL, cutoff, strata,
             sampsize = if (replace) nrow(x) else ceiling(.632*nrow(x)),
             nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
             maxnodes = NULL,
             importance=FALSE, groupImp=FALSE, localImp=FALSE, nPerm=1,
             proximity, oob.prox=proximity,
             norm.votes=TRUE, do.trace=FALSE,
             keep.forest=!is.null(y) && is.null(xtest), corr.bias=FALSE,
             keep.inbag=FALSE, varGroups=NULL, ...)
}

\arguments{
  \item{x}{a data frame or a matrix of predictors.}
  \item{y}{A response vector.  If a factor, classification is assumed,
    otherwise regression is assumed.  If omitted, \code{randomForest}
    will run in unsupervised mode.}
  \item{xtest}{a data frame or matrix (like \code{x}) containing
    predictors for the test set.}
  \item{ytest}{response for the test set.}
  \item{ntree}{Number of trees to grow.  This should not be set to too
    small a number, to ensure that every input row gets predicted at
    least a few times. }
  \item{mtry}{Number of variables randomly sampled as candidates at each
    split.  Note that the default values are different for
    classification (sqrt(p) where p is number of variables in \code{x})
    and regression (p/3)}
  \item{replace}{Should sampling of cases be done with or without
    replacement?}
  \item{classwt}{Priors of the classes.  Need not add up to one.
    Ignored for regression.}
  \item{cutoff}{(Classification only)  A vector of length equal to
    number of classes.  The `winning' class for an observation is the
    one with the maximum ratio of proportion of votes to cutoff.
    Default is 1/k where k is the number of classes (i.e., majority vote
    wins).}
  \item{strata}{A (factor) variable that is used for stratified sampling.}
  \item{sampsize}{Size(s) of sample to draw.  For classification, if
    sampsize is a vector of the length the number of strata, then
    sampling is stratified by strata, and the elements of sampsize
    indicate the numbers to be drawn from the strata.}
  \item{nodesize}{Minimum size of terminal nodes.  Setting this number
    larger causes smaller trees to be grown (and thus take less time).
    Note that the default values are different for classification (1)
    and regression (5).}
  \item{maxnodes}{Maximum number of terminal nodes trees in the forest
	can have.  If not given, trees are grown to the maximum possible
	(subject to limits by \code{nodesize}).  If set larger than maximum
	possible, a warning is issued.}
  \item{importance}{Should importance of predictors be assessed? }
  \item{groupImp}{Should group importance of predictors be assessed? }
  \item{localImp}{Should casewise importance measure be computed?
    (Setting this to \code{TRUE} will override \code{importance}.) }
  \item{nPerm}{Number of times the OOB data are permuted per tree for
    assessing variable importance.  Number larger than 1 gives slightly
    more stable estimate, but not very effective.  Currently only
    implemented for regression.}
  \item{proximity}{Should proximity measure among the rows be
    calculated?}
  \item{oob.prox}{Should proximity be calculated only on ``out-of-bag''
    data?}
  \item{norm.votes}{If \code{TRUE} (default), the final result of votes
    are expressed as fractions.  If \code{FALSE}, raw vote counts are
    returned (useful for combining results from different runs).
    Ignored for regression.}
  \item{do.trace}{If set to \code{TRUE}, give a more verbose output as
    \code{randomForest} is run.  If set to some integer, then running
    output is printed for every \code{do.trace} trees.}
  \item{keep.forest}{If set to \code{FALSE}, the forest will not be
    retained in the output object.  If \code{xtest} is given, defaults
    to \code{FALSE}.}
  \item{corr.bias}{perform bias correction for regression?  Note:
    Experimental.  Use at your own risk.}
  \item{keep.inbag}{Should an \code{n} by \code{ntree} matrix be
    returned that keeps track of which samples are ``in-bag'' in which
    trees (but not how many times, if sampling with replacement)}
  \item{varGroups}{A length \code{p} (factor) vector representing variable 
  groups, where \code{p} is the number of predictors.}  
  \item{...}{optional parameters to be passed to the low level function.}
}

\value{
  An object of class \code{randomForest}, which is a list with the
  following components:

  \item{call}{the original call to \code{randomForest}}
  \item{type}{one of \code{regression}, \code{classification}, or
    \code{unsupervised}.}
  \item{predicted}{the predicted values of the input data based on
    out-of-bag samples.}
  \item{importance}{a matrix with \code{nclass} + 2 (for classification)
    or two (for regression) columns.  For classification, the first
    \code{nclass} columns are the class-specific measures computed as
    mean descrease in accuracy.  The \code{nclass} + 1st column is the
    mean descrease in accuracy over all classes.  The last column is the
    mean decrease in Gini index.  For Regression, the first column is
    the mean decrease in accuracy and the second the mean decrease in MSE.
    If \code{importance=FALSE}, the last measure is still returned as a
    vector.}
  \item{importanceSD}{The ``standard errors'' of the permutation-based
    importance measure.  For classification, a \code{p} by \code{nclass
      + 1} matrix corresponding to the first \code{nclass + 1} columns
    of the importance matrix.  For regression, a length \code{p} vector.}
  \item{groupImportance}{a matrix with \code{nclass} + 1 (for classification)
    or one (for regression) columns.  For classification, the first
    \code{nclass} columns are the class-specific measures computed as
    mean descrease in accuracy.  The \code{nclass} + 1st column is the
    mean descrease in accuracy over all classes.  For Regression, the first column is
    the mean increase in MSE.}
  \item{groupImportanceSD}{The ``standard errors'' of the permutation-based
    group importance measure.  For classification, a \code{grp} by \code{nclass
      + 1} matrix corresponding to the first \code{nclass + 1} columns
    of the importance matrix.  For regression, a length \code{grp} vector.}
  \item{localImp}{a p by n matrix containing the casewise importance
    measures, the [i,j] element of which is the importance of i-th
    variable on the j-th case. \code{NULL} if \code{localImp=FALSE}.}
  \item{ntree}{number of trees grown.}
  \item{mtry}{number of predictors sampled for spliting at each node.}
  \item{forest}{(a list that contains the entire forest; \code{NULL} if
    \code{randomForest} is run in unsupervised mode or if
    \code{keep.forest=FALSE}.}
  \item{err.rate}{(classification only) vector error rates of the
    prediction on the input data, the i-th element being the (OOB) error rate
    for all trees up to the i-th.}
  \item{confusion}{(classification only) the confusion matrix of the
    prediction (based on OOB data).}
  \item{votes}{(classification only) a matrix with one row for each
    input data point and one column for each class, giving the fraction
    or number of (OOB) `votes' from the random forest.}
  \item{oob.times}{number of times cases are `out-of-bag' (and thus used
    in computing OOB error estimate)}
  \item{proximity}{if \code{proximity=TRUE} when
    \code{randomForest} is called, a matrix of proximity measures among
    the input (based on the frequency that pairs of data points are in
    the same terminal nodes).}

  \item{mse}{(regression only) vector of mean square errors: sum of squared
    residuals divided by \code{n}.}
  \item{rsq}{(regression only) ``pseudo R-squared'': 1 - \code{mse} /
    Var(y).}
  \item{test}{if test set is given (through the \code{xtest} or additionally
  \code{ytest} arguments), this component is a list which contains the
  corresponding \code{predicted}, \code{err.rate}, \code{confusion},
  \code{votes} (for classification) or \code{predicted}, \code{mse} and
  \code{rsq} (for regression) for the test set.  If
  \code{proximity=TRUE}, there is also a component, \code{proximity},
  which contains the proximity among the test set as well as proximity
  between test and training data.}
}

\note{
  The modified \code{randomForest} function does not provide the 
  formula interface. 
  
  The function is modified based on original R and C code by Andy Liaw 
  and Matthew Wiener.
}

\references{
  Liaw, A. and Wiener, M. (2002), \emph{Classification and regression by randomForest}, R news 2(3), 
  18–22, \url{https://cran.r-project.org/web/packages/randomForest/randomForest.pdf}.

  Breiman, L. (2001), \emph{Random Forests}, Machine Learning 45(1),
  5-32.

  Breiman, L (2002), ``Manual On Setting Up, Using, And Understanding
  Random Forests V3.1'', \url{https://www.stat.berkeley.edu/~breiman/Using_random_forests_V3.1.pdf}.
}

\seealso{\code{\link{grpVarImpPlot}}}

\examples{
\dontrun{
# Load libraries
library(rfExtra)
library(randomForest)

# Load data set
data = read.table(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/
    parkinsons/parkinsons.data",
    sep = ",", header = TRUE, check.names = FALSE)
data = subset(data, select = -name)

## ==============
## Classification
## ==============

# Set response variable
response = "status"

# Split data
predictor_vars = data[, colnames(data) != response]
predictor_vars = na.roughfix(predictor_vars)
response_var = data[, colnames(data) == response]

# Downsample to the smaller class size
response_var = as.factor(response_var)
min_size = min(table(response_var))
num_classes = length(table(response_var))

set.seed(1) # Set seed for reproducible results
rf = rfExtra::randomForest(x = predictor_vars, y = response_var,
                           groupImp = TRUE, ntree = 100000, sampsize = rep(min_size, num_classes),
                           varGroups=factor(c(rep('Group 1', 5), rep('Group 2', 11), rep('Group 3', 6))))

# Get group variable importance
round(groupImportance(rf), 2)

## ==========
## Regression
## ==========

# Set response variable
response = "PPE"

# Split data
predictor_vars = data[, colnames(data) != response]
predictor_vars = na.roughfix(predictor_vars)
response_var = data[, colnames(data) == response]

set.seed(1) # Set seed for reproducible results
rf = rfExtra::randomForest(x = predictor_vars, y = response_var,
                           groupImp = TRUE, ntree = 100000, 
                           varGroups=factor(c(rep('Group 1', 5), rep('Group 2', 11), rep('Group 3', 6))))

# Get group variable importance
round(groupImportance(rf), 2)
}
}
\keyword{classification}
\keyword{regression}
\keyword{tree}
