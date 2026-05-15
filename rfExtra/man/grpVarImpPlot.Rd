\name{grpVarImpPlot}
\alias{grpVarImpPlot}

\title{Group Variable Importance Plot}

\description{
  Dotchart of group variable importance as measured by the \code{randomForest} 
  function in this package (\code{rfExtra::randomForest()}).
}

\usage{
grpVarImpPlot(x, sort=TRUE, n.var=min(30, nrow(x$groupImportance)),
              class=NULL, scale=TRUE, 
              main=deparse(substitute(x)), ...)
}

\arguments{
  \item{x}{An object of class \code{randomForest}.}
  \item{sort}{Should the variables be sorted in decreasing order of
    importance?}
  \item{n.var}{How many variables to show? (Ignored if
    \code{sort=FALSE}.)}
  \item{class}{For classification, which class to be shown in the plot.
  Set class to \code{NULL} to get results over all classes.}
  \item{scale}{Whether to scale permutation-based variable importance 
  (divided by the standard error).}
  \item{main}{Plot title.}
  \item{...}{Other graphical parameters to be passed on to
    \code{\link{dotchart}}.}
}

\value{
  The group variable importance plot.
}

\seealso{
  \code{\link{randomForest}}, \code{\link{groupImportance}}
}

\references{
 Liaw, A. and Wiener, M. (2002), \emph{Classification and regression by randomForest}, R news 2(3), 18–22.
}

\examples{
\dontrun{
# Load library
library(rfExtra)

# Load data set
data = read.table(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/
    parkinsons/parkinsons.data",
    sep = ",", header = TRUE, check.names = FALSE)
data = subset(data, select = -name)

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
grpVarImpPlot(rf)
}
}
\keyword{classification}
\keyword{regression}

