\name{combine}
\alias{combine}

\title{Combine Ensembles of Trees}

\description{
Combine two more more ensembles of trees into one. Unlike the \code{randomForest} 
package, the returned components \code{importanceSD} and \code{groupImportanceSD} will
be standard errors rather than standard deviations. If \code{eval} set to \code{TRUE}, 
the \code{confusion}, \code{err.rate}, \code{mse} and \code{rsq} components will be 
recorded only for the combined object. The components will be \code{NULL} for the 
\code{test} compnent, if exist.
}

\usage{
combine(..., eval=TRUE)
}

\arguments{
  \item{...}{Two or more objects of class \code{randomForest}, to be
    combined into one.}
  \item{eval}{Whether to calculate confusion and error rate for classification, 
  or mean squared error and R-squared for regression. Default is \code{TRUE}.}
}

\value{
An object of class \code{randomForest}.
}

\seealso{\code{\link{randomForest}}}

\references{
 Liaw, A. and Wiener, M. (2002), \emph{Classification and regression by randomForest}, R news 2(3), 18–22.
}

\examples{
\dontrun{
# Below are classification and regression examples that build random forest models 
# with parallel computing and use the function to combine multiple results.

# Load libraries
library(rfExtra)
library(randomForest)
library(parallel)

# Set number of parallel streams and number of trees for each stream
num_jobs = 8
ntree_per_job = ceiling(100000 / num_jobs)

# Load data set
data = read.table(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/
    parkinsons/parkinsons.data",
    sep = ",", header = TRUE, check.names = FALSE)
data = subset(data, select = -name)

## Classification:

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

# Set seed for parallel environment
set.seed(1, "L'Ecuyer-CMRG")

# Train random forest models in parallel
rfExtra_res = mclapply(1:num_jobs, function(num_jobs) rfExtra::randomForest(x = predictor_vars,
    y = response_var, ntree = ntree_per_job, importance = TRUE, groupImp = TRUE,
    sampsize = rep(min_size, num_classes), varGroups = factor(c(rep("Group 1", 5), 
    rep("Group 2", 11), rep("Group 3", 6)))))

# Combine results
parallel_rf = do.call(rfExtra::combine, rfExtra_res)

# Get error rate
parallel_rf$err.rate

# Get confusion matrix
parallel_rf$confusion

## Regression:

# Set response variable
response = "PPE"

# Split data
predictor_vars = data[, colnames(data) != response]
predictor_vars = na.roughfix(predictor_vars)
response_var = data[, colnames(data) == response]

# Set seed for parallel environment
set.seed(1, "L'Ecuyer-CMRG")

# Train random forest models in parallel
rfExtra_res = mclapply(1:num_jobs, function(num_jobs) rfExtra::randomForest(x = predictor_vars,
    y = response_var, ntree = ntree_per_job, importance = TRUE, groupImp = TRUE,
    varGroups = factor(c(rep("Group 1", 5), rep("Group 2", 11), rep("Group 3", 6)))))

# Combine results
parallel_rf = do.call(rfExtra::combine, rfExtra_res)

# Get mean squared error
parallel_rf$mse

# Get R squared
parallel_rf$rsq
}
}
\keyword{classification}
\keyword{regression}

