## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(tidy.opts=list(width.cutoff=50), tidy=TRUE, echo=TRUE, 
                      collapse=TRUE, warning=FALSE, message=FALSE, eval=TRUE,
                      fig.ext='png', dpi=600)

## -----------------------------------------------------------------------------
# Install package from source
# install.packages('~/Downloads/rfExtra_0.0.0.9000.tar.gz', type='source', repos=NULL, force=TRUE)

# Load libraries
library(randomForest)
library(rfExtra)
library(parallel)

## -----------------------------------------------------------------------------
# Load data set
data = read.table(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/parkinsons/parkinsons.data",
    sep = ",", header = TRUE, check.names = FALSE)
data = subset(data, select = -name)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
# Set seed for reproducible results
set.seed(1) 

# Train random forest
rf = rfExtra::randomForest(x = predictor_vars, y = response_var, importance = TRUE,
                           groupImp = TRUE, ntree = 100000, sampsize = rep(min_size, num_classes),
                           varGroups=factor(c(rep('Group 1', 5), rep('Group 2', 11), rep('Group 3', 6))))

# Plot group variable importance
grpVarImpPlot(rf)

## -----------------------------------------------------------------------------
# Set number of parallel streams and number of trees for each stream
num_jobs = 8
ntree_per_job = ceiling(100000 / num_jobs)

# Set seed for parallel environment
set.seed(1, "L'Ecuyer-CMRG") 

# Train random forest models in parallel
res = mclapply(1:num_jobs, 
               function(num_jobs) rfExtra::randomForest(x = predictor_vars, y = response_var, 
                                                        ntree = ntree_per_job, importance=TRUE,
                                                        groupImp = TRUE,
                                                        sampsize = rep(min_size, num_classes),
                                                        varGroups = factor(c(rep('Group 1', 5), rep('Group 2', 11), 
                                                                             rep('Group 3', 6)))))

# Combine results
par_rf = do.call(rfExtra::combine, res)

## -----------------------------------------------------------------------------
rf$confusion
par_rf$confusion
rf$err.rate[length(rf$err.rate)]
par_rf$err.rate

## -----------------------------------------------------------------------------
set.seed(1)
randomForest_rf = randomForest::randomForest(x = predictor_vars, y = response_var,
                                             importance = TRUE, ntree = 100000, sampsize = rep(min_size, num_classes))

## -----------------------------------------------------------------------------
set.seed(1)

# Each group only has one variable
groups = factor(colnames(predictor_vars), levels=colnames(predictor_vars))

rfExtra_grpImp = rfExtra::randomForest(x = predictor_vars, y = response_var,
                                       importance = FALSE, groupImp = TRUE, ntree = 100000, 
                                       sampsize = rep(min_size, num_classes),
                                       varGroups=groups)

## -----------------------------------------------------------------------------
set.seed(1)
rfExtra_no_grpImp = rfExtra::randomForest(x = predictor_vars, y = response_var,
                                             importance = TRUE, groupImp = FALSE, ntree = 100000, 
                                             sampsize = rep(min_size, num_classes))

## -----------------------------------------------------------------------------
# Set number of parallel streams and number of trees for each stream
num_jobs = 8
ntree_per_job = ceiling(100000 / num_jobs)

set.seed(1, "L'Ecuyer-CMRG")
randomForest_res = mclapply(1:num_jobs, 
                            function(num_jobs) randomForest::randomForest(x = predictor_vars, y = response_var, 
                                                                          ntree = ntree_per_job, importance = TRUE,
                                                                          sampsize = rep(min_size, num_classes)))

par_rf_randomForest_cbne = do.call(randomForest::combine, randomForest_res)

## -----------------------------------------------------------------------------
par_rf_rfExtra_cbne = do.call(rfExtra::combine, randomForest_res)

## -----------------------------------------------------------------------------
set.seed(1, "L'Ecuyer-CMRG")
rfExtra_res = mclapply(1:num_jobs, 
                       function(num_jobs) rfExtra::randomForest(x = predictor_vars, y = response_var, 
                                                                ntree = ntree_per_job, importance=FALSE,
                                                                groupImp = TRUE,
                                                                sampsize = rep(min_size, num_classes),
                                                                varGroups = groups))

par_rf_rfExtra_grpImp = do.call(rfExtra::combine, rfExtra_res)

## ---- fig.width=12, fig.height=8----------------------------------------------
par(mfrow = c(2, 3), mar = c(2, 2, 2, 2))
varImpPlot(randomForest_rf, type = 1)
grpVarImpPlot(rfExtra_grpImp)
varImpPlot(rfExtra_no_grpImp, type = 1)
varImpPlot(par_rf_randomForest_cbne, type = 1)
varImpPlot(par_rf_rfExtra_cbne, type = 1)
grpVarImpPlot(par_rf_rfExtra_grpImp)

## -----------------------------------------------------------------------------
# Compare importance measures of randomForest_rf, rfExtra_grpImp, and rfExtra_no_grpImp
identical(importance(randomForest_rf, type=1), groupImportance(rfExtra_grpImp))
identical(importance(randomForest_rf, type=1), importance(rfExtra_no_grpImp, type=1))

## -----------------------------------------------------------------------------
# Compare importance measures of par_rf_rfExtra_cbne and par_rf_rfExtra_grpImp
identical(importance(par_rf_rfExtra_cbne, type=1), groupImportance(par_rf_rfExtra_grpImp))

## -----------------------------------------------------------------------------
# Set response variable
response = "PPE"

# Split data
predictor_vars = data[, colnames(data) != response]
predictor_vars = na.roughfix(predictor_vars)
response_var = data[, colnames(data) == response]

## -----------------------------------------------------------------------------
# Set seed for reproducible results
set.seed(1)

# Train random forest
rf = rfExtra::randomForest(x = predictor_vars, y = response_var, importance = TRUE,
                           groupImp = TRUE, ntree = 100000, 
                           varGroups=factor(c(rep('Group 1', 5), rep('Group 2', 11), rep('Group 3', 6))))

# Plot group variable importance
grpVarImpPlot(rf)

## -----------------------------------------------------------------------------
# Set number of parallel streams and number of trees for each stream
num_jobs = 8
ntree_per_job = ceiling(100000 / num_jobs)

# Set seed for parallel environment
set.seed(1, "L'Ecuyer-CMRG") 

# Train random forest models in parallel
res = mclapply(1:num_jobs, 
               function(num_jobs) rfExtra::randomForest(x = predictor_vars, y = response_var, 
                                                        ntree = ntree_per_job, importance=TRUE,
                                                        groupImp = TRUE,
                                                        varGroups = factor(c(rep('Group 1', 5), rep('Group 2', 11), 
                                                                             rep('Group 3', 6)))))

# Combine results
par_rf = do.call(rfExtra::combine, res)

## -----------------------------------------------------------------------------
rf$mse[length(rf$mse)]
par_rf$mse
rf$rsq[length(rf$rsq)]
par_rf$rsq

## -----------------------------------------------------------------------------
set.seed(1)
randomForest_rf = randomForest::randomForest(x = predictor_vars, y = response_var,
                                             importance = TRUE, ntree = 100000)

## -----------------------------------------------------------------------------
set.seed(1)

# Each group only has one variable
groups = factor(colnames(predictor_vars), levels=colnames(predictor_vars))

rfExtra_grpImp = rfExtra::randomForest(x = predictor_vars, y = response_var,
                                       importance = FALSE, groupImp = TRUE, ntree = 100000, 
                                       varGroups=groups)

## -----------------------------------------------------------------------------
set.seed(1)
rfExtra_no_grpImp = rfExtra::randomForest(x = predictor_vars, y = response_var,
                                          importance = TRUE, groupImp = FALSE, ntree = 100000)

## -----------------------------------------------------------------------------
# Set number of parallel streams and number of trees for each stream
num_jobs = 8
ntree_per_job = ceiling(100000 / num_jobs)

set.seed(1, "L'Ecuyer-CMRG")
randomForest_res = mclapply(1:num_jobs, 
                            function(num_jobs) randomForest::randomForest(x = predictor_vars, y = response_var, 
                                                                          ntree = ntree_per_job, importance = TRUE))

par_rf_randomForest_cbne = do.call(randomForest::combine, randomForest_res)

## -----------------------------------------------------------------------------
par_rf_rfExtra_cbne = do.call(rfExtra::combine, randomForest_res)

## -----------------------------------------------------------------------------
set.seed(1, "L'Ecuyer-CMRG")
rfExtra_res = mclapply(1:num_jobs, 
                       function(num_jobs) rfExtra::randomForest(x = predictor_vars, y = response_var, 
                                                                ntree = ntree_per_job, importance=FALSE,
                                                                groupImp = TRUE,
                                                                varGroups = groups))

par_rf_rfExtra_grpImp = do.call(rfExtra::combine, rfExtra_res)

## ---- fig.width=12, fig.height=8----------------------------------------------
par(mfrow = c(2, 3), mar = c(2, 2, 2, 2))
varImpPlot(randomForest_rf, type = 1)
grpVarImpPlot(rfExtra_grpImp)
varImpPlot(rfExtra_no_grpImp, type = 1)
varImpPlot(par_rf_randomForest_cbne, type = 1)
varImpPlot(par_rf_rfExtra_cbne, type = 1)
grpVarImpPlot(par_rf_rfExtra_grpImp)

## -----------------------------------------------------------------------------
# Compare importance measures of randomForest_rf, rfExtra_grpImp, and rfExtra_no_grpImp
identical(importance(randomForest_rf, type=1), groupImportance(rfExtra_grpImp))
identical(importance(randomForest_rf, type=1), importance(rfExtra_no_grpImp, type=1))

## -----------------------------------------------------------------------------
# Compare importance measures of par_rf_rfExtra_cbne and par_rf_rfExtra_grpImp
identical(importance(par_rf_rfExtra_cbne, type=1), groupImportance(par_rf_rfExtra_grpImp))

