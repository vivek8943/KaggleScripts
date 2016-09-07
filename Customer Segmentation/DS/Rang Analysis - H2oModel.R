setwd('~/Desktop/KaggleScripts/Rang/DS')

load('RTImputation_TrainAndTest.Rd')


#Factor Variables
factor_vars <- c('Cust_status','Trans24','Trans25','Trans26','Trans27','Promotion37','Active_Customer')
training.data[factor_vars] <- lapply(training.data[factor_vars], function(x) as.factor(x))
factor_vars <- c('Cust_status','Trans24','Trans25','Trans26','Trans27','Promotion37')
testing.data[factor_vars] <- lapply(testing.data[factor_vars], function(x) as.factor(x))



#required Libraries
library(h2o)

# Start h2o ,decide min and max memory sizes,number of cpu cores to be used (-1 means all cores)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,
                     max_mem_size = '8g', min_mem_size = '4g', nthreads = -1)
#Training data
training.data.hex<-as.h2o(training.data)

H2ORandomForest <- h2o.randomForest(x = 2:256, y = 257 ,  training_frame = training.data.hex,
                                    model_id='H2ORandomForest',max_depth=60,ntrees = 100,stopping_metric ="MSE",stopping_tolerance = 0.001)


#predict values for the Testing dataset 
testing.data.hex<-as.h2o(testing.data[,2:256])

x = 2:256
y = 257 
# RF Hyperparamters
mtries_opt <- 5:10 
ntrees=c(100,300)
max_depth_opt <- c(50, 20, 75)
sample_rate_opt <- c(0.7, 0.8)
col_sample_rate_per_tree_opt <- c(0.2, 0.3,  0.8)
hyper_params <- list(mtries = mtries_opt,
                     max_depth = max_depth_opt,
                     sample_rate = sample_rate_opt
                     ,ntrees=ntrees
)

rf_grid <- h2o.grid("randomForest", x = x, y = y,
                    training_frame = training.data.hex,
                    
                    seed = 1,
                    
                  
                    keep_cross_validation_predictions = TRUE,                    
                    hyper_params = hyper_params
)
best_modelrf <- h2o.getModel(rf_grid@model_ids[[1]])
best_model_paramsrf<-(best_modelrf@allparameters)
bestgrf_modelid<-best_model_paramsrf$model_id
ntreesrf=best_model_paramsrf$ntrees

max_depthrf=best_model_paramsrf$max_depth
samplerategbm=best_model_paramsrf$sample_rate
mtriesrf=best_model_paramsrf$mtries
h2o.randomForest.4 <- function(..., ntrees =ntreesrf,sample_rate=samplerategbm,mtries=mtriesrf, max_depth=max_depthrf, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, sample_rate = sample_rate,max_depth = max_depth, seed = seed,mtries = mtries)

learner <- c("h2o.randomForest.4","h2o.gbm.wrapper")
gc()
metalearner <- "h2o.gbm.wrapper"

fit <- h2o.ensemble(x = x, y = y,
                    training_frame = training.data.hex,
                    family = "binomial",
                    learner = learner,
                    metalearner = metalearner,
                    cvControl = list(V = 5, shuffle = TRUE))

pp <- predict(fit, testing.data.hex)
a=pp$pred
ppred=as.data.frame(a['predict'])
write.csv(ppred, file = "abc.csv", row.names = FALSE)

