
library(h2oEnsemble)  # Requires version >=0.1.8 of h2oEnsemble
library(h2o)
localH2O = h2o.init(nthreads = 4, max_mem_size = "6g") # Start an H2O cluster with nthreads = num cores on your machine


h20.train<-as.h2o(trainmice) 
train<-h20.train
h20.test<-as.h2o(testmice)  
test<-h20.test
train['Ticket']=NULL
test['Ticket']=NULL

head(train)
test['Survived']=0
y <- c("Survived","PassengerId","Cabin","Embarked")
x <- setdiff(names(h20.train), y)
y<-"Survived"
family <- "binomial"
#For binary classification, response should be a factor
train[,y] <- as.factor(train[,y])
test[,y] <- as.factor(test[,y])
# Specify the base learner library & the metalearner


# Random Grid Search (e.g. 120 second maximum)
# This is set to run fairly quickly, increase max_runtime_secs 
# or max_models to cover more of the hyperparameter space.
# Also, you can expand the hyperparameter space of each of the 
# algorithms by modifying the hyper param code below.

search_criteri <- list(strategy = "RandomDiscrete", 
                        max_runtime_secs = 1200)


# GBM Hyperparamters
learn_rate_opt <- c(0.01, 0.03) 
max_depth_opt <- c(3, 4, 5, 6, 9)
sample_rate_opt <- c(0.7, 0.8, 0.9, 1.0)
col_sample_rate_opt <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
ntrees<-c(100,90,150)
nfolds <- 4
hyper_params <- list(learn_rate = learn_rate_opt,
                     max_depth = max_depth_opt, 
                     sample_rate = sample_rate_opt,
                     col_sample_rate = col_sample_rate_opt,ntrees=ntrees)
gbm_grid <- h2o.grid("gbm", x = x, y = y,
                     training_frame = train,
                     seed = 1,
                     nfolds = nfolds,
                     fold_assignment = "Modulo",
                     keep_cross_validation_predictions = TRUE,
                     hyper_params = hyper_params
                     )
summary(gbm_grid)
gbm_models[1]


best_model <- h2o.getModel(gbm_grid@model_ids[[1]])
best_model_params<-(best_model@allparameters)
bestgbm_modelid<-best_model_params$model_id
ntreesgbm=best_model_params$ntrees
ntreesgbm=100
learnrategbm=best_model_params$learn_rate
max_depthgbm=best_model_params$max_depth
samplerategbm=best_model_params$sample_rate
colsamplerategbm=best_model_params$col_sample_rate

h2o.gbm.best <- function(..., ntrees = ntreesgbm,learn_rate=learnrategbm,sample_rate=samplerategbm,col_sample_rate = colsamplerategbm,max_depth = max_depthgbm, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees,max_depth = max_depth, col_sample_rate = col_sample_rate, learn_rate=learn_rate,sample_rate=sample_rate,seed = seed)

h2o.gbm.best <- function(..., ntrees = 100,learn_rate=0.03,sample_rate=1,col_sample_rate = 0.2,max_depth = 5, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees,max_depth = max_depth, col_sample_rate = col_sample_rate, learn_rate=learn_rate,sample_rate=sample_rate,seed = seed)



grid <- h2o.getGrid(grid_id="Grid_GBM_RTMP_sid_a15f_6_model_R_1460304015542_1_model_59","auc",decreasing=FALSE)


# RF Hyperparamters
mtries_opt <- 5:20 
max_depth_opt <- c(5, 10, 15, 20, 25)
sample_rate_opt <- c(0.7, 0.8, 0.9, 1.0)
col_sample_rate_per_tree_opt <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
hyper_params <- list(mtries = mtries_opt,
                     max_depth = max_depth_opt,
                     sample_rate = sample_rate_opt
                     ,ntrees=ntrees
                  )

rf_grid <- h2o.grid("randomForest", x = x, y = y,
                    training_frame = train,
                    
                    seed = 1,
                    nfolds = nfolds,
                    fold_assignment = "Modulo",
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

# Deeplearning Hyperparamters
activation_opt <- c("Rectifier", "RectifierWithDropout", 
                    "Maxout", "MaxoutWithDropout") 
hidden_opt <- list(c(80,30), c(50,35), c(50,30,10))
l1_opt <- c(0, 1e-3, 1e-5)
l2_opt <- c(0, 1e-3, 1e-5)
epochs = c(20,100)
hyper_params <- list(activation = activation_opt,
                     hidden = hidden_opt,
                     l1 = l1_opt,
                     l2 = l2_opt,
                     epochs =epochs)
gc()

dl_grid <- h2o.grid("deeplearning", x = x, y = y,
                    training_frame = train,
                   
                    seed = 1,
                    nfolds = nfolds,
                    fold_assignment = "Modulo",
                    keep_cross_validation_predictions = TRUE,                    
                    hyper_params = hyper_params)

summary(dl_grid)

best_modeldl <- h2o.getModel(dl_grid@model_ids[[1]])
best_model_paramsdl<-(best_modeldl@allparameters)
bestgdl_modelid<-best_model_paramsdl$model_id
bestdl_act=best_model_paramsdl$activation
bestdl_nfolds=best_model_paramsdl$nfolds
bestdl_epochs=best_model_paramsdl$epochs
bestdl_l1=best_model_paramsdl$l1
bestdl_l2=best_model_paramsdl$l2
bestdl_hidden=best_model_paramsdl$hidden

h2o.deeplearning.4 <- function(..., hidden = bestdl_hidden, activation = bestdl_act, epochs = bestdl_epochs,l2=bestdl_l2,l1=bestdl_l2,nfolds=bestdl_nfolds, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation,l1=l1,l2=l2,epochs = epochs, seed = seed)


dl_models.grid <- h2o.getGrid(grid_id = "Grid_DeepLearning_RTMP_sid_a15f_6_model_R_1460304015542_1275693", sort_by = "mse")
print(dl_models.grid)



# GLM Hyperparamters
alpha_opt <- seq(0,1,0.1)
lambda_opt <- c(0,1e-7,1e-5,1e-3,1e-1)
hyper_params <- list(alpha = alpha_opt,
                     lambda = lambda_opt)

glm_grid <- h2o.grid("glm", x = x, y = y,
                     training_frame = train,
                     family = "binomial",
                     nfolds = nfolds,
                     fold_assignment = "Modulo",
                     keep_cross_validation_predictions = TRUE,                    
                     hyper_params = hyper_params
                     )
glm_models <- lapply(glm_grid@model_ids, function(model_id) h2o.getModel(model_id))
gc()

# Create a list of all the base models
models <- c(gbm_models, rf_models, dl_models)

# Specify a defalt GLM as the metalearner
h2o.glm_nn <- function(..., non_negative = TRUE) {
  h2o.glm.wrapper(..., non_negative = non_negative)
}
metalearner <- "h2o.gbm.wrapper"


learner <- c("h2o.randomForest.4","h2o.gbm.best")
gc()
fit <- h2o.ensemble(x = x, y = y,
                    training_frame = train,
                    family = "binomial",
                    learner = learner,
                    metalearner = metalearner,
                    cvControl = list(V = 10, shuffle = TRUE))



h2o.ensemble_performance(fit,train)
test['Survived']=NULL
pp <- predict(fit, test)
head(pp$pred,15)

predictions <- as.data.frame((pp$pred)[,1]) #third column, p1 is P(Y==1)

submit <- data.frame(PassengerId = testmice$PassengerId, Survived = predictions)
write.csv(submit, file = "gridsearchensembblewithgbmandrf.csv", row.names = FALSE)



#0.80383,
h2o.shutdown()

