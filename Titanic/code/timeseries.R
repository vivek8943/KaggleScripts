setwd("~/Desktop/KaggleScripts/FOGNETS/data/")
train <- read.csv("train_2h.csv")

str(train)
test <- read.csv("test_2h.csv")
subm <- read.csv("submission_format.csv")
colnames.train=colnames(train)
remove<-c("X","yield")
colnames.train.x=setdiff(colnames.train, remove)
colnames.train.y="yield"

# Impute STarts! 
library(imputeTS)
trainimp=train
trainimp$X=NULL
#train$X <-  (as.POSIXlt(train$X, format = "%Y-%m-%d %H:%M:%S"))
str(trainimp)

trainimp$percip_mm=as.numeric(trainimp$percip_mm)
trainimp$humidity=as.numeric(trainimp$humidity)
trainimp$temp=as.numeric(trainimp$temp)
trainimp$leafwet450_min=as.numeric(trainimp$leafwet450_min)
trainimp$leafwet460_min=as.numeric(trainimp$leafwet460_min)
trainimp$leafwet_lwscnt=as.numeric(trainimp$leafwet_lwscnt)
trainimp$gusts_ms=as.numeric(trainimp$gusts_ms)

trainimp$wind_dir=as.numeric(trainimp$wind_dir)
trainimp$wind_ms=as.numeric(trainimp$wind_ms)
trainimp$yield=as.numeric(trainimp$yield)





for(i in seq(1:9)){

  trainimp[,i] = na.kalman(model="auto.arima",trainimp[,i])
  print(i)
}

for(i in seq(1:9)){
  
  trainimp[,i] = na.seadec(trainimp[,i], algorithm = "ma")
  print(i)
}

library(dplyr)
trainimp$X=train$X
train <- na.omit(train)
test <- left_join(subm, test, by="X") 
test$index <- row.names(test)
test.nna <- na.omit(test)

library(caTools)

ma_yield=runmean(train$yield,3)
str(train)

library(h2o)
## Create an H2O cloud 
h2o.init(nthreads=-1,max_mem_size = "6G")    ## specify the memory size for the H2O cloud
h2o.removeAll()


h20.train<-as.h2o(train) 
h20.train$ma=as.numeric(ma_yield)
h20.testfull<-as.h2o(test)
h20.test<-as.h2o(test.nna)  

str(h20.train)
str(h20.testfull)
col=colnames(h20.train)
col=as.vector(col)

colnames.train=colnames(h20.train)
remove<-c("X","yield")
colnames.train.x=setdiff(colnames.train, remove)
colnames.train.y="yield"

#trainforpca=train
#trainforpca$yield=NULL
#trainforpca$X=NULL
#h20.train.pca=as.h2o(trainforpca)

#PCA
#str(h20.train.pca)
#pca_model<-h2o.prcomp(h20.train.pca, k = 8, transform = "STANDARDIZE")
#plot(pca_model@model$sdev)
#features_pca <- h2o.predict(pca_model, h20.train.pca, num_pc=5)
#summary(features_pca)
#CV frame
library(dplyr)
train<-sample_frac(train, 0.7)
sid<-as.numeric(rownames(train)) # because rownames() returns character
CV_frame<-train[-sid,]
head(CV_frame)
h20.cv=as.h2o(CV_frame)

h20.train<-as.h2o(train) 
h20.testfull<-as.h2o(test)
h20.test<-as.h2o(test.nna)  


####GBM


gbm_fit <- h2o.gbm(
  training_frame = h20.train,    
  nfolds=5,
  x=colnames.train.x,                     
  y=colnames.train.y,   
  validation_frame=h20.cv,
  ntrees = 80,                
  learn_rate = 0.2,           ## increase the learning rate (from 0.1)
  max_depth = 100,             ## increase the depth (from 5)
  score_each_iteration = T,   
  model_id = "gbm_200", 
  seed = 2000000)     
summary(gbm_fit)

rf1 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = h20.train,    
  x=colnames.train.x,                     
  y=colnames.train.y, 
  model_id = "rf_covType_v1",   
  validation_frame=h20.cv,
  ntrees = 100, 
  max_depth = 100,keep_cross_validation_predictions=T,
  stopping_metric="MSE",
  seed = 1000000) 


Predictions<-h2o.predict(object = rf1,newdata = h20.test)

Predictions[(Predictions<0.0001)]<-0
h20.test$yield<-Predictions

testfull.df=as.data.frame(h20.testfull)
test.df=as.data.frame(h20.test)
head(test.df)
head(testfull.df)
str(test)
str(testfull.df)
testfull.df[test.df$index,]$yield <- test.df$yield
head(testfull.df)

submit <- data.frame(testfull.df$X, yield = testfull.df$yield)
write.csv(submit,file = "GBMwithcv.csv")


## Grid
learn_rate_opt <- c(0.01, 0.02,0.03) 
max_depth_opt <- c(30, 40, 100)
sample_rate_opt <- c(0.7, 0.8, 0.9, 1.0)
col_sample_rate_opt <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
ntrees<-c(200,150)
nfolds <- 4
hyper_params <- list(learn_rate = learn_rate_opt,
                     max_depth = max_depth_opt, 
                     sample_rate = sample_rate_opt,
                     col_sample_rate = col_sample_rate_opt,ntrees=ntrees)


gbm_grid <- h2o.grid("gbm", x = colnames.train.x, y = colnames.train.y,
                     training_frame = h20.train,
                     seed = 1,
                     nfolds = nfolds,
                     validation_frame = h20.cv,
                     
                     keep_cross_validation_predictions = TRUE,
                     hyper_params = hyper_params
)
summary(gbm_grid)

best_model <- h2o.getModel(gbm_grid@model_ids[[1]])
best_model_params<-(best_model@allparameters)
bestgbm_modelid<-best_model_params$model_id
ntreesgbm=best_model_params$ntrees

learnrategbm=best_model_params$learn_rate
max_depthgbm=best_model_params$max_depth
samplerategbm=best_model_params$sample_rate
colsamplerategbm=best_model_params$col_sample_rate



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

rf_grid <- h2o.grid("randomForest", x = colnames.train.x, y = colnames.train.y,
                    training_frame = h20.train,
                    
                    seed = 1,
                    nfolds = nfolds,
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
print(dl_models.grid)




###wrapper GRID

learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper")
metalearner <- "h2o.gbm.wrapper"
fit <- h2o.ensemble(x = colnames.train.x, y = colnames.train.y,
                    training_frame = h20.train,
                 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5))




pred <- predict(fit, h20.test)

yield=as.data.frame(pred$pred)
h20.test$yield<-pred$pred

testfull.df=as.data.frame(h20.testfull)
test.df=as.data.frame(h20.test)
head(test.df)
head(testfull.df)
str(test)
str(testfull.df)
testfull.df[test.df$index,]$yield <- test.df$yield
head(testfull.df)
submit <- data.frame(testfull.df$X, yield = testfull.df$yield)
write.csv(submit,file = "GBMwithcv.csv")



