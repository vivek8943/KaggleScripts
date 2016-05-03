library(h2oEnsemble)
setwd("~/Desktop/KaggleScripts/FOGNETS/data/")
train <- read.csv("train_2h.csv")
test <- read.csv("test_2h.csv")
subm <- read.csv("submission_format.csv")
colnames.train=colnames(train)
remove<-c("X","yield","leafwet460_min")
colnames.train.x=setdiff(colnames.train, remove)
colnames.train.y="yield"
train["leafwet460_min"]=NULL
str(train)

### Add hour to the Data to account for time of the day!!! 

train$hour=format(as.POSIXct(train$X, format="%Y-%m-%d %H:%M"), format="%H")
test$hour=format(as.POSIXct(test$X, format="%Y-%m-%d %H:%M"), format="%H")
subm$hour=format(as.POSIXct(subm$X, format="%Y-%m-%d %H:%M"), format="%H")

### Add lagged average component to account for time series info! 
leafwet450_min.twodaysago=lag(lag(train$leafwet450_min))
leafwet450_min.onedayago=lag(train$leafwet450_min)
yield.onedayago=lag(train$yield)

yield.twodaysago=lag(lag(train$yield))
wind_dir.onedayago=(lag(train$wind_dir))
wind_dir.onedayagot=(lag(test$wind_dir))
wind_dir.twodaysago=(lag(train$wind_dir,n=2))
temp.lag1=lag(train$temp)
temp.lag2=lag(train$temp,n=2)
precp.lag1=lag(train$percip_mm)
precp.lag2=lag(train$percip_mm,n=2)

length(temp.lag2)
#add lag values to Data Frame
train$yield.onedayago=yield.onedayago
train$yield.twodaysago=yield.twodaysago
train$wind_dir.onedayago=wind_dir.onedayago
test$wind_dir.onedayago=wind_dir.onedayagot
train$wind_dir.twodaysago=wind_dir.twodaysago
train$leafwet450_min.twodaysago=leafwet450_min.twodaysago
train$leafwet450_min.onedayago=leafwet450_min.onedayago
train$precp.lag1=precp.lag1
train$precp.lag2=precp.lag2
train$temp.lag1=temp.lag1
train$temp.lag2=temp.lag2
str(train)
train <- na.omit(train)
str(train)
str(test)
test <- left_join(subm, test, by="X") 
str(train)
head(test)

test$index <- row.names(test)
test$hour.y=NULL
str(test)
test.nna <- na.omit(test)
str(train)

rf <- randomForest(yield ~ percip_mm+humidity+leafwet450_min+temp+leafwet_lwscnt+wind_dir+wind_ms+day+month+hour, data=train, ntree=1000)
test.nna$yield <- predict(rf, newdata=test.nna)


library(h2o)
## Create an H2O cloud 
h2o.init(nthreads=-1,max_mem_size = "6G")    ## specify the memory size for the H2O cloud
h2o.removeAll()


h20.train<-as.h2o(train) 
h20.testfull<-as.h2o(test)
h20.test<-as.h2o(test.nna)  

str(h20.train)
str(h20.testfull)
col.x=colnames(h20.train)

remove<-c("X","yield","leafwet460_min","wind_dir.twodaysago","temp.lag2","precp.lag2","leafwet450_min.twodaysago","temp.lag2","precp.lag1" ,"temp.lag1"   )
colnames.train.x=setdiff(col.x, remove)
col.x=colnames.train.x
col.x
col.y="yield"

h20.train$hour=as.factor(h20.train$hour)
str(h20.train)
#####GS

learn_rate_opt <- c(0.01, 0.02,0.03) 
max_depth_opt <- c(30, 40, 100)
sample_rate_opt <- c(0.7, 0.8, 0.9, 1.0)
col_sample_rate_opt <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
ntrees<-c(200,100,150)
nfolds <- 4
hyper_params <- list(learn_rate = learn_rate_opt,
                     max_depth = max_depth_opt, 
                     sample_rate = sample_rate_opt,
                     col_sample_rate = col_sample_rate_opt,ntrees=ntrees)
gbm_grid <- h2o.grid("gbm", x = col.x, y = col.y,
                     training_frame = h20.train,
                     seed = 1,
                     nfolds = nfolds,
                    
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

h2o.gbm.best <- function(..., ntrees = ntreesgbm,learn_rate=learnrategbm,sample_rate=samplerategbm,col_sample_rate = colsamplerategbm,max_depth = max_depthgbm, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees,max_depth = max_depth, col_sample_rate = col_sample_rate, learn_rate=learn_rate,sample_rate=sample_rate,seed = seed)

#####DEepl

# Deeplearning Hyperparamters
activation_opt <- c( "RectifierWithDropout", "Rectifier",
                     "MaxoutWithDropout") 
hidden_opt <- list(c(50,20),c(100,20))
l1_opt <- c(0, 1e-3, 1e-5)
l2_opt <- c(0, 1e-3, 1e-5)
epochs = c(20,100)
hyper_params <- list(activation = activation_opt,
                     hidden = hidden_opt,
                     l1 = l1_opt,
                     l2 = l2_opt,
                     epochs =epochs)
gc()

dl_grid <- h2o.grid("deeplearning", x = col.x, y = col.y,
                    training_frame = h20.train,
                    
                    seed = 1,
                    nfolds = 4,
                    
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

# GLM Hyperparamters
alpha_opt <- seq(0,1,0.1)
lambda_opt <- c(0,1e-7,1e-5,1e-3,1e-1)
hyper_params <- list(alpha = alpha_opt,
                     lambda = lambda_opt)

glm_grid <- h2o.grid("glm",  x = col.x, y = col.y,
                     training_frame = h20.train,
                    
                     nfolds = nfolds,
                     
                     keep_cross_validation_predictions = TRUE,                    
                     hyper_params = hyper_params
)
glm_models <- lapply(glm_grid@model_ids, function(model_id) h2o.getModel(model_id))
glm_models[1]


# 
# best_modeldl <- h2o.getModel(dl_grid@model_ids[[1]])
# best_model_paramsdl<-(best_modeldl@allparameters)
# bestgdl_modelid<-best_model_paramsdl$model_id
# bestdl_act=best_model_paramsdl$activation
# bestdl_nfolds=best_model_paramsdl$nfolds
# bestdl_epochs=best_model_paramsdl$epochs
# bestdl_l1=best_model_paramsdl$l1
# bestdl_l2=best_model_paramsdl$l2
# bestdl_hidden=best_model_paramsdl$hidden






metalearner <- "h2o.gbm.wrapper"


learner <- c("h2o.gbm.best")
gc()
fit <- h2o.ensemble(x = col.x, y = col.y,
                    training_frame = h20.train,
                   
                    learner = learner,
                    metalearner = metalearner,
                    cvControl = list(V = 4, shuffle = TRUE))



h2o.ensemble_performance(fit,h20.train)
pp <- predict(fit, h20.test)
head(pp)
h20.test$yield<-pp$pred

testfull.df=as.data.frame(h20.testfull)
test.df=as.data.frame(h20.test)
testfull.df[test.df$index,]$yield <- test.df$yield

submit <- data.frame(X = testfull.df$X, yield = testfull.df$yield)
write.csv(submit, file = "fognetGS1.csv", row.names = FALSE)

gbm_fit <- h2o.gbm(
  training_frame = h20.train,    
  
  x=col.x,                     
  y=col.y,                      
  ntrees = 200, 
  nfolds=4,
  learn_rate = 0.03,           ## increase the learning rate (from 0.1)
  max_depth = 5,             ## increase the depth (from 5)
  stopping_rounds = 3,        
  stopping_tolerance = 0.01,  
  score_each_iteration = T,   
  model_id = "gbm_200", 
  seed = 2000000)             
Predictions<-h2o.predict(object = gbm_fit,newdata = h20.test)
head(Predictions)
Predictions[(Predictions<0)]<-0
head(Predictions)
h20.test$yield<-Predictions
head(h20.test)

testfull.df=as.data.frame(h20.testfull)
test.df=as.data.frame(h20.test)
head(test.df)
head(testfull.df)
str(test)
str(testfull.df)

library(dplyr)

str(testfull.df)
testfull.df[test.df$index,]$yield <- test.df$yield
head(testfull.df)
str(testfull.df)
merged.df <- left_join(testfull.df, test.df, by="index")
str(merged.df)





submit <- data.frame(X = testfull.df$X, yield = testfull.df$yield)
write.csv(submit, file = "fognet.csv", row.names = FALSE)


h2o.shutdown()
