###############################################
#Models for Fognet data only
library(h2o)

setwd("~/Desktop/KaggleScripts/FOGNETS/data/")
library(h2oEnsemble) 
subm <- read.csv("submission_format.csv")
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,
                     max_mem_size = '10g', min_mem_size = '4g', nthreads = -1)

#load Data
library(DataCombine)
load('BasicDataSet_FNM.Rd')
str(train.imputed)

yieldlag=slide(train.imputed,Var = 'yield',slideBy = -1)
head(yieldlag)
train.imputed$yieldlag=yieldlag$`yield-1`
training.data<-as.h2o(train.imputed)

training.data$year <- as.factor(training.data$year)
training.data$month <- as.factor(training.data$month)
training.data$day <- as.factor(training.data$day)
training.data$hour <- as.factor(training.data$hour)
str(training.data)
str(test.imputed)
testing.data.hex<-as.h2o(test.imputed)
testing.data.hex$year <- as.factor(testing.data.hex$year)
testing.data.hex$month <- as.factor(testing.data.hex$month)
testing.data.hex$day <- as.factor(testing.data.hex$day)
testing.data.hex$hour <- as.factor(testing.data.hex$hour)
str(testing.data.hex)

str(training.data[2:16])
gc()
##

library(tsDyn)

nnetTs(x, m, d = 1, steps = d, series, size, 
       control = list(trace = FALSE))
##



######
# RF Hyperparamters

ntrees=c(50,200)
mtries_opt <- 5:20 
max_depth_opt <- c( 25, 50)
sample_rate_opt <- c(0.7, 0.8, 0.9)
col_sample_rate_per_tree_opt <- c(0.2, 0.3, 0.4, 0.5, 0.6)
hyper_params <- list(mtries = mtries_opt,
                     max_depth = max_depth_opt,
                     sample_rate = sample_rate_opt
                     ,ntrees=ntrees
)

x=colnames(training.data)
remove<-c("X","yield","year","gusts_ms")
x=setdiff(x, remove)
y="yield"

rf_grid <- h2o.grid("randomForest", x = x, y = y,
                    training_frame = training.data,
                    seed = 1,
                    nfolds = 4,
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
h2o.randomForest.wrap <- function(..., ntrees =ntreesrf,sample_rate=samplerategbm,mtries=mtriesrf, max_depth=max_depthrf, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, sample_rate = sample_rate,max_depth = max_depth, seed = seed,mtries = mtries)


# GBM Hyperparamters
learn_rate_opt <- c(0.01, 0.1) 
max_depth_opt <- c(3, 4, 5)
sample_rate_opt <- c(0.7, 0.8, 0.9)
col_sample_rate_opt <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
ntrees<-c(200,100,50)
nfolds <- 3
hyper_params <- list(learn_rate = learn_rate_opt,
                     max_depth = max_depth_opt, 
                     sample_rate = sample_rate_opt,
                     col_sample_rate = col_sample_rate_opt,ntrees=ntrees)
gbm_grid <- h2o.grid("gbm",  x = 2:16, y = 'yield',
                     training_frame = training.data,
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

h2o.gbm.wrap <- function(..., ntrees = ntreesgbm,learn_rate=learnrategbm,sample_rate=samplerategbm,col_sample_rate = colsamplerategbm,max_depth = max_depthgbm, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees,max_depth = max_depth, col_sample_rate = col_sample_rate, learn_rate=learn_rate,sample_rate=sample_rate,seed = seed)

h2o.glm_nn <- function(..., non_negative = TRUE) {
  h2o.glm.wrapper(..., non_negative = non_negative)
}
metalearner <- "h2o.glm_nn"


learner <- c("h2o.randomForest.wrap","h2o.gbm.wrap","h2o.glm.wrapper")
gc()
fit <- h2o.ensemble(x = 2:16, y = 'yield',
                    training_frame = training.data,
                   
                    learner = learner,
                    metalearner = metalearner,
                    cvControl = list(V = 10, shuffle = TRUE))

h2o.ensemble_performance(fit,testing.data.hex)
pp <- predict(fit, testing.data.hex)
class(pp$basepred)
predictions <- as.data.frame(pp$basepred) #third column, p1 is P(Y==1)
predictions$predict[predictions$predict<0]<-0

head(predictions)
test$gbm=(predictions$h2o.gbm.wrap)
test$glm=predictions$h2o.gbm.wrap
testing.data.hex$yield=pred$predict
test=as.data.frame(testing.data.hex)
library(dplyr)
test <- left_join(subm, test, by="X") 
str(test)

test$yield.y[is.na(test$yield.y)]<-0
submit <- data.frame(test$X, yield = test$yield.y)
write.csv(submit,file = "rfgdngsearch.csv")
##################################################
library(party)
train.imputed$yieldlag[is.na(train.imputed$yieldlag)]<-0
fit <- cforest(yield ~ percip_mm+humidity+leafwet450_min+leafwet450_min+temp+leafwet_lwscnt+wind_dir+wind_ms+day+month+hour+yieldlag,
               data = train.imputed, controls=cforest_unbiased(ntree=2000, mtry=3))

varimpAUC(fit)
Prediction <- table(fit, test.imputed, OOB=TRUE, type = "response")




Prediction <- predict(fit, test.imputed, OOB=TRUE, type = "response")
test.imputed$yield=Prediction

test <- left_join(subm, test.imputed, by="X") 
str(test)
test$yield.y[is.na(test$yield.y)]<-0

submit <- data.frame(test$X, yield = test$yield.y)
write.csv(submit,file = "cffog.csv")
#################################################

#Models for Sidi+guel+Fognets

load('FNAImputedaddedFognet.Rd')
str(Sidi.guel.fognet.train)
str(Sidi.guel.fognet.test)



##################################################

###################################################
#models with weighted average for prediction
Sidi.guel.fognet.train


h2o.shutdown()
###################################################
combi

c<- rbind(train.imputed,test.imputed)


train.imputed$yield[train.imputed$yield>0]<-1
str(train.imputed)
train.imputed$yield=as.factor(train.imputed$yield)
write.csv(train.imputed,file = "ti.csv")


