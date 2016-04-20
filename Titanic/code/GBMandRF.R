library(h2o)
## Create an H2O cloud 
h2o.init(
  nthreads=-1,            ## -1: use all available threads
  max_mem_size = "2G")    ## specify the memory size for the H2O cloud
h2o.removeAll() # Clean slate - just in case the cluster was already running

h20.train<-as.h2o(trainmice) 
train<-h20.train
h20.test<-as.h2o(testmice)  
test<-h20.test
train['Ticket']=NULL
test['Ticket']=NULL

head(test)
test['Survived']=0
y <- c("Survived","PassengerId","Ticket")
x <- setdiff(names(h20.train), y)
y<-"Survived"

## run our first predictive model
rf1 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = train,        ## the H2O frame for training
                               ## the H2O frame for validation (not required)
  x=x,                        ## the predictor columns, by column index
  y=y,                          ## the target index (what we are predicting)
  model_id = "rf_covType_v1",    ## name the model in H2O
                                  ##   not required, but helps use Flow
  ntrees = 200,                  ## use a maximum of 200 trees to create the
 
  stopping_rounds = 3,           ## Stop fitting new trees when the 2-tree
  score_each_iteration = T,      ## Predict against training and validation for
                                  ##  each tree. Default will skip several.
  seed = 1000000)                ## Set the random seed so that this can be

summary(rf1)                     ## View information about the model.

gbm1 <- h2o.gbm(
  training_frame = train,        ## the H2O frame for training
          ## the H2O frame for validation (not required)
  x=x,                        ## the predictor columns, by column index
  y=y,                          ## the target index (what we are predicting)
  model_id = "gbm_covType1",     ## name the model in H2O
  seed = 2000000)                ## Set the random seed for reproducability

###############################################################################
summary(gbm1)                   ## View information about the model.

h2o.hit_ratio_table(gbm1,valid = T)


###############################################################################
gbm2 <- h2o.gbm(
  training_frame = train,     ##
 
  x=x,                     ##
  y=y,                       ## 
  ntrees = 200,                ## decrease the trees, mostly to allow for run time
  ##  (from 50)
  learn_rate = 0.2,           ## increase the learning rate (from 0.1)
  max_depth = 50,             ## increase the depth (from 5)
  stopping_rounds = 3,        ## 
  stopping_tolerance = 0.01,  ##
  score_each_iteration = T,   ##
  model_id = "gbm_covType2",  ##
  seed = 2000000)             ##



gbm3 <- h2o.gbm(
  training_frame = train,     ##

  x=x,                     ##
  y=y,                       ## 
  ntrees = 500,                ## add a few trees (from 20, though default is 50)
  learn_rate = 0.33,           ## increase the learning rate even further
  max_depth = 10,             ## 
  sample_rate = 0.7,          ## use a random 70% of the rows to fit each tree
  col_sample_rate = 0.7,       ## use 70% of the columns to fit each tree
  stopping_rounds = 2,        ## 
  stopping_tolerance = 0.01,  ##
  score_each_iteration = T,   ##
  model_id = "gbm_covType3",  ##
  seed = 2000000)             ##
###############################################################################

summary(gbm3)
h2o.hit_ratio_table(rf1,valid = T)[1,2]     ## review the random forest accuracy
h2o.hit_ratio_table(gbm1,valid = T)[1,2]    ## review the first model's accuracy
h2o.hit_ratio_table(gbm2,valid = T)[1,2]    ## review the second model's accuracy
h2o.hit_ratio_table(gbm3,valid = T)[1,2]    ## review the newest model's accuracy
###############################################################################


rf2 <- h2o.randomForest(        ##
  training_frame = train,       ##
  x=x,                       ##
  y=y,                         ##
  model_id = "rf_covType2",     ## 
  ntrees = 500,                 ##
  max_depth = 30,               ## Increase depth, from 20
  stopping_rounds = 2,          ##
  stopping_tolerance = 1e-2,    ##
  score_each_iteration = T,     ##
  seed=3000000)                 ##
###############################################################################
finalRf_predictions<-h2o.predict(
  object = rf2
  ,newdata = test)
finalgbm_predictions<-h2o.predict(
  object = gbm3
  ,newdata = test)


prediction <- as.numeric(finalRf_predictions$predict)
pred.df=as.data.frame(prediction)
round(pred.df$predict)
pred.df$predict[pred.df$predict<0]=0




submit <- data.frame(PassengerId = testmice$PassengerId, Survived = round(pred.df$predict))
write.csv(submit, file = "rf.csv", row.names = FALSE)

#GBM 0.755 TRAINING ERROR 0.22 but overfit
#RF 0.77 Trainng error 0.3 

h2o.shutdown(prompt=FALSE)