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

#Nov-mar rain
#jan-dec very cool


train$month=format(as.POSIXct(train$X, format="%Y-%m-%d %H:%M"), format="%m")
test$month=format(as.POSIXct(test$X, format="%Y-%m-%d %H:%M"), format="%m")
subm$month=format(as.POSIXct(subm$X, format="%Y-%m-%d %H:%M"), format="%m")


train$day=format(as.POSIXct(train$X, format="%Y-%m-%d %H:%M"), format="%d")
test$day=format(as.POSIXct(test$X, format="%Y-%m-%d %H:%M"), format="%d")
subm$day=format(as.POSIXct(subm$X, format="%Y-%m-%d %H:%M"), format="%d")

### Add lagged average component to account for time series info! 
leafwet450_min.twodaysago=lag(lag(train$leafwet450_min))
leafwet450_min.onedayago=lag(train$leafwet450_min)
yield.onedayago=lag(train$yield)
yield.twodaysago=lag(lag(train$yield))
wind_dir.onedayago=(lag(train$wind_dir))
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
train$wind_dir.twodaysago=wind_dir.twodaysago
train$leafwet450_min.twodaysago=leafwet450_min.twodaysago
train$leafwet450_min.onedayago=leafwet450_min.onedayago
train$precp.lag1=precp.lag1
train$precp.lag2=precp.lag2
train$temp.lag1=temp.lag1
train$temp.lag2=temp.lag2
head(train)



train <- na.omit(train)
str(train)
str(test)

library(dplyr)
test <- 
  left_join(subm, test.imputed, by="X") 
 
test$index <- row.names(test)
test.nna <- na.omit(test)
train$hour<-as.factor(train$hour)
train$day<-as.factor(train$day)
train$month<-as.factor(train$month)
str(test.nna)
test$hour<-as.factor(test.nna$hour)
train$day<-as.factor(train$day)
train$month<-as.factor(train$month)

rf <- randomForest(yield ~ percip_mm+humidity+leafwet450_min+temp+leafwet_lwscnt+wind_dir+wind_ms+day+month+hour, data=train, ntree=1000)
predict(rf,)

library(h2o)
## Create an H2O cloud 
h2o.init(nthreads=-1,max_mem_size = "6G")    ## specify the memory size for the H2O cloud
h2o.removeAll()


h20.train<-as.h2o(train) 
h20.testfull<-as.h2o(test)
h20.test<-as.h2o(test.nna)  

str(h20.train)
str(h20.testfull)
colnames.train=colnames(h20.train)
remove<-c("X","yield","leafwet460_min")
colnames.train.x=setdiff(colnames.train, remove)
colnames.train.y="yield"

h20.train$hour=as.factor(h20.train$hour)
h20.train$month=as.factor(h20.train$month)
h20.train$day=as.factor(h20.train$day)



str(h20.train)


gbm_fit <- h2o.gbm(
  training_frame = h20.train,    
  
  x=colnames.train.x,                     
  y=colnames.train.y,                      
  ntrees = 200,                
  learn_rate = 0.3,           ## increase the learning rate (from 0.1)
  max_depth = 100,             ## increase the depth (from 5)
  stopping_rounds = 3,        
  stopping_tolerance = 0.01,  
  score_each_iteration = T,   
  model_id = "gbm_200", 
  seed = 2000000)     
summary(gbm_fit)

rf1 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = h20.train,    
  x=colnames.train.x,                     
  y=colnames.train.y, 
  model_id = "rf_covType_v1",   
  ntrees = 10,          
  seed = 1000000) 

summary(rf1)

finalRf_predictions<-h2o.predict(
  object = rf1
  ,newdata = h20.test)

Predictions<-h2o.predict(object = gbm_fit,newdata = h20.test)
head(Predictions)
Predictions[(Predictions<0)]<-0
head(Predictions)
h20.test$yield<-Predictions
str(h20.test)

testfull.df=as.data.frame(h20.testfull)
test.df=as.data.frame(h20.test)
head(test.df)
head(testfull.df)
str(test)
str(testfull.df)
h20.testfull[h20.test$index,]$yield <- h20.test$yield

library(dplyr)

str(testfull.df)
testfull.df[test.df$index,]$yield <- test.df$yield
head(testfull.df)
str(testfull.df)
merged.df <- left_join(testfull.df, test.df, by="index")
str(test$yield.y=NULL)
submit <- data.frame(X = testfull.df$X, yield = testfull.df$yield)
write.csv(submit, file = "fognet.csv", row.names = FALSE)



write.csv(test.imputed,file="tesi.csv")
