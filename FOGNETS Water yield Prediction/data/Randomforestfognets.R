#GET Data from CSV

setwd("~/Desktop/KaggleScripts/FOGNETS/data/")
train <- read.csv("train_2h.csv")
test <- read.csv("test_2h.csv")
subm <- read.csv("submission_format.csv")

ts_train<-ts(train)
head(ts_train)
##
test <- left_join(subm, test, by="X")

str(train)
test$yield=9999
sapply(train, function(x) sum(is.na(x))) 
sapply(test, function(x) sum(is.na(x))) 

##Mice impute

library(mice)
combi <- rbind(train, test)
str(combi)
mice_mod <- mice(combi[, !names(combi) %in% c('X','wind_ms','wind_dir','gusts_ms','humidity')], method='rf')
mice_output <- complete(mice_mod)
combi$leafwet460_min=mice_output$leafwet460_min
combi$temp=mice_output$temp
combi$leafwet450_min=mice_output$leafwet450_min
combi$percip_mm=mice_output$percip_mm
combi$leafwet_lwscnt=mice_output$leafwet_lwscnt

sapply(combi, function(x) sum(is.na(x))) 


#split back to train and test

traincleaned<- combi[1:5802,]
testcleaned <- combi[5803:6912,]
testcleaned$yield=NULL
str(traincleaned)
traincleaned <- na.omit(traincleaned)

## Set Coly,x
colnames.train=colnames(train)
remove<-c("X","yield")
colnames.train.x=setdiff(colnames.train, remove)
colnames.train.y="yield"

#Prepare Test data
#test <- left_join(subm, test, by="X") 
#test$index <- row.names(test)

test.nna <- na.omit(test)
test.nna$yield.x =NULL
test.nna$yield.y =NULL
test.nna$yield =NULL
test$yield.x =NULL
test$yield.y =NULL
test$yield =0
str(test)
str(test.nna)
#Random forest 
rf <- randomForest(yield ~ percip_mm+humidity+leafwet450_min+temp+gusts_ms+leafwet_lwscnt+wind_dir+wind_ms, data=traincleaned, ntree=200)
test.nna$yield <- predict(rf, newdata=test.nna)
plot( importance(rf), lty=2, pch=16)
lines(importance(rf))
plot(rf)


test.nna[test.nna$yield < .00001,]

str(test)
str(test.nna)
submit <- data.frame(test$X, yield = test$yield)
write.csv(submit,file = "simplerf.csv")

#cforest
library(party)
fit <- cforest(yield ~ percip_mm+humidity+leafwet450_min+temp+gusts_ms+leafwet_lwscnt+leafwet460_min+wind_dir+wind_ms,
               data = traincleaned, controls=cforest_unbiased(ntree=2000, mtry=3))



Prediction <- predict(fit, test.nna, OOB=TRUE, type = "response")
test.nna$yield =Prediction
submit <- data.frame(test$X, yield = test$yield)
write.csv(submit,file = "simplerf.csv")

