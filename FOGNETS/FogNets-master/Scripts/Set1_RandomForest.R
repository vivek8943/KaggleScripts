source("F:\\Data Science\\FogNets\\Scripts\\Set1_DataProcess.R")

#### Model Fitting ####
library(randomForest)
train.rf <- select(train, -x, -leafwet460_min)
rf <- randomForest(yield ~ ., data=train.rf, ntree=1000)



#### Test set, submission generation ####
test <- read.csv("test_micro_2hr.csv")
subm <- read.csv("submission_format.csv")
test <- 
  left_join(subm, test, by="X") %>%
  select(-leafwet460_min)
test$index <- row.names(test)

test.nna <- na.omit(test)
test.nna$yield <- predict(fit, newdata=test.nna)
test.nna[test.nna$yield < .0001,] #optional to zero calc

test[test.nna$index,]$yield <- test.nna$yield

# be careful of excel - it screws the date

str(test$yield)
write.csv(test,"pred.csv")

