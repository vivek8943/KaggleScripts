train <- read.csv("train.csv")
test <- read.csv("test.csv")
library(rpart)
library(party)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
train$Name[1]
test$Survived <- NA
combi <- rbind(train, test)

combi$Name <- as.character(combi$Name)
# What's in a name, again?
combi$Name[1]

# Find the indexes for the tile piece of the name
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]

# Engineered variable: Title

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
# Inspect new feature
table(combi$Title)
# Combine small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
combi$Title <- factor(combi$Title)

# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Engineered variable: Family
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Inspect new feature
table(combi$FamilyID)
# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)

# Split back into test and train sets


library(mice)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(combi[, !names(combi) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived','Fare')], method='rf')
mice_output <- complete(mice_mod)
combi$Age <- mice_output$Age


#write.csv(train, "cleanedtrain.csv", row.names=FALSE)
#write.csv(test, "cleanedtest.csv", row.names=FALSE)
#save(train,test,file="cleaned.Rda")

#train$Age=Predicted.Age.Train.df$Age

# Build a new tree with our new features

traincleaned<- combi[1:891,]
testcleaned <- combi[892:1309,]


check.missing<-function(x) return(paste0(round(sum(is.na(x))/length(x),4)*100,'%'))
data.frame(sapply(traincleaned,check.missing))
data.frame(sapply(testcleaned,check.missing))




set.seed(415)
fit <- cforest(as.factor(Survived) ~  Pclass + Sex + Age + SibSp + Parch + Embarked + Title + FamilySize+FamilyID ,
               data = traincleaned, controls=cforest_unbiased(ntree=2200, mtry=3))
varimp(fit)
varimpAUC(fit)
Prediction <- predict(fit, testcleaned, OOB=TRUE,  type = "response")




rpartfit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Title + FamilySize+FamilyID ,
             data=traincleaned, method="class")
fancyRpartPlot(fit)

# Now let's make a prediction and write a submission file
Predictionrp <- predict(fit, testcleaned, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "Submissionlessvars.csv", row.names = FALSE)
library(ada)




perf <- performance(Prediction,"tpr","fpr")
plot(perf)


varImpPlot(fit)


