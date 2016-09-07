library(rpart)
library(rattle)
library(rpart.plot)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

train$Name[1]
test$Survived <- NA
fulldata <- rbind(train, test)

fulldata$Name <- as.character(fulldata$Name)


strsplit(fulldata$Name[1], split='[,.]')
strsplit(fulldata$Name[1], split='[,.]')[[1]]
strsplit(fulldata$Name[1], split='[,.]')[[1]][2]

# Engineered variable: Title

fulldata$Title <- sapply(fulldata$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
fulldata$Title <- sub(' ', '', fulldata$Title)

table(fulldata$Title)

fulldata$Title[fulldata$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
fulldata$Title[fulldata$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
fulldata$Title[fulldata$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

fulldata$Title <- factor(fulldata$Title)


fulldata$FamilySize <- fulldata$SibSp + fulldata$Parch + 1

# Engineered variable: Family
fulldata$Surname <- sapply(fulldata$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
fulldata$FamilyID <- paste(as.character(fulldata$FamilySize), fulldata$Surname, sep="")
fulldata$FamilyID[fulldata$FamilySize <= 2] <- 'Small'
# Inspect new feature
table(fulldata$FamilyID)
# Delete bad family IDs
famIDs <- data.frame(table(fulldata$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
fulldata$FamilyID[fulldata$FamilyID %in% famIDs$Var1] <- 'Small'

fulldata$FamilyID <- factor(fulldata$FamilyID)


Age.Data.Frame.WithNAs['Age']=Prediction
str(Age.Data.Frame.WithNAs)
age.predicted.df=rbind(Age.NotNA.df,Age.Data.Frame.WithNAs)
tail(age.predicted.df)
train.df=subset(age.predicted.df, !is.na(Survived)) 
Test.df= subset(age.predicted.df, is.na(Survived)) 
head(Test.df)
str(train.df)
Test.df['Survived']=NULL

# PREDICTED AGE TEST AND TRAIN with added features

set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train.df, controls=cforest_unbiased(ntree=2000, mtry=3))



Prediction <- predict(fit, Test.df, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = Test.df$PassengerId, Survived = Prediction)
write.csv(submit, file = "Submission2.csv", row.names = FALSE)
