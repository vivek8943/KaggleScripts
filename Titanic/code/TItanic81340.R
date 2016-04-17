setwd("~/Desktop/Titanic")
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
train.df=combi
Age.Data.Frame.WithNAs= subset(train.df, is.na(Age)) 

Age.Data.Frame.WithNAs['Age']=NULL
#Df with Age!=Nas
Age.NotNA.df=subset(train.df, !is.na(Age)) 
str(Age.Data.Frame.WithNAs)
str(Age.NotNA.df)
library(rpart)
fit <- rpart(Age ~ Pclass +SibSp + Survived +Embarked +Fare+ Title+FamilySize + Sex+FamilyID,
             data=Age.NotNA.df, method="anova")

summary(fit)
Prediction <- predict(fit, Age.Data.Frame.WithNAs, type = "vector")

Age.Data.Frame.WithNAs['Age']=Prediction


Predicted.Age.Train.df=rbind(Age.Data.Frame.WithNAs,Age.NotNA.df)
str(Age.Data.Frame.WithNAs)
str(Predicted.Age.Train.df)
Predicted.Age.Train.df <- as.data.frame(unclass(Predicted.Age.Train.df))
test<- as.data.frame(unclass(test))
str(test)

Train.df=subset(Predicted.Age.Train.df, !is.na(Survived)) 

Test.df= subset(Predicted.Age.Train.df, is.na(Survived)) 
str(Train.df)

write.csv(Train.df, "rpartagetrain.csv", row.names=FALSE)
write.csv(Test.df, "rpartagetest.csv", row.names=FALSE)
check.missing<-function(x) return(paste0(round(sum(is.na(x))/length(x),4)*100,'%'))
data.frame(sapply(Train.df,check.missing))
data.frame(sapply(Test.df,check.missing))

set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Fare + Embarked + Title + FamilySize + FamilyID,
               data = Train.df, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, Test.df, OOB=TRUE, type = "response")
summary(fit)
submit <- data.frame(PassengerId = Test.df$PassengerId, Survived = Prediction)
write.csv(submit, file = "rpartagecforestsurv.csv", row.names = FALSE)


#8.1340

varimp(fit, mincriterion = 0, conditional = FALSE, 
       threshold = 0.2, nperm = 1, OOB = TRUE)


