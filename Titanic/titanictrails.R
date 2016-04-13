train <- read.csv("train.csv")
test <- read.csv("test.csv")

train.df=data.frame(train)
head(train.df)
#Remove Cabin
train.df['Cabin']<- NULL
#Remove Ticket
train.df['Ticket']<- NULL
train.df['PassengerId']<- NULL
train.df['Name']<- NULL

#Replacing Gender variable (Male/Female) with a Dummy Variable (0/1)
train.df$Sex = gsub("female", 1, train.df$Sex)
train.df$Sex = gsub("male", 0, train.df$Sex)
test$Sex = gsub("female", 1, test$Sex)
test$Sex = gsub("male", 0, test$Sex)

#Replacing C Q S with 0 1 2
train.df$Embarked = gsub("C", 0, train.df$Embarked)
train.df$Embarked = gsub("Q", 1, train.df$Embarked)
train.df$Embarked = gsub("S", 2, train.df$Embarked)
test$Embarked = gsub("C", 0, test$Embarked)
test$Embarked = gsub("Q", 1, test$Embarked)
test$Embarked = gsub("S", 2, test$Embarked)
#Getting row nos for names with Mr, Master etc
master_vector = grep("Master.",train$Name, fixed=TRUE)
miss_vector = grep("Miss.", train$Name, fixed=TRUE)
mrs_vector = grep("Mrs.", train$Name, fixed=TRUE)
mr_vector = grep("Mr.", train$Name, fixed=TRUE)
dr_vector = grep("Dr.", train$Name, fixed=TRUE)

#predictAge from other variables 
#DF with Age == NAs
Age.Data.Frame.WithNAs= subset(train.df, is.na(Age)) 

Age.Data.Frame.WithNAs['Age']=NULL
#Df with Age!=Nas
Age.NotNA.df=subset(train.df, !is.na(Age)) 
str(Age.Data.Frame.WithNAs)
str(Age.NotNA.df)
#Linear model
train.lm.age <- lm(Age ~ Pclass + Sex  +SibSp + Survived +Embarked +Fare+ Parch , data = Age.NotNA.df)
Lm.predict=predict(train.lm.age,Age.Data.Frame.WithNAs)
#randomforest
library(rpart)
fit <- rpart(Age ~ Pclass + Sex  +SibSp + Survived  +Embarked +Fare+ Parch,
             data=Age.NotNA.df, method="anova")

summary(fit)
Prediction <- predict(fit, Age.Data.Frame.WithNAs, type = "vector")

Age.Data.Frame.WithNAs['Age']=Prediction
#all.ages=Age.NotNA.df$Age
#Age.NotNA.df['Age']=NULL
#Age.NotNA.df['Age']=all.ages

Predicted.Age.Train.df=rbind(Age.Data.Frame.WithNAs,Age.NotNA.df)
str(Age.Data.Frame.WithNAs)
str(Predicted.Age.Train.df)
Predicted.Age.Train.df <- as.data.frame(unclass(Predicted.Age.Train.df))
test<- as.data.frame(unclass(test))
# Predicted.Age.Train.df['Name']=0
# count<-0
# for(i in master_vector) {
#   count = count +Predicted.Age.Train.df$Age[i] 
# 
# }
# 
# AvgMasterAge= count/length(master_vector)
# count<-0
# for(i in miss_vector) {
#   count = count +Predicted.Age.Train.df$Age[i] 
#   
# }
# AvgMissAge= count/length(master_vector)
# 
# for(i in mrs_vector) {
#   Predicted.Age.Train.df$Name[i] = "Mrs"
# }
# for(i in mr_vector) {
#   Predicted.Age.Train.df$Name[i] = "Mr"
# }
# for(i in dr_vector) {
#   Predicted.Age.Train.df$Name[i] = "Dr"
# }

library(party)
fit <- ctree(Kyphosis ~ Age + Number + Start, 
             data=kyphosis)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=Predicted.Age.Train.df, method="class")

gbmmodel<-train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                data=Predicted.Age.Train.df, method="gbm", distribution="bernoulli")

fit <- cforest(Survived  ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
               data = Predicted.Age.Train.df, controls=cforest_unbiased(ntree=2000, mtry=3))


Prediction <- predict(fit, test, OOB=TRUE, type = "response")


Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "Submission2.csv", row.names = FALSE)

