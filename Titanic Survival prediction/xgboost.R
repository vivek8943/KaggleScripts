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
traindataforada=traincleaned
traindataforada['Name']=NULL
traindataforada['Ticket']=NULL
traindataforada['Title']=NULL
traindataforada['Cabin']=NULL
traindataforada['Surname']=NULL
traindataforada['FamilyID']=NULL
testcleaned['Survived']=NULL
testcleaned['Name']=NULL
testcleaned['Ticket']=NULL
testcleaned['Cabin']=NULL
testcleaned['Title']=NULL
testcleaned['Surname']=NULL
testcleaned['FamilyID']=NULL
library(caret)
library(corrplot)
require(lattice)
library("xgboost")
dummies <- dummyVars(~ ., data = traindataforada)
dummiestest <- dummyVars(~ ., data = testcleaned)
mtrain = predict(dummies, newdata = traindataforada)
mtest = predict(dummiestest, newdata = testcleaned)	
mtrainnumeric <- transform(mtrain, class=as.numeric(mtrain))
mtrainnumeric['class']=NULL
mtrainnumeric=(mtrainnumeric[1:891,])
mtestnumeric <- transform(mtest, class=as.numeric(mtest)) 
mtestnumeric['class']=NULL
mtestnumeric=(mtestnumeric[1:418,])


#levelplot(cor(mtrainnumeric,use="pairwise.complete.obs"))
str(mtrainnumeric)


train.matrix = as.matrix(mtrainnumeric)
str(train.matrix)
mode(train.matrix) = "numeric"
labeldataforada=train.matrix[,2]
length(train.matrix[,2])
y = as.matrix(labeldataforada)

test.matrix = as.matrix(mtestnumeric)
mode(test.matrix) = "numeric"
length(test.matrix[,2])
test.matrix[!is.finite(test.matrix)] <- 0
param <- list("objective" = "multi:softprob",    # multiclass classification 
              
              "num_class"=2,
              "nthread" = 4,   # number of threads to be used 
              "max_depth" = 16,    # maximum depth of tree 
              "eta" = 0.3,    # step size shrinkage 
              "gamma" = 0,    # minimum loss reduction 
              "subsample" = 1,    # part of data instances to grow tree 
              "colsample_bytree" = 1,  # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 12  # minimum sum of instance weight needed in a child 
)
set.seed(1234)

# k-fold cross validation, with timing
nround.cv = 20
system.time( bst.cv <- xgb.cv(param=param, data=train.matrix, label=y, 
                              nfold=4, nrounds=nround.cv, prediction=TRUE, verbose=FALSE) )
system.time( bst <- xgboost(param=param, data=train.matrix, label=y, 
                            nrounds=4, verbose=0) )

pred <- predict(bst, test.matrix)  
length(test.matrix[,2])  
length(pred)
head(pred,100)

test.df.final=as.data.frame(test.matrix)
test.df.final['Survived']=round(pred)



submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "Submissionlessvars.csv", row.names = FALSE)



