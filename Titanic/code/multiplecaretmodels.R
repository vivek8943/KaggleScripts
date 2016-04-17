gc(reset=TRUE)
set.seed(1234) 

#Libraries
library(caret)
library(devtools)
install_github('caretEnsemble', 'zachmayer') #Install zach's caretEnsemble package
library(caretEnsemble)
library(mlbench)
#Data

train_xvalues=trainmice
train_xvalues["Survived"]=NULL
train_yvalues=trainmice["Survived"]
traindataforada=train_xvalues
traindataforada['Name']=NULL
traindataforada['Ticket']=NULL
traindataforada['Cabin']=NULL
traindataforada['Surname']=NULL
traindataforada['FamilyID']=NULL



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

mtrainnumeric[is.na(mtrainnumeric)] <- 0
#levelplot(cor(mtrainnumeric,use="pairwise.complete.obs"))
str(mtrainnumeric)




#Setup CV Folds
#returnData=FALSE saves some space
folds=5
repeats=1
myControl <- trainControl(method='cv', number=folds, repeats=repeats, 
                          returnResamp='none', classProbs=TRUE,
                          returnData=FALSE, savePredictions=TRUE, 
                          verboseIter=TRUE, allowParallel=TRUE,
                          summaryFunction=twoClassSummary,
                          index=createMultiFolds(train_yvalues, k=folds, times=repeats))
PP <- c('center', 'scale')

#Train some models
model1 <- train(mtrainnumeric, train_yvalues, method='gbm', trControl=myControl,
                tuneGrid=expand.grid(.n.trees=500, .interaction.depth=15, .shrinkage = 0.01))
model2 <- train(mtrainnumeric, train_yvalues, method='blackboost', trControl=myControl)
model3 <- train(X[train,], Y[train], method='parRF', trControl=myControl)
model4 <- train(X[train,], Y[train], method='mlpWeightDecay', trControl=myControl, trace=FALSE, preProcess=PP)
model5 <- train(X[train,], Y[train], method='knn', trControl=myControl, preProcess=PP)
model6 <- train(X[train,], Y[train], method='earth', trControl=myControl, preProcess=PP)
model7 <- train(X[train,], Y[train], method='glm', trControl=myControl, preProcess=PP)
model8 <- train(X[train,], Y[train], method='svmRadial', trControl=myControl, preProcess=PP)
model9 <- train(X[train,], Y[train], method='gam', trControl=myControl, preProcess=PP)
model10 <- train(X[train,], Y[train], method='glmnet', trControl=myControl, preProcess=PP)

#Make a list of all the models
all.models <- list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10)
names(all.models) <- sapply(all.models, function(x) x$method)
sort(sapply(all.models, function(x) min(x$results$ROC)))

#Make a greedy ensemble - currently can only use RMSE
greedy <- caretEnsemble(all.models, iter=1000L)
sort(greedy$weights, decreasing=TRUE)
greedy$error

#Make a linear regression ensemble
linear <- caretStack(all.models, method='glm', trControl=trainControl(method='cv'))
linear$error

#Predict for test set:
library(caTools)
preds <- data.frame(sapply(all.models, function(x){predict(x, X[!train,], type='prob')[,2]}))
preds$ENS_greedy <- predict(greedy, newdata=X[!train,])
preds$ENS_linear <- predict(linear, newdata=X[!train,], type='prob')[,2]
sort(data.frame(colAUC(preds, Y[!train])))