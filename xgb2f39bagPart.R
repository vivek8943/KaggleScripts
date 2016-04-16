require(xgboost)
require(data.table)
set.seed(1)

sumabs = function(x) {
  sum(abs(x),na.rm=T)
}

train = fread('/home/mikeskim/Desktop/kaggle/winton/train.csv',data.table=F,header=T)
test = fread('/home/mikeskim/Desktop/kaggle/winton/test_2.csv',data.table=F,header=T)

train$Id=NULL; test$Id=NULL

tmpTrain = apply(train[,28:146],1,sumabs)
tmpTest = apply(test[,28:146],1,sumabs)

train = cbind(tmpTrain,train)
test = cbind(tmpTest,test)

tmpTrain = t(apply(train[,29:147],1,diff))
tmpTest = t(apply(test[,29:147],1,diff))

train = cbind(tmpTrain,train)
test = cbind(tmpTest,test)

#Feature selection using KS test with 0.015 D cutoff.
tmpJ = 1:ncol(test)
ksMat = NULL
for (j in tmpJ) {
  print(j)
  ksMat = rbind(ksMat, cbind(j, ks.test(train[,j],test[,j])$statistic))
}

ksMat2 = ksMat[ksMat[,2]<0.015,]
feats = as.numeric(ksMat2[,1])



# Start XGBoost params set via local CV.
param <- list(
  "objective" = "reg:linear",#  "objective" = "count:poisson",
  "bst:eta" = 0.009,
  "bst:max_depth" = 9 , #8,10 worse
  "subsample" = 0.900, #0.7 worse
  "colsample_bytree" = 0.40 ,# 0.5 worse  "gamma" = 0.5,
  "min_child_weight" = 6,#5 worse
  "nthread" = 6)

trainY = train$Ret_PlusOne
xgtrain = xgb.DMatrix(as.matrix(train[,feats]), label = trainY, missing = NA)
xgtest = xgb.DMatrix(as.matrix(test[,feats]), missing=NA)


# Do bagging
tmpP = rep(0,nrow(test))
for (j in 1:30) {
set.seed(j+200)
print(j)
bst = xgb.train(param=param, data=xgtrain, nrounds=1200)

tmpP = tmpP + predict(bst,xgtest)
}

tmpP = tmpP/j



# Make submission based upon prior sub, basically the same approach for Ret_PlusOne and Ret_PlusTwo
# xgb2f32bag.csv is Ret_PlusTwo, others are mostly just medians / scaled medians.
sub1 = fread('/home/mikeskim/Desktop/kaggle/winton/xgb2f32bag.csv',data.table=F,header=T)
index = seq(61,nrow(sub1),by=62)
alpha = 0.90
sub1[index,2] = alpha*sub1[index,2] + (1-alpha)*tmpP

write.csv(sub1, file='/home/mikeskim/Desktop/kaggle/winton/xgb2f39bag.csv', quote=FALSE,row.names=FALSE)



