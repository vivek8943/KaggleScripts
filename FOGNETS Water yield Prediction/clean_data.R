#############################################################################################
# clean_data.R
#############################################################################################

setwd("~/Desktop/KaggleScripts/FOGNETS/data/")
train <- read.csv("train_micro_2hr.csv")
test <- read.csv("test_micro_2hr.csv")
guel <- read.csv("~/Desktop/KaggleScripts/FOGNETS/data/macro_guelandsidi.csv")
head(guel)
datesmacro<-as.POSIXct(as.character(guel$Local.time.in.Guelmim),format = "%y-%m-%d %H:%M:%S")
timemacro.poslt <- as.POSIXlt(datesmacro);
timemacro.hour <- timemacro.poslt$hour
guel["hour"]=timemacro.hour



f <- ~Td+Ff+c+DD+gusts_ms+wind_dir+leafwet450_min
i <- mnimput(f,train,eps=1e-3,ts=TRUE, method="spline")
a=(predict(i))





gueldates<-as.POSIXct(as.character(train$Date),format = "%m/%d/%y %H:%M")
time.poslt <- as.POSIXlt(gueldates);
time.hour <- time.poslt$hour
train["hour"]=time.hour
train["X"]=NULL
train$hour=as.factor(train$hour)
str(train)

# Merge Data frames based on DAte

colnames.guel<-names(guel)
colnames.guel[1]="Date"
colnames(guel)=colnames.guel

library(dplyr)
merged.df <- left_join(train, guel, by="Date")
head(merged.df)
str(merged.df)

sapply(train, function(x) sum(is.na(x)))
sapply(merged.df, function(x) sum(is.na(x)))


set.seed(415)
colnames.train=colnames(train)
remove<-c("Date","yield")
colnames.train.x=setdiff(colnames.train, remove)
library(randomForest)
train["leafwet460_min"]=NULL
train <- na.omit(train)
fit <- randomForest(yield ~ percip_mm+humidity  +temp  +leafwet450_min+ leafwet_lwscnt
                   + gusts_ms  +    wind_dir    +   wind_ms,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
varImpPlot(fit)



test.nna <- na.omit(test)
test.nna$yield <- predict(fit, newdata=test.nna)
test.nna[test.nna$yield < .0001,] #optional to zero calc
str(test.nna)
test$yield
test[test.nna$index,]$yield <- test.nna$yield
reordered_data_frame <- shuffled_data_frame[row.names(submission),]


Prediction <- predict(fit, test)
test$yield=Prediction
length(test[,1])
test[test$yield < .0001,]





write.csv(test,"pred.csv")

######################################################################################################
#Time Series
#names(train)
#library(xts)
#x <- xts(train[0:11],order.by=as.Date(train$Date, "%Y-%m-%d %H:%M:%S"))
#str(x)
#class(x)
#summary(x)
######################################################################################################

library(h2o)
## Create an H2O cloud 
h2o.init(
  nthreads=-1,            ## -1: use all available threads
  max_mem_size = "2G")    ## specify the memory size for the H2O cloud
h2o.removeAll() # Clean slate - just in case the cluster was already running

h20.train<-as.h2o(trainmice) 
train<-h20.train
h20.test<-as.h2o(testmice)  
test<-h20.test



#Deep Learning
#Random Forest
#GBM
#XGBoost
#GridSearch





