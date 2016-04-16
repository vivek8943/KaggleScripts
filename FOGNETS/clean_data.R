#############################################################################################
# clean_data.R
#############################################################################################

setwd("~/Desktop/KaggleScripts/FOGNETS/data/")
train <- read.csv("train_micro_2hr.csv")
test <- read.csv("test.csv")

length(train)
head(train)


dates<-as.POSIXct(as.character(train$Date),format = "%m/%d/%y %H:%M")

time.poslt <- as.POSIXlt(dates);
time.hour <- time.poslt$hour
train["hour"]=time.hour
train["X"]=NULL
as.factor(train["hour"])
str(train)

names(train)
library(xts)
x <- xts(train[0:11],order.by=as.Date(train$Date, "%Y-%m-%d %H:%M:%S"))
str(x)
class(x)
summary(x)



