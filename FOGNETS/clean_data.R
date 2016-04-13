#############################################################################################
# clean_data.R
# By : Rudi Kruger

#############################################################################################
setwd("~/Desktop/KaggleScripts/FOGNETS/data/")
train <- read.csv("train5min.csv")
test <- read.csv("test.csv")

str(train[2:10])
dates<-as.POSIXct(as.character(train$X),format = "%Y-%m-%d %H:%M:%S")
train$date<-dates
names(train)
library(xts)
x <- xts(train[2:10],order.by=as.Date(train$date, "%Y-%m-%d %H:%M:%S"))

class(x)
str(x)
