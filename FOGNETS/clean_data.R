#############################################################################################
# clean_data.R
# By : Rudi Kruger

#############################################################################################
setwd("~/Desktop/KaggleScripts/FOGNETS/data/")
train <- read.csv("train_micro_2hr.csv")
test <- read.csv("test.csv")
length(train)
head(train)

dates<-as.POSIXct(as.character(train$Date),format = "%m/%d/%y %H:%M")
train$Date<-dates
names(train)
library(xts)
x <- xts(train[0:11],order.by=as.Date(train$Date, "%Y-%m-%d %H:%M:%S"))
str(x)
x[12:1]
