#############################################################################################
# clean_data.R
#############################################################################################

setwd("~/Desktop/KaggleScripts/FOGNETS/data/")
train <- read.csv("train_micro_2hr.csv")
guel <- read.csv("~/Desktop/KaggleScripts/FOGNETS/data/macro_guelandsidi.csv")
head(guel)
datesmacro<-as.POSIXct(as.character(guel$Local.time.in.Guelmim),format = "%m/%d/%y %H:%M")
timemacro.poslt <- as.POSIXlt(datesmacro);
timemacro.hour <- timemacro.poslt$hour
guel["hour"]=timemacro.hour
str(guel)

gueldates<-as.POSIXct(as.character(train$Date),format = "%m/%d/%y %H:%M")
time.poslt <- as.POSIXlt(dates);
time.hour <- time.poslt$hour
train["hour"]=time.hour
train["X"]=NULL
train$hour=as.factor(train$hour)
str(train)
# Merge Data frames based on DAte

colnames.guel<-names(guel)
colnames.guel[1]="Date"
colnames(guel)=colnames.guel

merged.df=merge(guel, train, by=c('Date','hour'))
str(merged.df)


######################################################################################################
#Time Series
#names(train)
#library(xts)
#x <- xts(train[0:11],order.by=as.Date(train$Date, "%Y-%m-%d %H:%M:%S"))
#str(x)
#class(x)
#summary(x)
######################################################################################################


#Deep Learning
#Random Forest
#GBM
#XGBoost
#GridSearch





