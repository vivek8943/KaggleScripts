#Imports

library(imputeTS)

#Load Data

setwd("~/Desktop/KaggleScripts/FOGNETS/data/")
train <- read.csv("train_2h.csv")
test <- read.csv("test_2h.csv")
subm <- read.csv("submission_format.csv")


#Check Missing

sapply(train, function(x) sum(is.na(x))) 
sapply(test, function(x) sum(is.na(x))) 


#combine DS
test$yield=NA
combined.data <- rbind(train, test)

yieldlag=slide(combined.data,Var = 'yield',slideBy = -1)
head(yieldlag)
combined.data$yieldlag=yieldlag$`yield-1`
combined.data$yieldlag[is.na(combined.data$yieldlag)]<-0

#Check Datasets
str(test)
str(train)
str(combined.data)


#Add Hour,Day,Month,Year to datasets!!

combined.data$hour=format(as.POSIXct(combined.data$X, format="%Y-%m-%d %H:%M"), format="%H")
combined.data$month=format(as.POSIXct(combined.data$X, format="%Y-%m-%d %H:%M"), format="%m")
combined.data$day=format(as.POSIXct(combined.data$X, format="%Y-%m-%d %H:%M"), format="%d")
combined.data$year=format(as.POSIXct(combined.data$X, format="%Y-%m-%d %H:%M"), format="%y")
#verify formats
str(combined.data)

#Convert Hour,Day,Month,Year  to Factors

combined.data$year <- as.factor(combined.data$year)
combined.data$month <- as.factor(combined.data$month)
combined.data$day <- as.factor(combined.data$day)
combined.data$hour <- as.factor(combined.data$hour)

str(combined.data)
# Remove yield to impute
combined.data.yield=combined.data$yield
combined.data$yield=NULL

#Impute timeseries Missing values
for(i in seq(from=2,to=dim(combined.data)[2])){
  x <- as.numeric(combined.data[,i])
  combined.data[,i] <- na.interpolation(x)
}

############### Kalman FIlter
#for(i in seq(1:9)){trainimp[,i] = na.kalman(model="auto.arima",trainimp[,i])
#print(i)}
###############

#Add yield back to combined data
combined.data$yield=combined.data.yield
combined.data.yield=NULL

#divide training and Test

test.imputed <- subset(combined.data, is.na(combined.data$yield))
train.imputed <- subset(combined.data, !is.na(combined.data$yield))
head(test.imputed)
head(train.imputed)
test.imputed$yield=0

## Save datasets as RD
save(train.imputed,test.imputed,file='BasicDataSet_FNM.Rd')

########################################################################
########################################################################


#### Laod Guelmin Data
Guelmi.data <-  read.csv('guel_macro.csv', stringsAsFactors = F,na.strings = c("", " "))
colnames(Guelmi.data)
str(Guelmi.data)

#Check Missing values
sapply(Guelmi.data, function(x) sum(is.na(x))) 



#Make them dummy
Guelmi.data$ff10[is.na(Guelmi.data$ff10)] <- 0
Guelmi.data$WW[is.na(Guelmi.data$WW)] <- 'Unknown'
Guelmi.data$W.W.[is.na(Guelmi.data$W.W.)] <- 'Unknown'


str(Guelmi.data)
#Factor Variables
factors <- c('DD','c','WW','W.W.','VV','Ff','ff10')
Guelmi.data[factors] <- lapply(Guelmi.data[factors], function(x) as.factor(x))
summary(Guelmi.data)



#Impute
for(i in seq(from=2,to=dim(Guelmi.data)[2])){
  x <- as.numeric(Guelmi.data[,i])
  Guelmi.data[,i] <- na.interpolation(x)
}

# SIDI data LOading

Sidi.data <-  read.csv('sidi_mi.csv', stringsAsFactors = F,na.strings = c("", " "))
colnames(Sidi.data)
summary(Sidi.data)
#makeem Dummy
Sidi.data$WW[is.na(Sidi.data$WW)] <- 'Unknown'
Sidi.data$W1[is.na(Sidi.data$W1)] <- 'Unknown'
Sidi.data$W2[is.na(Sidi.data$W2)] <- 'Unknown'
Sidi.data$E. <- NULL
Sidi.data$sss <- NULL 
Sidi.data$Pa <- NULL 
Sidi.data$Po<- NULL 

#FActorize Cats

factor_vars <- c('DD','Ff','N','WW','W1','W2','Cl','Nh',
                 'H','Cm','Ch','tR','E','Tg','RRR')
Sidi.data[factor_vars] <- lapply(Sidi.data[factor_vars], function(x) as.factor(x))

for(i in seq(from=2,to=dim(Sidi.data)[2])){
  x <- ts(Sidi.data[,2])
  Sidi.data[,i] <- na.interpolation(x)
}
save(Sidi.data,Guelmi.data,file='FNAImputed.Rd')

### Combine DataSets

colnames(Sidi.data)[1]='X'
colnames(Guelmi.data)[1]='X'
Sidi.guel<- data.frame(merge(Sidi.data,Guelmi.data,by="X",all.x = T))
Sidi.guel.fognet.train <- data.frame(merge(train.imputed,Sidi.guel,by="X",all.x = T))
Sidi.guel.fognet.test <- data.frame(merge(test.imputed,Sidi.guel,by="X",all.x = T))


save(Sidi.guel.fognet.train,Sidi.guel.fognet.test,Guelmi.data,file='FNAImputedaddedFognet.Rd')





