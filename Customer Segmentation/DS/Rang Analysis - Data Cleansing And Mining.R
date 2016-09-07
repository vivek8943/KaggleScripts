setwd('C:/Users/Priyank/OneDrive/Documents/MIS/Rang Technology/Source')


#library('ggplot2') # visualization
#library('ggthemes') # visualization
library('mice') # imputation
#library('randomForest') # classification algorithm
library('dplyr') # data manipulation


training.data <- read.csv('Train.csv', stringsAsFactors = F)
testing.data <- read.csv('Test.csv', stringsAsFactors = F)

dim(training.data)


training.data  <- bind_rows(training.data, testing.data)

summary(training.data)
summary(testing.data)


##Data set Cleansing and Mining
#Cust_status (Old/New) -> Replace with 0 and 1
#Sex
training.data$BooleanCust_Status <- 0

for(i in seq(from=1,to=length(training.data$BooleanCust_Status))){
  if(training.data$Cust_status[i] == 'New'){
    training.data$BooleanCust_Status[i] <- 1
  }
}
training.data[,c('Cust_status','BooleanCust_Status')]


#Replace

training.data$Cust_status <- training.data$BooleanCust_Status
training.data$BooleanCust_Status <- NULL

#Trans24,Trans25,Trans26,Trans27 ->(Enabe,Not-Enable) as 0 and 1
#Trans24
training.data$BooleanTrans24 <- 0

for(i in seq(from=1,to=length(training.data$BooleanTrans24))){
  if(training.data$Trans24[i] == 'Enable'){
    training.data$BooleanTrans24[i] <- 1
  }
}
training.data[,c('Trans24','BooleanTrans24')]

training.data$Trans24 <- training.data$BooleanTrans24
training.data$BooleanTrans24 <- NULL


#Trans25
training.data$BooleanTrans25 <- 0

for(i in seq(from=1,to=length(training.data$BooleanTrans25))){
  if(training.data$Trans25[i] == 'Enable'){
    training.data$BooleanTrans25[i] <- 1
  }
}
training.data[,c('Trans25','BooleanTrans25')]

training.data$Trans25 <- training.data$BooleanTrans25
training.data$BooleanTrans25 <- NULL


#Trans26
training.data$BooleanTrans26 <- 0

for(i in seq(from=1,to=length(training.data$BooleanTrans26))){
  if(training.data$Trans26[i] == 'Enable'){
    training.data$BooleanTrans26[i] <- 1
  }
}
training.data[,c('Trans26','BooleanTrans26')]

training.data$Trans26 <- training.data$BooleanTrans26
training.data$BooleanTrans26 <- NULL

#Trans27
training.data$BooleanTrans27 <- 0

for(i in seq(from=1,to=length(training.data$BooleanTrans27))){
  if(training.data$Trans27[i] == 'Enable'){
    training.data$BooleanTrans27[i] <- 1
  }
}
training.data[,c('Trans27','BooleanTrans27')]

training.data$Trans27 <- training.data$BooleanTrans27
training.data$BooleanTrans27 <- NULL




#active_customer boolean 0 or 1

#Factor Variables
factor_vars <- c('Cust_status','Trans24','Trans25','Trans26','Trans27','Promotion37','Active_Customer')
training.data[factor_vars] <- lapply(training.data[factor_vars], function(x) as.factor(x))



#Filling NA Values
training.data.fillna <- mice(training.data[,c(1:256)])
training.data.mice <- complete(training.data.fillna,1)

#Seperate Training and Testinf Data
testing.data.new <- training.data.mice[c(25767:36808),]

training.data.new <- cbind(training.data.mice[c(1:25766),],training.data$Active_Customer)
names(training.data.new)[names(training.data.new)=="training.data$Active_Customer"] <- "Active_Customer"


testing.data <- testing.data.new
training.data <- training.data.new

#save and load data in R .Initial data from source file
save(testing.data,training.data,file='RDFiles/RTImputation_TrainAndTest.Rd')

load('RDFiles/RTImputation_TrainAndTest.Rd')

summary(training.data.mice)


