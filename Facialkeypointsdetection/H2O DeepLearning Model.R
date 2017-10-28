#required Libraries
library(h2o)
library(foreach)
library(doParallel)
library(reshape2)
library(mice)


#data source path and source files
data.dir   <- ''
data.dir   <- '/home/pdsilva/'

train.file <- paste0(data.dir, 'training.csv')
test.file  <- paste0(data.dir, 'test.csv')


#load data from training package
d.train <- read.csv(train.file, stringsAsFactors=F)

#fill data 
d.train.na.fill <- mice(d.train,m=5,maxit=50,meth='pmm',seed=500)
d.train <- complete(d.train.na.fill,1)


#omit na values..filter data
filtered.d.train=na.omit(d.train)

#image data
im.train<-filtered.d.train$Image

# Remove image data from filtered data
filtered.d.train$Image<-NULL

#introduce an ID column for later merger
filtered.d.train$id<-1:dim(filtered.d.train)[1]

#convert image data to integer
im.train <- foreach(im = im.train, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

# Convert it to a data frame
d.im.train<-data.frame(im.train)
# Remove row names
row.names(d.im.train)<-NULL
# Add an ID to this image data
d.im.train$id<-1:dim(d.im.train)[1]

#consolidate the training data
training.data <- data.frame(merge(filtered.d.train,d.im.train,by="id"))
training.data$id<-NULL


#test data
d.test  <- read.csv(test.file, stringsAsFactors=F)
im.test <- foreach(im = d.test$Image, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

#save as dataframe
d.im.test<-data.frame(im.test)

#remove row names
row.names(df)<-NULL

testing.data<-d.im.test

colnames(testing.data) <- colnames(training.data[31:9246])

head(testing.data)
head(training.data)

#save and load data in R .Initial data from source file after cleaning (Omitting the NA's)
save(training.data, testing.data, file='DeepLearningInitial.Rd')

load('DeepLearningInitial.Rd')


summary(training.data[1:30])
install.packages('h2o')

###############################################################################
#Deep Learning Model
###############################################################################

# Start h2o ,decide min and max memory sizes,number of cpu cores to be used (-1 means all cores)
localH2O <- h2o.init(ip = "129.118.104.11", port = 54321, startH2O = FALSE,
                     #max_mem_size = '12g', min_mem_size = '4g', 
                     nthreads = -1)


# Start h2o ,decide min and max memory sizes,number of cpu cores to be used (-1 means all cores)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,
                     max_mem_size = '12g', min_mem_size = '4g', 
                     nthreads = -1)

# Process begins now
Sys.time()

?as.h2o

#Training data
training.data.hex<-as.h2o(localH2O, training.data[,1:9246])


?h2o.deeplearning
#crete H2O models
model_1 <- h2o.deeplearning(x = c(31:9246), y = 1,  training_frame = training.data.hex)
model_2 <- h2o.deeplearning(x = c(1:1,31:9246), y = 2,  training_frame = training.data.hex)
model_3 <- h2o.deeplearning(x = c(1:2,31:9246), y = 3,  training_frame = training.data.hex)
model_4 <- h2o.deeplearning(x = c(1:3,31:9246), y = 4 , training_frame = training.data.hex)
model_5 <- h2o.deeplearning(x = c(1:4,31:9246), y = 5 , training_frame = training.data.hex)
model_6 <- h2o.deeplearning(x = c(1:5,31:9246), y = 6 , training_frame = training.data.hex)
model_7 <- h2o.deeplearning(x = c(1:6,31:9246), y = 7 , training_frame = training.data.hex)
model_8 <- h2o.deeplearning(x = c(1:7,31:9246), y = 8 , training_frame = training.data.hex)
model_9 <- h2o.deeplearning(x = c(1:8,31:9246), y = 9 , training_frame = training.data.hex)
model_10 <- h2o.deeplearning(x = c(1:9,31:9246), y = 10 , training_frame = training.data.hex)
model_11 <- h2o.deeplearning(x = c(1:10,31:9246), y = 11 , training_frame = training.data.hex)
model_12 <- h2o.deeplearning(x = c(1:11,31:9246), y = 12 , training_frame = training.data.hex)
model_13 <- h2o.deeplearning(x = c(1:12,31:9246), y = 13 , training_frame = training.data.hex)
model_14 <- h2o.deeplearning(x = c(1:13,31:9246), y = 14 , training_frame = training.data.hex)
model_15 <- h2o.deeplearning(x = c(1:14,31:9246), y = 15 , training_frame = training.data.hex)
model_16 <- h2o.deeplearning(x = c(1:15,31:9246), y = 16 , training_frame = training.data.hex)
model_17 <- h2o.deeplearning(x = c(1:16,31:9246), y = 17 , training_frame = training.data.hex)
model_18 <- h2o.deeplearning(x = c(1:17,31:9246), y = 18 , training_frame = training.data.hex)
model_19 <- h2o.deeplearning(x = c(1:18,31:9246), y = 19 , training_frame = training.data.hex)
model_20 <- h2o.deeplearning(x = c(1:19,31:9246), y = 20 , training_frame = training.data.hex)
model_21 <- h2o.deeplearning(x = c(1:20,31:9246), y = 21 , training_frame = training.data.hex)
model_22 <- h2o.deeplearning(x = c(1:21,31:9246), y = 22 , training_frame = training.data.hex)
model_23 <- h2o.deeplearning(x = c(1:22,31:9246), y = 23 , training_frame = training.data.hex)
model_24 <- h2o.deeplearning(x = c(1:23,31:9246), y = 24 , training_frame = training.data.hex)
model_25 <- h2o.deeplearning(x = c(1:24,31:9246), y = 25 , training_frame = training.data.hex)
model_26 <- h2o.deeplearning(x = c(1:25,31:9246), y = 26 , training_frame = training.data.hex)
model_27 <- h2o.deeplearning(x = c(1:26,31:9246), y = 27 , training_frame = training.data.hex)
model_28 <- h2o.deeplearning(x = c(1:27,31:9246), y = 28 , training_frame = training.data.hex)
model_29 <- h2o.deeplearning(x = c(1:28,31:9246), y = 29 , training_frame = training.data.hex)
model_30 <- h2o.deeplearning(x = c(1:29,31:9246), y = 30 , training_frame = training.data.hex)

#save and load data in R.All 30 Data models
save(model_1,model_2,model_3,model_4,model_5,model_6,model_7,model_8,model_9,model_10,
     model_11,model_12,model_13,model_14,model_15,model_16,model_17,model_18,model_19,model_20,
     model_21,model_22,model_23,model_24,model_25,model_26,model_27,model_28,model_29,model_30
      , file='DeepLearningModels_rotate.Rd')

load('DeepLearningModels_nafill.Rd')


#use maximum size
memory.size(TRUE)

#Function to refresh data after the prediction
data.refresh <- function(prediction.hex,training.data,testing.data,i) {
  #column name of parameter
  col.name=colnames(training.data)[i]
  #store result as df
  prediction <- as.data.frame(prediction.hex)
  #assign the proper column name
  colnames(prediction) <- col.name
  #add id value to merge
  prediction$id<-1:dim(prediction)[1]
  testing.data$id<-1:dim(testing.data)[1]
  training.data.new <- data.frame(merge(prediction,testing.data,by="id"))
  training.data.new$id <- NULL
  #return data
  return(training.data.new)
}

#predict values for the Testing dataset 
#model 1
testing.data.hex<-as.h2o(localH2O, testing.data)
prediction.hex <- h2o.predict(model_1, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,1)

#model 2
testing.data.hex<-as.h2o(localH2O, testing.data)
prediction.hex <- h2o.predict(model_2, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,2)

#model 3
testing.data.hex<-as.h2o(localH2O, testing.data[,c(2:1,3:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_3, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,3)

#model 4
testing.data.hex<-as.h2o(localH2O, testing.data[,c(3:1,4:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_4, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,4)

#model 5
testing.data.hex<-as.h2o(localH2O, testing.data[,c(4:1,5:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_5, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,5)

#model 6
testing.data.hex<-as.h2o(localH2O, testing.data[,c(5:1,6:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_6, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,6)

#model 7
testing.data.hex<-as.h2o(localH2O, testing.data[,c(6:1,7:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_7, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,7)

#model 8
testing.data.hex<-as.h2o(localH2O, testing.data[,c(7:1,8:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_8, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,8)

#model 9
testing.data.hex<-as.h2o(localH2O, testing.data[,c(8:1,9:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_9, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,9)

#model 10
testing.data.hex<-as.h2o(localH2O, testing.data[,c(9:1,10:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_10, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,10)

#model 11
testing.data.hex<-as.h2o(localH2O, testing.data[,c(10:1,11:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_11, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,11)

#model 12
testing.data.hex<-as.h2o(localH2O, testing.data[,c(11:1,12:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_12, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,12)

#model 13
testing.data.hex<-as.h2o(localH2O, testing.data[,c(12:1,13:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_13, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,13)

#model 14
testing.data.hex<-as.h2o(localH2O, testing.data[,c(13:1,14:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_14, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,14)

#model 15
testing.data.hex<-as.h2o(localH2O, testing.data[,c(14:1,15:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_15, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,15)

#model 16
testing.data.hex<-as.h2o(localH2O, testing.data[,c(15:1,16:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_16, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,16)

#model 17
testing.data.hex<-as.h2o(localH2O, testing.data[,c(16:1,17:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_17, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,17)

#model 18
testing.data.hex<-as.h2o(localH2O, testing.data[,c(17:1,18:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_18, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,18)

#model 19
testing.data.hex<-as.h2o(localH2O, testing.data[,c(18:1,19:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_19, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,19)

#model 20
testing.data.hex<-as.h2o(localH2O, testing.data[,c(19:1,20:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_20, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,20)

#model 21
testing.data.hex<-as.h2o(localH2O, testing.data[,c(20:1,21:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_21, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,21)

#model 22
testing.data.hex<-as.h2o(localH2O, testing.data[,c(21:1,22:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_22, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,22)

#model 23
testing.data.hex<-as.h2o(localH2O, testing.data[,c(22:1,23:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_23, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,23)

#model 24
testing.data.hex<-as.h2o(localH2O, testing.data[,c(23:1,24:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_24, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,24)

#model 25
testing.data.hex<-as.h2o(localH2O, testing.data[,c(24:1,25:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_25, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,25)

#model 26
testing.data.hex<-as.h2o(localH2O, testing.data[,c(25:1,26:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_26, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,26)

#model 27
testing.data.hex<-as.h2o(localH2O, testing.data[,c(26:1,27:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_27, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,27)

#model 28
testing.data.hex<-as.h2o(localH2O, testing.data[,c(27:1,28:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_28, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,28)

#model 29
testing.data.hex<-as.h2o(localH2O, testing.data[,c(28:1,29:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_29, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,29)

#model 30
testing.data.hex<-as.h2o(localH2O, testing.data[,c(29:1,30:dim(testing.data)[2])])
prediction.hex <- h2o.predict(model_30, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,30)

predicted.test.data=testing.data[,c(30:1,31:dim(testing.data)[2])]

#save and load data in R .Predicted Test results
save(predicted.test.data, file='DeepLearningPredictedResults_rotated.Rd')

load('DeepLearningPredictedResults_nafill.Rd')


#Create the submission file to evaluate scores
#capture predictions in proper format
predictions <- data.frame(ImageId = 1:nrow(predicted.test.data))
predictions[2:31]<-predicted.test.data[,1:30] 

#submission in proper format
submission <- melt(predictions, id.vars="ImageId", variable.name="FeatureName", value.name="Location")

#us IDLookuoTable to store in proper format
Id.lookup <- read.csv(paste0(data.dir, 'IdLookupTable.csv'),header=T)
Idlookup_colnames <- names(Id.lookup)
Idlookup_colnames
Id.lookup$Location <- NULL


#store the results in proper submission format
msub <- merge(Id.lookup, submission, all.x=T, sort=F)
nsub <- msub[, Idlookup_colnames]

submit_file  <- paste0(data.dir, 'submission_H2O_deeplearning_rotate.csv')
write.csv(nsub[,c(1,4)], file=submit_file, quote=F, row.names=F)

# End time
Sys.time()

#shutdown h2o
h2o.shutdown(localH2O2, prompt=FALSE)

?h2o.deeplearning
