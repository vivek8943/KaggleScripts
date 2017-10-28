#required Libraries
library(h2o)
library(foreach)
library(doParallel)
library(reshape2)
library(mice)
library(data.table)


#data source path and source files
data.dir   <- ''
train.file <- paste0(data.dir, 'training.csv')
test.file  <- paste0(data.dir, 'test.csv')
idlookup.file <- paste0(data.dir, 'IdLookupTable.csv')

#load the Training data
#load data from training package
d.train <- read.csv(train.file, stringsAsFactors=F)

#image data
im.train<-d.train$Image

# Remove image data from  data
d.train$Image<-NULL

#introduce an ID column for later merger
d.train$id<-1:dim(d.train)[1]

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
training.data <- data.frame(merge(d.train,d.im.train,by="id"))
training.data$id<-NULL

dim(training.data)
summary(training.data[1:30])

#################################################################
#Rotate and multiply
#Multiply the training data
training.data.flip.im <-NULL
training.data.flip.points <- NULL
for (i in 1:dim(training.data)[1] ) {
  
  string=paste('Process Started for Row :',i)
  print (string)
  
  im <- matrix(as.integer(training.data[i,31:9246]), nrow=96, ncol=96)
  training.data.flip.im = rbind(training.data.flip.im,as.data.frame(matrix(im, ncol = 9216, byrow = TRUE)))
  
  points <-training.data[i,1:30]
  training.data.flip.points = rbind(training.data.flip.points,points)
  
  #Flip
  im <- im[nrow(im):1,]
  training.data.flip.im = rbind(training.data.flip.im,as.data.frame(matrix(im, ncol = 9216, byrow = TRUE)))
  
  for (k in seq(from = 1, to = 30, by = 2)) {
    points[,k]<-96-points[,k]
  }
  training.data.flip.points = rbind(training.data.flip.points,points)
  
}


training.data.flip.im$id<-1:dim(training.data.flip.im)[1]
training.data.flip.points$id<-1:dim(training.data.flip.points)[1]
training.data.flip <- data.frame(merge(training.data.flip.points,training.data.flip.im,by="id"))
training.data.flip$id <- NULL



#save and load data in R .Initial data from source file after cleaning (Omitting the NA's)
save(training.data.flip,training.data.flip.im,training.data.flip.points ,file='Filp.Rd')

#load('Resize.Rd')
training.data<- training.data.flip


#################################################################

#Resize the Training Data

#Resize function
rescale <- function(x, newrange=range(x)){
  xrange <- range(x)
  mfac <- (newrange[2]-newrange[1])/(xrange[2]-xrange[1])
  newrange[1]+(x-xrange[1])*mfac
}

ResizeMat <- function(mat, ndim=dim(mat)){
  if(!require(fields)) stop("`fields` required.")
  
  # input object
  odim <- dim(mat)
  obj <- list(x= 1:odim[1], y=1:odim[2], z= mat)
  
  # output object
  ans <- matrix(NA, nrow=ndim[1], ncol=ndim[2])
  ndim <- dim(ans)
  
  # rescaling
  ncord <- as.matrix(expand.grid(seq_len(ndim[1]), seq_len(ndim[2])))
  loc <- ncord
  loc[,1] = rescale(ncord[,1], c(1,odim[1]))
  loc[,2] = rescale(ncord[,2], c(1,odim[2]))
  
  # interpolation
  ans[ncord] <- interp.surface(obj, loc)
  
  ans
}


#get resized data
training.data.resize.im <-NULL
training.data.resize.points <- NULL
for (i in 1:dim(training.data)[1] ) {
  
  string=paste('Process Started for Row :',i)
  print (string)
  
  im <- matrix(as.integer(training.data[i,31:9246]), nrow=96, ncol=96)
  im.48 <-ResizeMat(im, c(48,48))
  training.data.resize.im = rbind(training.data.resize.im,as.data.frame(matrix(im.48, ncol = 2304, byrow = TRUE)))
  
  point <- training.data[i,1:30]
  training.data.resize.points = rbind(training.data.resize.points,(point/2))
}


training.data.resize.im$id<-1:dim(training.data.resize.im)[1]
training.data.resize.points$id<-1:dim(training.data.resize.points)[1]
training.data.resize <- data.frame(merge(training.data.resize.points,training.data.resize.im,by="id"))
training.data.resize$id <- NULL


#save and load data in R .Initial data from source file after cleaning (Omitting the NA's)
save(training.data.resize,training.data.resize.im,training.data.resize.points ,file='Resize.Rd')

#load('Resize.Rd')
training.data<- training.data.resize

#Analyse the Test Data Format to be send
#IDLookuoTable
Id.lookup <- read.csv(idlookup.file,header=T)
Idlookup_colnames <- names(Id.lookup)
Idlookup_colnames
Id.lookup$Location <- 1

#Identify which columns to populate for the Test data
test.data.analysis <- dcast(Id.lookup[,2:3], ImageId~FeatureName)
test.data.analysis <- test.data.analysis[,c(1,20,21,2:19,22:31)]
test.data.analysis$Concat <- do.call(paste, c(test.data.analysis[,c(2:31)], sep = ",")) 
test.data.analysis$Concat <- gsub(",NA", "", test.data.analysis$Concat)
test.model.format <- unique(test.data.analysis$Concat)

#add model info to the training data
model.training.data <- training.data[1:30]
model.training.data$ImageId<-1:dim(model.training.data)[1]

for (i in 1:dim(model.training.data)[1]){
  for (j in 1:(dim(model.training.data)[2]-1)){
    if (is.na(model.training.data[i,j])==FALSE) {
      model.training.data[i,j] <- colnames(model.training.data)[j]
    }
  }
}

model.training.data <- model.training.data[,colnames(model.training.data[,c(31,21,22,1:20,23:30)])]
model.training.data$Concat <- do.call(paste, c(model.training.data[,c(2:31)], sep = ",")) 
model.training.data$Concat <- gsub(",NA", "", model.training.data$Concat)

training.data$Format <- model.training.data$Concat


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

#resize the testing data
#get resized data
testing.data.resize <-NULL
for (i in 1:dim(testing.data)[1] ) {
  
  string=paste('Process Started for Row :',i)
  print (string)
  
  im <- matrix(as.integer(testing.data[i,1:9216]), nrow=96, ncol=96)
  im.48 <-ResizeMat(im, c(48,48))
  testing.data.resize = rbind(testing.data.resize,as.data.frame(matrix(im.48, ncol = 2304, byrow = TRUE)))
  
}


#save and load data in R .Initial data from source file after cleaning (Omitting the NA's)
save(testing.data.resize,file='Resize_Test.Rd')

#load('Resize_Test.Rd')

testing.data<- testing.data.resize


testing.data$ImageId<-1:dim(testing.data)[1]
testing.data <- data.frame(merge(testing.data,test.data.analysis[,c('ImageId','Concat')],by="ImageId"))

##Assign proper model numbers to test and train data
testing.data$Model <- foreach(Format = testing.data$Concat, .combine=rbind) %dopar% {
  if (length(unlist(strsplit(Format, ","))) > 8 ) {
    1
  }
  else {
    2
  }
}

training.data$Model <- foreach(Format = training.data$Format, .combine=rbind) %dopar% {
  if(length(unlist(strsplit(Format, ","))) > 8 ) {
    1
  }
  else{
    2
  }
}

#training.data[7049,c(1:30,9247,9248)]
#d.train[1739,]
#summary(training.data[,c(1:30,9247,9248)])
#save and load data in R .Initial data from source file after cleaning (Omitting the NA's)
save(training.data,testing.data ,file='H2O_Dynamic_Flip_Initial.Rd')

#load('H2O_Dynamic_Initial.Rd')
#load('Exploded_Training_Data_Merged.Rd')
#summary(training.data[1:30])

#####################################################################################################################

# Start h2o ,decide min and max memory sizes,number of cpu cores to be used (-1 means all cores)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,
                     max_mem_size = '12g', min_mem_size = '4g', nthreads = -1)

#h2o.shutdown(localH2O, prompt=FALSE)

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


#Model Sepration

training.data.model1 <- training.data[training.data$Model==1,1:2334]
training.data.model2 <- training.data[training.data$Model==2,1:2334]

testing.data.model1 <- testing.data[testing.data$Model==1,1:2305]
testing.data.model2 <- testing.data[testing.data$Model==2,1:2305]


colnames(testing.data.model1) <- c('ImageId',colnames(training.data[,31:2334]))
colnames(testing.data.model2) <- c('ImageId',colnames(training.data[,31:2334]))

predicted.testing.data1 <- testing.data.model1
predicted.testing.data2 <- testing.data.model2

#?h2o.deeplearning
#?h2o.gbm

#Model 1

testing.data.model1.hex<-as.h2o(localH2O, testing.data.model1[,2:2305])

for (i in 1:30) {
  
  string=paste('Process Started for Model 1 Column :',i)
  print (string)
  ModelString<-paste0('Model1_',i)
  
  #create the h2o frames
  training.data.model1.hex<-as.h2o(localH2O, training.data.model1[complete.cases(training.data.model1[,i]),])

  
  #create model
  H2o.Model1 <- h2o.deeplearning(x = c(31:2334), y = i,  training_frame = training.data.model1.hex,
                                 model_id=ModelString,l1=1e-5,input_dropout_ratio=0.2,
                                 hidden=c(256,256,256),epochs = 110,
                                 activation = "RectifierWithDropout")
  

  #predict Model
  prediction.hex <- h2o.predict(H2o.Model1, testing.data.model1.hex)
  #Append results
  predicted.testing.data1=data.refresh(prediction.hex,training.data,predicted.testing.data1,i)
}

predicted.testing.data1=predicted.testing.data1[,c(30:1,31:dim(predicted.testing.data1)[2])]


#Model 2
#create the h2o frames

testing.data.model2.hex<-as.h2o(localH2O, testing.data.model2[,2:2305])

for (i in 1:30) {
  
  string=paste('Process Started for Model 2 Column :',i)
  print (string)
  ModelString<-paste0('Model2_',i)
  
  training.data.model2.hex<-as.h2o(localH2O, training.data.model2[complete.cases(training.data.model2[,i]),])
  
  if (i==1|i==2|i==3|i==4|i==21|i==22|i==29|i==30) {
    #create model
    H2o.Model2 <- h2o.deeplearning(x = c(31:2334), y = i,  training_frame = training.data.model2.hex,
                                   model_id=ModelString,l1=1e-5,input_dropout_ratio=0.2,
                                   hidden=c(256,256,256),epochs = 110,
                                   activation = "RectifierWithDropout")
    #predict Model
    prediction.hex <- h2o.predict(H2o.Model2, testing.data.model2.hex)
    #Append results
    predicted.testing.data2=data.refresh(prediction.hex,training.data,predicted.testing.data2,i)
  }
  else {
    
    col.name=colnames(training.data)[i]
    predicted.testing.data2$default <-0
    colnames(predicted.testing.data2) <- c(colnames(predicted.testing.data2[,1:(dim(predicted.testing.data2)[2]-1)]),col.name)
  
  }
  
}

#save and load data in R .Initial data from source file after cleaning (Omitting the NA's)
save(predicted.testing.data1,predicted.testing.data2 ,file='H2O_Dynamic_Flip_Predictions.Rd')

#load('H2O_Dynamic_Predictions.Rd')


#colnames(predicted.testing.data1)[1:100]
#colnames(predicted.testing.data1[,1:2]) <- c('left_eye_center_x','left_eye_center_y')

#predicted.testing.data1111 <- predicted.testing.data1[,c(1:30,90)]
#predicted.testing.data.temp <- rbind(predicted.testing.data1111,predicted.testing.data2[,colnames(predicted.testing.data1111)])


predicted.testing.data.temp <- rbind(predicted.testing.data1,predicted.testing.data2[,colnames(predicted.testing.data1)])


predicted.testing.data <- NULL
predicted.testing.data$ImageId <- 1:dim(testing.data)[1]
predicted.testing.data <- data.frame(merge(predicted.testing.data,predicted.testing.data.temp,by="ImageId"))


colnames(predicted.testing.data)[1:35]

predicted.test.data <- predicted.testing.data[,1:31]

tail(predicted.test.data)

#Create the submission file to evaluate scores
#capture predictions in proper format
predictions <- data.frame(ImageId = 1:nrow(predicted.test.data))
predictions[2:31]<-predicted.test.data[,2:31] 

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

nsub$Location <- nsub$Location*2

submit_file  <- paste0(data.dir, 'submission_H2O_deeplearning_2Model_Flip-48x48.csv')
write.csv(nsub[,c(1,4)], file=submit_file, quote=F, row.names=F)
