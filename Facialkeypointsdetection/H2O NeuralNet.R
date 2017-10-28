#required Libraries
library(h2o)
library(foreach)
library(doParallel)
library(reshape2)
library(mice)


#data source path and source files
data.dir   <- ''
train.file <- paste0(data.dir, 'training.csv')
test.file  <- paste0(data.dir, 'test.csv')

colnames(d.im.train)
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

#save and load data in R .Initial data from source file after cleaning (Omitting the NA's)
save(training.data, testing.data, file='CustomH2OInitial.Rd')

load('CustomH2OInitial.Rd')

###############################################################################
#Deep Learning Model
###############################################################################

# Start h2o ,decide min and max memory sizes,number of cpu cores to be used (-1 means all cores)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,
                     max_mem_size = '12g', min_mem_size = '4g', nthreads = -1,Xmx = '8g')



###############################################################################
#Model for Prediction Nose value from the Image

Nose.Training.Data=training.data[complete.cases(training.data[,21:22]),c(21:22,31:dim(training.data)[2])]

#Training data
training.data.hex<-as.h2o(localH2O, Nose.Training.Data)

#crete H2O models
Model_Nose_X <- h2o.deeplearning(x = c(3:dim(Nose.Training.Data)[2]), y = 1,  training_frame = training.data.hex,
                                 activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)
Model_Nose_Y <- h2o.deeplearning(x = c(1,3:dim(Nose.Training.Data)[2]), y = 2,  training_frame = training.data.hex,
                                 activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)

###############################################################################
#Model for Prediction Left Eye value from the Image

LEye.Training.Data=training.data[complete.cases(training.data[,c(1:2,21:22)]),c(1:2,21:22,31:dim(training.data)[2])]

#Training data
training.data.hex<-as.h2o(localH2O, LEye.Training.Data)

#crete H2O models
Model_LEye_X <- h2o.deeplearning(x = c(3:dim(LEye.Training.Data)[2]), y = 1,  training_frame = training.data.hex,
                                 activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)
Model_LEye_Y <- h2o.deeplearning(x = c(1,3:dim(LEye.Training.Data)[2]), y = 2,  training_frame = training.data.hex,
                                 activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)


###############################################################################
#Model for Prediction Right Eye value from the Image

REye.Training.Data=training.data[complete.cases(training.data[,c(3:4,1:2,21:22)]),c(3:4,1:2,21:22,31:dim(training.data)[2])]

#Training data
training.data.hex<-as.h2o(localH2O, REye.Training.Data)

#crete H2O models
Model_REye_X <- h2o.deeplearning(x = c(3:dim(REye.Training.Data)[2]), y = 1,  training_frame = training.data.hex,
                                 activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)
Model_REye_Y <- h2o.deeplearning(x = c(1,3:dim(REye.Training.Data)[2]), y = 2,  training_frame = training.data.hex,
                                 activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)


###############################################################################
#Model for Prediction Mouth Bottom value from the Image

MB.Training.Data=training.data[complete.cases(training.data[,c(29:30,3:4,1:2,21:22)]),c(29:30,3:4,1:2,21:22,31:dim(training.data)[2])]

#Training data
training.data.hex<-as.h2o(localH2O, MB.Training.Data)

#crete H2O models
Model_MB_X <- h2o.deeplearning(x = c(3:dim(MB.Training.Data)[2]), y = 1,  training_frame = training.data.hex,
                               activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)
Model_MB_Y <- h2o.deeplearning(x = c(1,3:dim(MB.Training.Data)[2]), y = 2,  training_frame = training.data.hex,
                               activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)

###############################################################################
#Model for Prediction Left Eye Remaining Values
###############################################################################

##########left_eye_inner_corner##########
LEye.IC.Training.Data=training.data[complete.cases(training.data[,c(5:6,29:30,3:4,1:2,21:22)]),c(5:6,29:30,3:4,1:2,21:22,31:dim(training.data)[2])]

#Training data
training.data.hex<-as.h2o(localH2O, LEye.IC.Training.Data)

#crete H2O models
Model_LEye.IC_X <- h2o.deeplearning(x = c(3:dim(LEye.IC.Training.Data)[2]), y = 1,  training_frame = training.data.hex,
                                    activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)
Model_LEye.IC_Y <- h2o.deeplearning(x = c(1,3:dim(LEye.IC.Training.Data)[2]), y = 2,  training_frame = training.data.hex,
                                    activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)

##########left_eye_outer_corner##########
LEye.OC.Training.Data=training.data[complete.cases(training.data[,c(7:8,29:30,3:4,1:2,21:22)]),c(7:8,29:30,3:4,1:2,21:22,31:dim(training.data)[2])]

#Training data
training.data.hex<-as.h2o(localH2O, LEye.OC.Training.Data)

#crete H2O models
Model_LEye.OC_X <- h2o.deeplearning(x = c(3:dim(LEye.OC.Training.Data)[2]), y = 1,  training_frame = training.data.hex,
                                    activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)
Model_LEye.OC_Y <- h2o.deeplearning(x = c(1,3:dim(LEye.OC.Training.Data)[2]), y = 2,  training_frame = training.data.hex,
                                    activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)

##########left_eyebrow_inner_end##########
LEye.EBIE.Training.Data=training.data[complete.cases(training.data[,c(13:14,29:30,3:4,1:2,21:22)]),c(13:14,29:30,3:4,1:2,21:22,31:dim(training.data)[2])]

#Training data
training.data.hex<-as.h2o(localH2O, LEye.EBIE.Training.Data)

#crete H2O models
Model_LEye.EBIE_X <- h2o.deeplearning(x = c(3:dim(LEye.EBIE.Training.Data)[2]), y = 1,  training_frame = training.data.hex,
                                      activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)
Model_LEye.EBIE_Y <- h2o.deeplearning(x = c(1,3:dim(LEye.EBIE.Training.Data)[2]), y = 2,  training_frame = training.data.hex,
                                      activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)

##########left_eyebrow_outer_end##########
LEye.EBOE.Training.Data=training.data[complete.cases(training.data[,c(15:16,29:30,3:4,1:2,21:22)]),c(15:16,29:30,3:4,1:2,21:22,31:dim(training.data)[2])]

#Training data
training.data.hex<-as.h2o(localH2O, LEye.EBOE.Training.Data)

#crete H2O models
Model_LEye.EBOE_X <- h2o.deeplearning(x = c(3:dim(LEye.EBOE.Training.Data)[2]), y = 1,  training_frame = training.data.hex,
                                      activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)
Model_LEye.EBOE_Y <- h2o.deeplearning(x = c(1,3:dim(LEye.EBOE.Training.Data)[2]), y = 2,  training_frame = training.data.hex,
                                      activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)

###############################################################################
#Model for Prediction Right Eye Remaining Values
###############################################################################

##########right_eye_inner_corner##########
REye.IC.Training.Data=training.data[complete.cases(training.data[,c(9:10,29:30,3:4,1:2,21:22)]),c(9:10,29:30,3:4,1:2,21:22,31:dim(training.data)[2])]

#Training data
training.data.hex<-as.h2o(localH2O, REye.IC.Training.Data)

#crete H2O models
Model_REye.IC_X <- h2o.deeplearning(x = c(3:dim(REye.IC.Training.Data)[2]), y = 1,  training_frame = training.data.hex,
                                    activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)
Model_REye.IC_Y <- h2o.deeplearning(x = c(1,3:dim(REye.IC.Training.Data)[2]), y = 2,  training_frame = training.data.hex,
                                    activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)

##########right_eye_outer_corner##########
REye.OC.Training.Data=training.data[complete.cases(training.data[,c(11:12,29:30,3:4,1:2,21:22)]),c(11:12,29:30,3:4,1:2,21:22,31:dim(training.data)[2])]

#Training data
training.data.hex<-as.h2o(localH2O, REye.OC.Training.Data)

#crete H2O models
Model_REye.OC_X <- h2o.deeplearning(x = c(3:dim(REye.OC.Training.Data)[2]), y = 1,  training_frame = training.data.hex,
                                    activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)
Model_REye.OC_Y <- h2o.deeplearning(x = c(1,3:dim(REye.OC.Training.Data)[2]), y = 2,  training_frame = training.data.hex,
                                    activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)

##########right_eyebrow_inner_end##########
REye.EBIE.Training.Data=training.data[complete.cases(training.data[,c(17:18,29:30,3:4,1:2,21:22)]),c(17:18,29:30,3:4,1:2,21:22,31:dim(training.data)[2])]

#Training data
training.data.hex<-as.h2o(localH2O, REye.EBIE.Training.Data)

#crete H2O models
Model_REye.EBIE_X <- h2o.deeplearning(x = c(3:dim(REye.EBIE.Training.Data)[2]), y = 1,  training_frame = training.data.hex,
                                      activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)
Model_REye.EBIE_Y <- h2o.deeplearning(x = c(1,3:dim(REye.EBIE.Training.Data)[2]), y = 2,  training_frame = training.data.hex,
                                      activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)

##########right_eyebrow_outer_end##########
REye.EBOE.Training.Data=training.data[complete.cases(training.data[,c(19:20,29:30,3:4,1:2,21:22)]),c(19:20,29:30,3:4,1:2,21:22,31:dim(training.data)[2])]

#Training data
training.data.hex<-as.h2o(localH2O, REye.EBOE.Training.Data)

#crete H2O models
Model_REye.EBOE_X <- h2o.deeplearning(x = c(3:dim(REye.EBOE.Training.Data)[2]), y = 1,  training_frame = training.data.hex,
                                      activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)
Model_REye.EBOE_Y <- h2o.deeplearning(x = c(1,3:dim(REye.EBOE.Training.Data)[2]), y = 2,  training_frame = training.data.hex,
                                      activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)

###############################################################################
#Model for Prediction Mouth Remaining Values
###############################################################################

##########mouth_left_corner##########
ML.Training.Data=training.data[complete.cases(training.data[,c(23:24,29:30,3:4,1:2,21:22)]),c(23:24,29:30,3:4,1:2,21:22,31:dim(training.data)[2])]

#Training data
training.data.hex<-as.h2o(localH2O, ML.Training.Data)

#crete H2O models
Model_ML_X <- h2o.deeplearning(x = c(3:dim(ML.Training.Data)[2]), y = 1,  training_frame = training.data.hex,
                               activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)
Model_ML_Y <- h2o.deeplearning(x = c(1,3:dim(ML.Training.Data)[2]), y = 2,  training_frame = training.data.hex,
                               activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)

##########mouth_right_corner##########
MR.Training.Data=training.data[complete.cases(training.data[,c(25:26,29:30,3:4,1:2,21:22)]),c(25:26,29:30,3:4,1:2,21:22,31:dim(training.data)[2])]

#Training data
training.data.hex<-as.h2o(localH2O, MR.Training.Data)

#crete H2O models
Model_MR_X <- h2o.deeplearning(x = c(3:dim(MR.Training.Data)[2]), y = 1,  training_frame = training.data.hex,
                               activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)
Model_MR_Y <- h2o.deeplearning(x = c(1,3:dim(MR.Training.Data)[2]), y = 2,  training_frame = training.data.hex,
                               activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)


##########mouth_center_top_lip##########
MT.Training.Data=training.data[complete.cases(training.data[,c(27:28,29:30,3:4,1:2,21:22)]),c(27:28,29:30,3:4,1:2,21:22,31:dim(training.data)[2])]

#Training data
training.data.hex<-as.h2o(localH2O, MT.Training.Data)

#crete H2O models
Model_MT_X <- h2o.deeplearning(x = c(3:dim(MT.Training.Data)[2]), y = 1,  training_frame = training.data.hex,
                               activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)
Model_MT_Y <- h2o.deeplearning(x = c(1,3:dim(MT.Training.Data)[2]), y = 2,  training_frame = training.data.hex,
                               activation = "RectifierWithDropout",hidden = c(100,100,100),epochs = 100)


###############################################################################
###############################################################################

#save and load data in R.All 30 Data models
save(Model_Nose_X,Model_Nose_Y,Model_LEye_X,Model_LEye_Y,Model_REye_X,Model_REye_Y,Model_MB_X,Model_MB_Y,
     Model_LEye.IC_X,Model_LEye.IC_Y,Model_LEye.OC_X,Model_LEye.OC_Y,Model_LEye.EBIE_X,Model_LEye.EBIE_Y,Model_LEye.EBOE_X,Model_LEye.EBOE_Y,
     Model_REye.IC_X,Model_REye.IC_Y,Model_REye.OC_X,Model_REye.OC_Y,Model_REye.EBIE_X,Model_REye.EBIE_Y,Model_REye.EBOE_X,Model_REye.EBOE_Y,
     Model_ML_X,Model_ML_Y,Model_MR_X,Model_MR_Y,Model_MT_X,Model_MT_Y,
     file='CustomH2OModels.Rd')

load('CustomH2OModels.Rd')

#summary(Model_LEye_X)
#?h2o.predict
#?h2o.deeplearning
#?h2o.init
#?as.h2o

h2o.download_pojo('DeepLearningModel__9833b92cb8e1b4ffb9fd653bc8904c3f.java', getwd())
h2o.download_pojo(Model_LEye_Y, getwd())
h2o.download_pojo(Model_REye_X, getwd())
h2o.download_pojo(Model_REye_Y, getwd())
h2o.download_pojo(Model_MB_X, getwd())
h2o.download_pojo(Model_MB_Y, getwd())
h2o.exportHDFS(Model_REye_X, getwd())

h2o.importFile('DeepLearningModel__9833b92cb8e1b4ffb9fd653bc8904c3f.java')

?h2o.loadModel('DeepLearningModel__9833b92cb8e1b4ffb9fd653bc8904c3f.java')
h2o.

h2o.saveModel(Model_LEye_Y, dir=getwd(), name="ModeltoSave", force=FALSE)

testing.data.hex<-as.h2o(localH2O, testing.data,destination_frame='testing.data.hex')
###############################################################################
###############################################################################

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
###############################################################################
#Nose
#Nose Values X
testing.data.hex<-as.h2o(localH2O, testing.data)
prediction.hex <- h2o.predict(Model_Nose_X, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,21)

#Nose Values Y
testing.data.hex<-as.h2o(localH2O, testing.data)
prediction.hex <- h2o.predict(Model_Nose_Y, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,22)
testing.data=testing.data[,c(2,1,3:dim(testing.data)[2])]

###############################################################################
#Left Eye
# Values X
testing.data.hex<-as.h2o(localH2O, testing.data)
prediction.hex <- h2o.predict(Model_LEye_X, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,1)

# Values Y
testing.data.hex<-as.h2o(localH2O, testing.data)
prediction.hex <- h2o.predict(Model_LEye_Y, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,2)
testing.data=testing.data[,c(2,1,3:dim(testing.data)[2])]

###############################################################################
#Right Eye
# Values X
testing.data.hex<-as.h2o(localH2O, testing.data)
prediction.hex <- h2o.predict(Model_REye_X, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,3)

# Values Y
testing.data.hex<-as.h2o(localH2O, testing.data)
prediction.hex <- h2o.predict(Model_REye_Y, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,4)
testing.data=testing.data[,c(2,1,3:dim(testing.data)[2])]

###############################################################################
#Mouth
# Values X
testing.data.hex<-as.h2o(localH2O, testing.data)
prediction.hex <- h2o.predict(Model_MB_X, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,29)

# Values Y
testing.data.hex<-as.h2o(localH2O, testing.data)
prediction.hex <- h2o.predict(Model_MB_Y, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,30)
testing.data=testing.data[,c(2,1,3:dim(testing.data)[2])]

###############################################################################
###############################################################################
testing.data.pattern.fixed=testing.data
###############################################################################
###############################################################################

#Left Eye Remaining
#left_eye_inner_corner
testing.data.pattern=testing.data.pattern.fixed
# Values X
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_LEye.IC_X, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,5)

# Values Y
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_LEye.IC_Y, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,6)
testing.data.pattern=testing.data.pattern[,c(2,1,3:dim(testing.data.pattern)[2])]
testing.data.pattern$id<-1:dim(testing.data.pattern)[1]
testing.data$id<-1:dim(testing.data)[1]
testing.data <- data.frame(merge(testing.data.pattern[,c(1:2,dim(testing.data.pattern)[2])],testing.data,by="id"))
testing.data$id <- NULL

###############################################################################

#left_eye_outer_corner
testing.data.pattern=testing.data.pattern.fixed
# Values X
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_LEye.OC_X, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,7)

# Values Y
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_LEye.OC_Y, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,8)
testing.data.pattern=testing.data.pattern[,c(2,1,3:dim(testing.data.pattern)[2])]
testing.data.pattern$id<-1:dim(testing.data.pattern)[1]
testing.data$id<-1:dim(testing.data)[1]
testing.data <- data.frame(merge(testing.data.pattern[,c(1:2,dim(testing.data.pattern)[2])],testing.data,by="id"))
testing.data$id <- NULL


###############################################################################

#left_eyebrow_inner_end
testing.data.pattern=testing.data.pattern.fixed
# Values X
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_LEye.EBIE_X, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,13)

# Values Y
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_LEye.EBIE_Y, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,14)
testing.data.pattern=testing.data.pattern[,c(2,1,3:dim(testing.data.pattern)[2])]
testing.data.pattern$id<-1:dim(testing.data.pattern)[1]
testing.data$id<-1:dim(testing.data)[1]
testing.data <- data.frame(merge(testing.data.pattern[,c(1:2,dim(testing.data.pattern)[2])],testing.data,by="id"))
testing.data$id <- NULL

###############################################################################

#left_eyebrow_outer_end
testing.data.pattern=testing.data.pattern.fixed
# Values X
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_LEye.EBOE_X, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,15)

# Values Y
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_LEye.EBOE_Y, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,16)
testing.data.pattern=testing.data.pattern[,c(2,1,3:dim(testing.data.pattern)[2])]
testing.data.pattern$id<-1:dim(testing.data.pattern)[1]
testing.data$id<-1:dim(testing.data)[1]
testing.data <- data.frame(merge(testing.data.pattern[,c(1:2,dim(testing.data.pattern)[2])],testing.data,by="id"))
testing.data$id <- NULL


###############################################################################


#Right Eye Remaining
#right_eye_inner_corner
testing.data.pattern=testing.data.pattern.fixed
# Values X
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_REye.IC_X, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,9)

# Values Y
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_REye.IC_Y, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,10)
testing.data.pattern=testing.data.pattern[,c(2,1,3:dim(testing.data.pattern)[2])]
testing.data.pattern$id<-1:dim(testing.data.pattern)[1]
testing.data$id<-1:dim(testing.data)[1]
testing.data <- data.frame(merge(testing.data.pattern[,c(1:2,dim(testing.data.pattern)[2])],testing.data,by="id"))
testing.data$id <- NULL

###############################################################################
#right_eye_outer_corner
testing.data.pattern=testing.data.pattern.fixed
# Values X
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_REye.OC_X, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,11)

# Values Y
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_REye.OC_Y, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,12)
testing.data.pattern=testing.data.pattern[,c(2,1,3:dim(testing.data.pattern)[2])]
testing.data.pattern$id<-1:dim(testing.data.pattern)[1]
testing.data$id<-1:dim(testing.data)[1]
testing.data <- data.frame(merge(testing.data.pattern[,c(1:2,dim(testing.data.pattern)[2])],testing.data,by="id"))
testing.data$id <- NULL

###############################################################################
#right_eyebrow_inner_end
testing.data.pattern=testing.data.pattern.fixed
# Values X
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_REye.EBIE_X, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,17)

# Values Y
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_REye.EBIE_Y, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,18)
testing.data.pattern=testing.data.pattern[,c(2,1,3:dim(testing.data.pattern)[2])]
testing.data.pattern$id<-1:dim(testing.data.pattern)[1]
testing.data$id<-1:dim(testing.data)[1]
testing.data <- data.frame(merge(testing.data.pattern[,c(1:2,dim(testing.data.pattern)[2])],testing.data,by="id"))
testing.data$id <- NULL

###############################################################################
#right_eyebrow_outer_end
testing.data.pattern=testing.data.pattern.fixed
# Values X
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_REye.EBOE_X, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,19)

# Values Y
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_REye.EBOE_Y, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,20)
testing.data.pattern=testing.data.pattern[,c(2,1,3:dim(testing.data.pattern)[2])]
testing.data.pattern$id<-1:dim(testing.data.pattern)[1]
testing.data$id<-1:dim(testing.data)[1]
testing.data <- data.frame(merge(testing.data.pattern[,c(1:2,dim(testing.data.pattern)[2])],testing.data,by="id"))
testing.data$id <- NULL

###############################################################################


#Mouth Remaining
#mouth_left_corner
testing.data.pattern=testing.data.pattern.fixed
# Values X
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_ML_X, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,23)

# Values Y
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_ML_Y, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,24)
testing.data.pattern=testing.data.pattern[,c(2,1,3:dim(testing.data.pattern)[2])]
testing.data.pattern$id<-1:dim(testing.data.pattern)[1]
testing.data$id<-1:dim(testing.data)[1]
testing.data <- data.frame(merge(testing.data.pattern[,c(1:2,dim(testing.data.pattern)[2])],testing.data,by="id"))
testing.data$id <- NULL

###############################################################################
#mouth_right_corner
testing.data.pattern=testing.data.pattern.fixed
# Values X
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_MR_X, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,25)

# Values Y
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_MR_Y, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,26)
testing.data.pattern=testing.data.pattern[,c(2,1,3:dim(testing.data.pattern)[2])]
testing.data.pattern$id<-1:dim(testing.data.pattern)[1]
testing.data$id<-1:dim(testing.data)[1]
testing.data <- data.frame(merge(testing.data.pattern[,c(1:2,dim(testing.data.pattern)[2])],testing.data,by="id"))
testing.data$id <- NULL

###############################################################################
#mouth_center_top_lip
testing.data.pattern=testing.data.pattern.fixed
# Values X
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_MT_X, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,27)

# Values Y
testing.data.pattern.hex<-as.h2o(localH2O, testing.data.pattern)
prediction.hex <- h2o.predict(Model_MT_Y, testing.data.pattern.hex)
testing.data.pattern=data.refresh(prediction.hex,training.data,testing.data.pattern,28)
testing.data.pattern=testing.data.pattern[,c(2,1,3:dim(testing.data.pattern)[2])]
testing.data.pattern$id<-1:dim(testing.data.pattern)[1]
testing.data$id<-1:dim(testing.data)[1]
testing.data <- data.frame(merge(testing.data.pattern[,c(1:2,dim(testing.data.pattern)[2])],testing.data,by="id"))
testing.data$id <- NULL

###############################################################################
###############################################################################

#testing.data$mouth_center_top_lip_y <- NULL

predicted.test.data=testing.data[,c('left_eye_center_x','left_eye_center_y','right_eye_center_x','right_eye_center_y','left_eye_inner_corner_x','left_eye_inner_corner_y',
                     'left_eye_outer_corner_x','left_eye_outer_corner_y','right_eye_inner_corner_x','right_eye_inner_corner_y','right_eye_outer_corner_x',
                     'right_eye_outer_corner_y','left_eyebrow_inner_end_x','left_eyebrow_inner_end_y','left_eyebrow_outer_end_x','left_eyebrow_outer_end_y',
                     'right_eyebrow_inner_end_x','right_eyebrow_inner_end_y','right_eyebrow_outer_end_x','right_eyebrow_outer_end_y','nose_tip_x',
                     'nose_tip_y','mouth_left_corner_x','mouth_left_corner_y','mouth_right_corner_x','mouth_right_corner_y','mouth_center_top_lip_x',
                     'mouth_center_top_lip_y','mouth_center_bottom_lip_x','mouth_center_bottom_lip_y')]

#save and load data in R .Predicted Test results
save(testing.data.pattern.fixed,testing.data,predicted.test.data, file='CustomH2OPredictedResults2.Rd')

load('CustomH2OPredictedResults2.Rd')

summary(predicted.test.data)

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

submit_file  <- paste0(data.dir, 'submission_H2O_deeplearning_Custom2.csv')
write.csv(nsub[,c(1,4)], file=submit_file, quote=F, row.names=F)

# End time
Sys.time()

#shutdown h2o
h2o.shutdown(localH2O, prompt=FALSE)


