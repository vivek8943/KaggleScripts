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

training.data <- training.data.explode


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

colnames(model.training.data)


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

testing.data$ImageId<-1:dim(testing.data)[1]
testing.data <- data.frame(merge(testing.data,test.data.analysis[,c('ImageId','Concat')],by="ImageId"))

##Assign proper model numbers to test and train data
testing.data$Model <- foreach(Format = testing.data$Concat, .combine=rbind) %dopar% {
  if (length(unlist(strsplit(Format, ","))) == 30 ) {
    1
  }
  else if (length(unlist(strsplit(Format, ","))) == 8 ) {
    2
  }
  else 3
}

training.data$Model <- foreach(Format = training.data$Format, .combine=rbind) %dopar% {
  if(length(unlist(strsplit(Format, ","))) == 30 ) {
    1
  }
  else if (length(unlist(strsplit(Format, ","))) == 8 ) {
    2
  }
  else 3
}

#training.data[7049,c(1:30,9247,9248)]
#d.train[1739,]
#summary(training.data[,c(1:30,9247,9248)])
#save and load data in R .Initial data from source file after cleaning (Omitting the NA's)
save(training.data,testing.data ,file='H2O_Dynamic_Initial.Rd')

load('H2O_Dynamic_Initial.Rd')
load('Exploded_Training_Data_Merged.Rd')
summary(training.data[1:30])

#####################################################################################################################

# Start h2o ,decide min and max memory sizes,number of cpu cores to be used (-1 means all cores)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,
                     max_mem_size = '12g', min_mem_size = '4g', nthreads = -1)

h2o.shutdown(localH2O, prompt=FALSE)

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


H2o.Model2 <- h2o.deeplearning(x = c(31:9246),  training_frame = training.data.model1.hex,autoencoder = T)
#Model Sepration

training.data.model1 <- training.data[training.data$Model==1,1:9246]
training.data.model2 <- training.data[training.data$Model==2,1:9246]



testing.data.model1 <- testing.data[testing.data$Model==1,1:9217]
testing.data.model2 <- testing.data[testing.data$Model==2,1:9217]


colnames(testing.data.model1) <- c('ImageId',colnames(training.data[,31:9246]))
colnames(testing.data.model2) <- c('ImageId',colnames(training.data[,31:9246]))

predicted.testing.data1 <- testing.data.model1
predicted.testing.data2 <- testing.data.model2

colnames(testing.data.model2)[1:35]
colnames(testing.data.model2)[1:35]

#Model 1
#create the h2o frames
training.data.model1.hex<-as.h2o(localH2O, training.data.model1)
testing.data.model1.hex<-as.h2o(localH2O, testing.data.model1[,2:9217])

for (i in 1:30) {
  #create model
  H2o.Model1 <- h2o.deeplearning(x = c(31:9246), y = i,  training_frame = training.data.model1.hex)
  #predict Model
  prediction.hex <- h2o.predict(H2o.Model1, testing.data.model1.hex)
  #Append results
  predicted.testing.data1=data.refresh(prediction.hex,training.data,predicted.testing.data1,i)
}

predicted.testing.data1=predicted.testing.data1[,c(30:1,31:dim(predicted.testing.data1)[2])]


#Model 2
#create the h2o frames
training.data.model2.hex<-as.h2o(localH2O, training.data.model2)
testing.data.model2.hex<-as.h2o(localH2O, testing.data.model2[,2:9217])

for (i in 1:30) {
  
  if (i==1|i==2|i==3|i==4|i==21|i==22|i==29|i==30) {
    #create model
    H2o.Model2 <- h2o.deeplearning(x = c(31:9246), y = i,  training_frame = training.data.model2.hex)
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
save(predicted.testing.data1,predicted.testing.data2 ,file='H2O_Dynamic_Predictions.Rd')

load('H2O_Dynamic_Predictions.Rd')

predicted.testing.data.temp <- rbind(predicted.testing.data1,predicted.testing.data2[,colnames(predicted.testing.data1)])


predicted.testing.data <- NULL
predicted.testing.data$ImageId <- 1:dim(testing.data)[1]
predicted.testing.data <- data.frame(merge(predicted.testing.data,predicted.testing.data.temp,by="ImageId"))


colnames(predicted.testing.data)[1:35]

predicted.test.data <- predicted.testing.data


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

submit_file  <- paste0(data.dir, 'submission_H2O_deeplearning_2Model.csv')
write.csv(nsub[,c(1,4)], file=submit_file, quote=F, row.names=F)




















predicted.testing.data2=predicted.testing.data2[,c(30:1,31:dim(predicted.testing.data2)[2])]

predicted.testing.data2<-NULL
predicted.testing.data2 <- testing.data.model2 <- testing.data[testing.data$Model==2,1:9217]

colnames(predicted.testing.data1[1:35])
colnames(predicted.testing.data2[1:35])

predicted.testing.data <- NULL
predicted.testing.data$ImageId <- 1:dim(testing.data)[1]

predicted.testing.data <- data.frame(merge(predicted.testing.data,predicted.testing.data1,by="ImageId"))

predicted.testing.data <- data.frame(merge(predicted.testing.data,predicted.testing.data2,by="ImageId"))
colnames(predicted.testing.data[1:35])

colnames(predicted.testing.data)
predicted.testing.data[1:30]

summary(training.data.model2[1:31])

#######################################################################################


colnames(predicted.testing.data1)[1:33]


predicted.testing.data1[predicted.testing.data1$ImageId==575,]

##View Predictions

layout = layout(matrix(1:6, nc = 2)) 

for (i in 
     
     c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,406,407,408,409,410,411,413,417,418,419,420,422,423,425,426,428,429,431,432,433,435,437,438,439,440,441,442,443,445,446,447,448,449,450,451,452,453,454,455,456,457,460,461,462,463,464,465,466,467,468,469,472,473,474,475,476,478,479,480,482,484,485,486,487,488,489,490,491,492,493,494,495,496,498,499,500,503,506,507,508,509,510,511,512,513,514,515,516,517,518,519,520,522,523,524,525,526,527,528,529,530,531,533,534,535,537,538,539,540,541,542,543,544,545,546,547,548,549,550,553,555,556,557,558,560,561,562,563,564,565,566,567,568,569,570,571,573,574,577,578,579,580,581,582,583,584,585,586,587,588,589,590,591)
     
     
     ){
  
  im.train=predicted.testing.data[i,32:9247]
  d.train=predicted.testing.data[i,2:31]
  
  im <- matrix(data=rev(as.integer(im.train)), nrow=96, ncol=96)
  image(1:96, 1:96, im, col=gray((0:255)/255))
  
  
  ##Given Data for the Image
  points(96-d.train$left_eye_center_x,         96-d.train$left_eye_center_y,         col="green")
  points(96-d.train$right_eye_center_x,         96-d.train$right_eye_center_y,         col="green")
  points(96-d.train$nose_tip_x,         96-d.train$nose_tip_y,         col="green")
  
  points(96-d.train$left_eye_inner_corner_x,         96-d.train$left_eye_inner_corner_y,         col="green")
  points(96-d.train$left_eye_outer_corner_x,         96-d.train$left_eye_outer_corner_y,         col="green")
  
  points(96-d.train$right_eye_inner_corner_x,         96-d.train$right_eye_inner_corner_y,         col="green")
  points(96-d.train$right_eye_outer_corner_x,         96-d.train$right_eye_outer_corner_y,         col="green")
  
  points(96-d.train$left_eyebrow_inner_end_x,         96-d.train$left_eyebrow_inner_end_y,         col="green")
  points(96-d.train$left_eyebrow_outer_end_x,         96-d.train$left_eyebrow_outer_end_y,         col="green")
  
  points(96-d.train$right_eyebrow_inner_end_x,         96-d.train$right_eyebrow_inner_end_y,         col="green")
  points(96-d.train$right_eyebrow_outer_end_x,         96-d.train$right_eyebrow_outer_end_y,         col="green")
  
  
  points(96-d.train$mouth_center_top_lip_x,         96-d.train$mouth_center_top_lip_y,         col="green")
  points(96-d.train$mouth_center_bottom_lip_x,         96-d.train$mouth_center_bottom_lip_y,         col="green")
  points(96-d.train$mouth_left_corner_x,         96-d.train$mouth_left_corner_y,         col="green")
  points(96-d.train$mouth_right_corner_x,         96-d.train$mouth_right_corner_y,         col="green")
  
}


