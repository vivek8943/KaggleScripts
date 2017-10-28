#required Libraries
library(h2o)
library(foreach)
library(doParallel)
library(reshape2)
library(mice)
library(data.table)


#data source path and source files
data.dir   <- 'C:/Users/Priyank/OneDrive/Documents/MIS/Applied Multivariate Analysis/Mid Term Project - Facial Keypoints Detection/DataSource/'
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


##function to rotate points
rotate.points <- function(x,y,a) {
  xm=48
  ym=48
  a = a * pi / 180
  xr = (x - xm) * cos(a) - (y - ym) * sin(a)   + xm
  yr = (x - xm) * sin(a) + (y - ym) * cos(a)   + ym
  return (c(xr,yr))
}


#Multiply the training data
training.data.explode.im <-NULL
training.data.explode.points <- NULL
for (i in 1:dim(training.data)[1] ) {
  
  string=paste('Process Started for Row :',i)
  print (string)
  
  
  im <- matrix(as.integer(training.data[i,31:9246]), nrow=96, ncol=96)
  training.data.explode.im = rbind(training.data.explode.im,as.data.frame(matrix(im, ncol = 9216, byrow = TRUE)))
  training.data.explode.points = rbind(training.data.explode.points,training.data[i,1:30])
  
  for (j in 1:3) {
    im <- apply(im, 1, rev)
    training.data.explode.im = rbind(training.data.explode.im,as.data.frame(matrix(im, ncol = 9216, byrow = TRUE)))
    
    training.data.explode.points = rbind(training.data.explode.points,training.data[i,1:30])
    if (j==1){rot <- 90}
    if (j==2){rot <- 180}
    if (j==3){rot <- 270}
    for (k in seq(from = 1, to = 30, by = 2)) {
      new.points <- rotate.points(training.data[i,k],training.data[i,k+1],rot)
      training.data.explode.points[(i-1)*4+j+1,k] <- new.points[1]
      training.data.explode.points[(i-1)*4+j+1,k+1] <- new.points[2]
    }
  }
}


dim(training.data.explode.im)
dim(training.data.explode.points)

##display the images
layout = layout(matrix(1:4, nc = 2)) 

for (i in (dim(training.data.explode.im)[1]-3):dim(training.data.explode.im)[1]){
  im.train <- training.data.explode.im[i,]
  d.train <- training.data.explode.points[i,]
  
  
  im <- matrix(rev(as.integer(im.train)), nrow=96, ncol=96)
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



#save and load data in R .Initial data from source file after cleaning (Omitting the NA's)
save(training.data.explode.im,training.data.explode.points ,file='Exploded_Training_Data.Rd')

load('Exploded_Training_Data.Rd')

# Add an ID to this image data
training.data.explode.im$id<-1:dim(training.data.explode.im)[1]
training.data.explode.points$id<-1:dim(training.data.explode.points)[1]


#consolidate the training data
training.data.explode <- data.frame(merge(training.data.explode.points,training.data.explode.im,by="id"))
training.data.explode$id <- NULL

summary(training.data.explode[,1:30])

#save and load data in R .Initial data from source file after cleaning (Omitting the NA's)
save(training.data.explode,file='Exploded_Training_Data_Merged.Rd')

load('Exploded_Training_Data_Merged.Rd')


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


#Colummns for the Model
model.column.include <- strsplit(test.model.format[21], ",")[[1]]
model.column.exclude <- setdiff(colnames(training.data)[1:30],strsplit(test.model.format[21], ",")[[1]])

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

model.training.data <- model.training.data[,colnames(test.data.analysis[,c(1,20,21,2:19,22:31)])]
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

#save and load data in R .Initial data from source file after cleaning (Omitting the NA's)
save(training.data,testing.data ,file='H2O_Dynamic_Initial.Rd')

load('H2O_Dynamic_Initial.Rd')


#####################################################################################
## Here to code to make data independent -- Saurabh working                        ##
#####################################################################################


#####################################################################################################################

#Create an H2O Model for each variables
# Start h2o ,decide min and max memory sizes,number of cpu cores to be used (-1 means all cores)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,
                     max_mem_size = '12g', min_mem_size = '4g', nthreads = -1)


#get training data with respect to model
training.data.format <- training.data[training.data$Format == test.model.format[18],c(1:(dim(training.data)[2]-1))]
head(training.data.format[1:30])
dim(training.data[training.data$Format == test.model.format[18],c(1:(dim(training.data)[2]-1))])
test.model.format[18]

unlist(strsplit(test.model.format[21], ","))[1]


output.columns <-unlist(strsplit(test.model.format[18], ","))[1]

#Training data
training.data.hex<-as.h2o(localH2O, training.data.format)

#h20 Model
Model <- h2o.deeplearning(x = c(31:9246), y = output.columns,  training_frame = training.data.hex)


#use maximum size
memory.size(TRUE)

#get Image Ids for the Model
test.data.ImageId=test.data.analysis[test.data.analysis$Concat == test.model.format[18],c(1,32)]
test.data.ImageId$ImageId

#predict values for the Testing dataset 
testing.data.format <- testing.data[test.data.ImageId$ImageId,]
testing.data.hex<-as.h2o(localH2O, testing.data.format)
prediction.hex <- h2o.predict(Model, testing.data.hex)

prediction <- as.data.frame(prediction.hex)
prediction$id <-test.data.ImageId$ImageId

testing.data$id<-1:dim(testing.data)[1]
testing.data.predicted<- testing.data[,9216:9217]
testing.data.predicted <- data.frame(merge(testing.data.predicted,data.frame(merge(prediction,testing.data,by="id")),by='id'))
setnames(testing.data.predicted, old='predict', new=output.columns)

head(testing.data.predicted[,1:5])
testing.data.predicted$nose_tip_y

testing.data.predicted$col <- prediction.hex
testing.data.predicted <- testing.data.format

testing.data=data.refresh(prediction.hex,training.data,testing.data,1)

#shutdown h2o
h2o.shutdown(localH2O, prompt=FALSE)

colnames(testing.data.predicted)

training.data.hex[,21]


names(testing.data.predicted)[1:5]
replace(names(testing.data.predicted),'predict','xxx')[1:5]

setnames(testing.data.predicted, old='predict', new=output.columns)

######################################################################################################################


output.columns <-colnames(training.data)[22]
input.columns <- setdiff(colnames(training.data),output.columns)

f <- as.formula(paste(paste(output.columns, collapse = " + "),'~', paste(input.columns, collapse = " + ")))



dim(test.model.format)
length(test.model.format)

colnames(test.data.analysis)


test.data.analysis$NewCol[1783]

x <- c("1\t\t", "2", "3\t\t\t")

gsub("\\t", "", x)



test.data.analysis$NewCol2[1783] 


xyz=c(test.data.analysis[,c(3:31)], sep = ",")
xyz[1783]
dim(xyz)

complete.cases(test.data.analysis[,c(2:31)])

MB.Training.Data=training.data[complete.cases(training.data[,c(29:30,3:4,1:2,21:22)]),c(29:30,3:4,1:2,21:22,31:dim(training.data)[2])]



#store the results in proper submission format
msub <- merge(Id.lookup, submission, all.x=T, sort=F)
nsub <- msub[, Idlookup_colnames]

Id.lookup[1:100,]

Id.lookup.groupby <- aggregate(.~ImageId+,Id.lookup[,c(2,4)],sum)
?aggregate
head(Id.lookup.groupby)
unique(Id.lookup.groupby$Location)


head(Id.lookup[,2:3])
library(reshape2)
melt(Id.lookup[,2:3], id.vars="ImageId")


aggregate(Id.lookup$ImageId, by=list(Id.lookup$FeatureName), FUN=sum)[2]

levels(factor(Id.lookup$ImageId))
?reshape2
?dcast

colnames(Id.lookup)


#Air quality example
names(airquality) <- tolower(names(airquality))
aqm <- melt(airquality, id=c("month", "day"), na.rm=TRUE)

acast(aqm, day ~ month ~ variable)
acast(aqm, month ~ variable, mean)
acast(aqm, month ~ variable, mean, margins = TRUE)
dcast(aqm, month ~ variable, mean, margins = c("month", "variable"))


head(Id.lookup[,2:3])
library(reshape2)
xyz=dcast(Id.lookup[,2:3], ImageId~FeatureName)

colnames(xyz)
head(
  xyz[1700,]


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