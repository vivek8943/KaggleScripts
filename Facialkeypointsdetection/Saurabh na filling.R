install.packages("mice")
library(mice)

MeratempData <- mice(d.train,m=5,maxit=50,meth='pmm',seed=500)

completedData <- complete(MeratempData,1)


#data source path and source files
data.dir   <- 'C:/Users/Priyank/OneDrive/Documents/MIS/Applied Multivariate Analysis/Mid Term Project - Facial Keypoints Detection/DataSource/'
write.csv(training.data,paste0(data.dir,"training_na_fills_with_image.csv"),row.names=F,quote=F)

summary(d.train)
summary(completedData)

colMeans(d.train,na.rm = T)
colMeans(completedData)




#load data from training package
d.train <- read.csv(train.file, stringsAsFactors=F)

#omit na values..filter data
filtered.d.train=na.omit(d.train)

#image data
im.train<-d.train$Image

# Remove image data from filtered data
filtered.d.train$Image<-NULL

#introduce an ID column for later merger
completedData$id<-1:dim(completedData)[1]

#convert image data to integer
im.train <- foreach(im = im.train, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

# Convert it to a data frame
im.train<-data.frame(im.train)
# Remove row names
row.names(im.train)<-NULL
# Add an ID to this image data
im.train$id<-1:dim(im.train)[1]

#consolidate the training data
training.data <- data.frame(merge(completedData,im.train,by="id"))
training.data$id<-NULL


#test data
d.test  <- read.csv(test.file, stringsAsFactors=F)
im.test <- foreach(im = d.test$Image, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

