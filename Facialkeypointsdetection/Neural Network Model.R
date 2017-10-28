#required Libraries
library(neuralnet)
library(foreach)
library(doParallel)


#data source path and source files
data.dir   <- 'C:/Users/Priyank/OneDrive/Documents/MIS/Applied Multivariate Analysis/Mid Term Project - Facial Keypoints Detection/DataSource/'
train.file <- paste0(data.dir, 'training.csv')
test.file  <- paste0(data.dir, 'test.csv')


#load data from training package
d.train <- read.csv(train.file, stringsAsFactors=F)

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

#save and load data in R
save(training.data, testing.data, file='NeuralNetworkInitial.Rd')

load('NeuralNetworkInitial.Rd')

###############################################################################
#Neural Network Model
###############################################################################

training.data1=training.data[1:2000,]
training.data2=training.data[2001:2140,]

#output.columns <- names(training.data[,1:2])

output.columns <-colnames(training.data)[1]
input.columns <- names(training.data[,31:9000])


f <- as.formula(paste(paste(output.columns, collapse = " + "),'~', paste(input.columns, collapse = " + ")))

nn <- neuralnet(f,data=training.data1,hidden=c(5,5,5,5,5,5),likelihood=T)
                
#plot(nn)

predicted.results <- compute(nn,training.data2[,31:61])


results1 <- data.frame(actual = training.data2$left_eye_center_x,prediction = predicted.results$net.result[,1])
results2 <- data.frame(actual = training.data2$left_eye_center_y,prediction = predicted.results$net.result[,2])

head(results1)
head(results2)




