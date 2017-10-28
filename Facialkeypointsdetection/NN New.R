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


#Multiply the training data
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



library(neuralnet)
summary(training.data)

training.data <- training.data.resize[complete.cases(training.data.resize),]

final[complete.cases(final),]


#NN
output.columns <-colnames(training.data)[1:30]
input.columns <- names(training.data[,31:2334])


f <- as.formula(paste(paste(output.columns, collapse = " + "),'~', paste(input.columns, collapse = " + ")))

nn <- neuralnet(f,data=training.data,hidden=c(100,100))


predicted.results <- compute(nn,training.data[31:2334])


summary(yy)


head(predicted.results)
head(predicted.results$net.result)


x <- as.matrix(training.data[,31:2334])
y <- as.matrix(training.data[,1:30])
m3 <- dbn.dnn.train(x = x, y = y, hidden = c(100,100),activationfun='tanh')
nn.test(m3, x, y)
yy <- nn.predict(m3, x)
?dbn.dnn.train



##display the images
layout = layout(matrix(1:4, nc = 2)) 

for (i in 1:dim(training.data.resize.im[1:8,])[1]){
  im.train <- training.data.resize.im[i,]
  d.train <- training.data.resize.points[i,]
  
  
  im <- matrix(rev(as.integer(im.train)), nrow=48, ncol=48)
  image(1:48, 1:48, im, col=gray((0:255)/255))
  
  ##Given Data for the Image
  points(48-d.train$left_eye_center_x,         48-d.train$left_eye_center_y,         col="green")
  points(48-d.train$right_eye_center_x,         48-d.train$right_eye_center_y,         col="green")
  points(48-d.train$nose_tip_x,         48-d.train$nose_tip_y,         col="green")
  
  points(48-d.train$left_eye_inner_corner_x,         48-d.train$left_eye_inner_corner_y,         col="green")
  points(48-d.train$left_eye_outer_corner_x,         48-d.train$left_eye_outer_corner_y,         col="green")
  
  points(48-d.train$right_eye_inner_corner_x,         48-d.train$right_eye_inner_corner_y,         col="green")
  points(48-d.train$right_eye_outer_corner_x,         48-d.train$right_eye_outer_corner_y,         col="green")
  
  points(48-d.train$left_eyebrow_inner_end_x,         48-d.train$left_eyebrow_inner_end_y,         col="green")
  points(48-d.train$left_eyebrow_outer_end_x,         48-d.train$left_eyebrow_outer_end_y,         col="green")
  
  points(48-d.train$right_eyebrow_inner_end_x,         48-d.train$right_eyebrow_inner_end_y,         col="green")
  points(48-d.train$right_eyebrow_outer_end_x,         48-d.train$right_eyebrow_outer_end_y,         col="green")
  
  
  points(48-d.train$mouth_center_top_lip_x,         48-d.train$mouth_center_top_lip_y,         col="green")
  points(48-d.train$mouth_center_bottom_lip_x,         48-d.train$mouth_center_bottom_lip_y,         col="green")
  points(48-d.train$mouth_left_corner_x,         48-d.train$mouth_left_corner_y,         col="green")
  points(48-d.train$mouth_right_corner_x,         48-d.train$mouth_right_corner_y,         col="green")
}

#summary(training.data[,c(1:30,9247,9248)])
#save and load data in R .Initial data from source file after cleaning (Omitting the NA's)
save(training.data.resize,training.data.resize.im,training.data.resize.points ,file='Resize.Rd')



