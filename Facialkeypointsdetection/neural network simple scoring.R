#data source path and source files
data.dir   <- ''
train.file <- paste0(data.dir, 'training.csv')
test.file  <- paste0(data.dir, 'test.csv')

#load data from training package
d.train <- read.csv(train.file, stringsAsFactors=F)

#store image data in seperate variable
im.train      <- d.train$Image
#remove image data from main variable
d.train$Image <- NULL

#convert image data to integer
library(foreach)
library(doParallel)

im.train$Ima <- foreach(im = im.train, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

##repeat process for test data
d.test  <- read.csv(test.file, stringsAsFactors=F)
im.test <- foreach(im = d.test$Image, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}
d.test$Image <- NULL


#save and load data in R
save(d.train, im.train, d.test, im.test, file='SimpleNN.Rd')

load('SimpleNN.Rd')

##########################################################
library("neuralnet")


#viviek
traininginput1<-im.train
trainingoutput1<-d.train$left_eye_center_x
trainingdata1<- cbind(traininginput1,trainingoutput1)
colnames(trainingdata1) <- c("Input","Output")
net.sqrt <- neuralnet(Output~Input,trainingdata1, hidden=10, threshold=0.01)
print(net.sqrt)




str(trainingdata1)
colnames(trainingdata1)

d.NNtraining=d.train[1:1000,]
d.NNtesting=d.train[1001:150,]

str(d.NNtraining)

d.NNtraining$Image <- foreach(im = d.NNtraining$Image, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}


PCA=princomp(d.NNtraining[1])
ls(PCA)
PCA$sd

d.NNtesting$Image <- foreach(im = d.NNtesting$Image, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}



colnames(d.NNtraining)


n <- names(d.NNtraining)
f <- as.formula(paste("Image ~", paste(n[!n %in% "Image"], collapse = " + ")))

nn <- neuralnet(f,data=d.NNtraining,hidden=c(5,3),linear.output=T)

?neuralnet

## build the neural network (NN)
facedetectnet <- neuralnet(right_eye_center_x ~ right_eye_center_y, d.NNtraining,
                           hidden = 4, lifesign = "minimal", 
                           linear.output = FALSE, threshold = 0.1)

plot(facedetectnet, rep = "best")
temp_test <- subset(d.NNtesting, select = c("right_eye_center_y"))

facedetectnet.results <- compute(facedetectnet, temp_test)

head(temp_test)
results <- data.frame(actual = d.NNtesting$right_eye_center_x,
                      prediction = facedetectnet.results$net.result)
results



AND <- c(rep(0,7),1)
OR <- c(0,rep(1,7))
binary.data <- data.frame(expand.grid(c(0,1), c(0,1), c(0,1)), AND, OR)
print(net <- neuralnet(AND+OR~Var1+Var2+Var3,  binary.data, hidden=0, 
                       rep=10, err.fct="ce", linear.output=FALSE))

data(infert, package="datasets")
print(net.infert <- neuralnet(case~parity+induced+spontaneous, infert, 
                              err.fct="ce", linear.output=FALSE, likelihood=TRUE))
