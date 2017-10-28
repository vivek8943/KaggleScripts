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

im.train <- foreach(im = im.train, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

##repeat process for test data
d.test  <- read.csv(test.file, stringsAsFactors=F)
im.test <- foreach(im = d.test$Image, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}
d.test$Image <- NULL


#save and load data in R
save(d.train, im.train, d.test, im.test, file='mean_wo_outliers.Rd')

load('mean_wo_outliers.Rd')

#########################################################

cleaned.mean.values <-NULL


for (i in 1:30){
  
boxplot.data=boxplot(d.train[,i])
outliers=boxplot.data$out
index.values=match(outliers,d.train[,i])

#remove the outliers
cleaned.d.train=d.train[-(index.values),i]

#capture the mean
cleaned.mean.values=c(cleaned.mean.values,mean(cleaned.d.train,na.rm = T))

}

#predict
p           <- matrix(data=cleaned.mean.values, nrow=nrow(d.test), ncol=ncol(d.train), byrow=T)
colnames(p) <- names(d.train)
predictions <- data.frame(ImageId = 1:nrow(d.test), p)
head(predictions)


#reformat for submission
library(reshape2)

submission <- melt(predictions, id.vars="ImageId", variable.name="FeatureName", value.name="Location")
head(submission)


#creating a submission file
example.submission <- read.csv(paste0(data.dir, 'submissionFileFormat.csv'))
sub.col.names      <- names(example.submission)
example.submission$Location <- NULL
submission <- merge(example.submission, submission, all.x=T, sort=F)
submission <- submission[, sub.col.names]
write.csv(submission, file=paste0(data.dir, "submission_means_wo_outliers.csv"), quote=F, row.names=F)






