#data source path and source files
data.dir   <- 'C:/Users/Priyank/OneDrive/Documents/MIS/Applied Multivariate Analysis/Mid Term Project - Facial Keypoints Detection/DataSource/'
train.file <- paste0(data.dir, 'training.csv')
test.file  <- paste0(data.dir, 'test.csv')


#load data from training package
d.train <- read.csv(train.file, stringsAsFactors=F)

#store image data in seperate variable
im.train      <- d.train$Image
#remove image data from main variable
d.train$Image <- NULL


layout = layout(matrix(1:6, nc = 2)) 
layout = layout(matrix(1:1, nc = 1)) 
layout = layout(matrix(1:3, nc = 3)) 

maxvaluelist=c(0)
im.train.all=im.train
d.train.all=d.train


for (i in 1:1){

im.train=im.train.all[i]
d.train=d.train.all[i,]

#convert image data to integer
library(foreach)
library(doParallel)

im.train <- foreach(im = im.train, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

#save and load data in R
#save(d.train, im.train, file=paste0(data.dir,'SingleImage.Rd'))

#load(paste0(data.dir,'SingleImage.Rd'))

im <- matrix(data=rev(im.train), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))


colnames(d.train)

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


maxvaluelist=c(maxvaluelist,max(im))

}

max(maxvaluelist)


computeFeatures(im.train,ref=im)

## load and segment nucleus
y = readImage(system.file("images", "nuclei.tif", package="EBImage"))[,,1]
x = thresh(y, 10, 10, 0.05)
x = opening(x, makeBrush(5, shape='disc'))
x = bwlabel(x)
display(y, title="Cell nuclei")
display(x, title="Segmented nuclei")

## compute shape features
fts = computeFeatures.shape(x)
fts

## compute features
ft = computeFeatures(x, y, xname="nucleus")
cat("median features are:\n")
apply(ft, 2, median)

## compute feature properties
ftp = computeFeatures(x, y, properties=TRUE, xname="nucleus")
ftp

max(im)


#Model for 
source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("EBImage")


update.packages()
a
source("http://rimagebook.googlecode.com/svn/installRImageBook.R")

installRImageBook()
library(adimpro)

install.packages("biOps")
library(foreach)

?edgeProfile

install.packages("adimpro")
edges(im, type = "Laplacian", ltype=1, abs=FALSE)
