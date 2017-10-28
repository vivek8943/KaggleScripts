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


##############################################################
#Analysis to check if the filling of NA values is appropriate#
##############################################################

#load data from training package
d.train <- read.csv(train.file, stringsAsFactors=F)
#store image data in seperate variable
im.train      <- d.train$Image
#remove image data from main variable
d.train$Image <- NULL

#filled data
load('DeepLearningInitial.Rd')
d.train.na.fill=training.data

im.train.all=im.train
d.train.all=d.train


complete.cases(d.train.all)

####

summary(predicted.test.data[1:30])

layout = layout(matrix(1:6, nc = 2)) 

for (i in 444){
  
  im.train=predicted.test.data[i,31:9246]
  d.train=predicted.test.data[i,1:30]

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

#conclusion
#As though they are colse enof they are not eextract

#plot data from the last h2o learning model
load('DeepLearningPredictedResults_nafill.Rd')
#load data from training package
d.test <- read.csv(test.file, stringsAsFactors=F)
#store image data in seperate variable
im.test      <- d.test$Image
#remove image data from main variable
d.test$Image <- NULL


#predicted.test.data

####

layout = layout(matrix(1:18, nc = 6)) 

for (i in 1000:1700){
  
  im.train=im.test[i]
  d.train=predicted.test.data[i,]
  
  im.train <- foreach(im = im.train, .combine=rbind) %dopar% {
    as.integer(unlist(strsplit(im, " ")))
  }
  
  im <- matrix(data=rev(im.train), nrow=96, ncol=96)
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

#Analysis for new model

summary(d.train)
colnames(d.train)[21:22]


plot(c(10,20),c(50,50))

submit_file  <- paste0(data.dir, 'datatoanalysse.csv')
write.csv(training.data[1:30], file=submit_file, quote=F, row.names=F)



######################################################################################################################
#Rotate Image
#######################################################################################################################




#load data from training package
d.train <- read.csv(train.file, stringsAsFactors=F)
#store image data in seperate variable
im.train      <- d.train$Image
#remove image data from main variable
d.train$Image <- NULL

#filled data
load('DeepLearningInitial.Rd')
d.train.na.fill=training.data

im.train.all=im.train
d.train.all=d.train


complete.cases(d.train.all)

####ROTATE
im<- apply(im, 1, rev)
im<- apply(im, 1, rev)
points(d.train$nose_tip_x[2],         d.train$nose_tip_y[2],         col="red")
points(d.train$left_eye_center_x[2],  d.train$left_eye_center_y[2],  col="blue")
points(d.train$right_eye_center_x[2], d.train$right_eye_center_y[2], col="green")

im.train.rotate <- im.train.all[1]
d.train.rotate <- d.train.na.fill[1,]

apply(im.train.rotate,im.train.all[1], 1, rev)
?apply
im.train.rotate = rbind(im.train.rotate,im.train.all[1])
d.train.rotate = rbind(d.train.rotate,d.train.na.fill[1,])

dim(im.train.rotate)
dim(d.train.rotate)

install.packages("adimpro")
library(adimpro)
rotate.image(im.train.rotate, angle = 90, compress=NULL)

#############################################################################################
layout = layout(matrix(1:4, nc = 2)) 

for (i in 1){
  im.train=training.data[1,31:9246]

  im <- matrix(rev(as.integer(im.train)), nrow=96, ncol=96)
  im.train.rotate <- as.data.frame(matrix(im, ncol = 9216, byrow = TRUE))
  image(1:96, 1:96, im, col=gray((0:255)/255))
  
  im <- apply(im, 1, rev)
  im.train.rotate = rbind(im.train.rotate,as.data.frame(matrix(im, ncol = 9216, byrow = TRUE)))
  image(1:96, 1:96, im, col=gray((0:255)/255))
  
  im <- apply(im, 1, rev)
  im.train.rotate = rbind(im.train.rotate,as.data.frame(matrix(im, ncol = 9216, byrow = TRUE)))
  image(1:96, 1:96, im, col=gray((0:255)/255))
  
  im <- apply(im, 1, rev)
  im.train.rotate = rbind(im.train.rotate,as.data.frame(matrix(im, ncol = 9216, byrow = TRUE)))
  image(1:96, 1:96, im, col=gray((0:255)/255))
  
}

#############################################################################################
d.train.rotate <- training.data[1,1:30]
d.train.rotate = rbind(d.train.rotate,training.data[1,1:30])
d.train.rotate[2:4,] <-d.train.na.fill[1,c(2,1,
                                                          4,3,
                                                          6,5,
                                                          8,7,
                                                          10,9,
                                                          12,11,
                                                          14,13,
                                                          16,15,
                                                          18,17,
                                                          20,19,
                                                          22,21,
                                                          24,23,
                                                          26,25,
                                                          28,27,
                                                          30,29)]
                       
dim(d.train.rotate)
d.train.rotate[,1:2]


for (i in seq(from = 1, to = 30, by = 2)) {
  x=d.train.rotate[1,i]
  y=d.train.rotate[1,i+1]
  xm=48
  ym=48
  a=90
  a = a * pi / 180
  xr = (x - xm) * cos(a) - (y - ym) * sin(a)   + xm
  yr = (x - xm) * sin(a) + (y - ym) * cos(a)   + ym
  d.train.rotate[2,i] <- xr
  d.train.rotate[2,i+1] <- yr
  
  a=180
  a = a * pi / 180
  xr = (x - xm) * cos(a) - (y - ym) * sin(a)   + xm
  yr = (x - xm) * sin(a) + (y - ym) * cos(a)   + ym
  d.train.rotate[3,i] <- xr
  d.train.rotate[3,i+1] <- yr
  
  a=270
  a = a * pi / 180
  xr = (x - xm) * cos(a) - (y - ym) * sin(a)   + xm
  yr = (x - xm) * sin(a) + (y - ym) * cos(a)   + ym
  d.train.rotate[4,i] <- xr
  d.train.rotate[4,i+1] <- yr
  
  
}

x=66.03356
y=39.00227
xm=48
ym=48
a=90
a = a * pi / 180

xr = (x - xm) * cos(a) - (y - ym) * sin(a)   + xm
yr = (x - xm) * sin(a) + (y - ym) * cos(a)   + ym
xr
yr


layout = layout(matrix(1:4, nc = 2)) 

for (i in 1:4){
  
  im.train=im.train.rotate[i,]
  d.train=d.train.rotate[i,]
  
  im <- matrix((as.integer(im.train)), nrow=96, ncol=96)
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


######################################################################################################################
#Reduce Dimentions
#######################################################################################################################

im.train <- training.data[1:5,31:9246]



pca <- prcomp(im.train,
              center = TRUE,
              scale. = TRUE)
summary(pca)
plot(pca)

restr <- pca$x %*% t(pca$rotation)

dim(t(pca$rotation))
summary(pca$rotation)
plot(restr)

ls(pca)

pca$center
pca$rotation
pca$scale
pca$sdev
pca$x



# unscale and uncenter the data
if(pca$scale != FALSE){
  restr <- scale(restr, center = FALSE , scale=1/pca$scale)
}
if(all(pca$center != FALSE)){
  restr <- scale(restr, center = -1 * pca$center, scale=FALSE)
}

# plot your original image and reconstructed image
par(mfcol=c(1,2), mar=c(1,1,2,1))
im <- matrix(rev(as.integer(im.train[2,])), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))

rst <- matrix(data=rev(restr[2,]), nrow=96, ncol=96)
image(1:96, 1:96, rst, col=gray((0:255)/255))

im.out<- resizeImage(im,48,48)
image(1:48, 1:48, im.out, col=gray((0:255)/255))


resizeImage = function(im, w.out, h.out) {
  # function to resize an image 
  # im = input image, w.out = target width, h.out = target height
  # Bonus: this works with non-square image scaling.
  
  # initial width/height
  w.in = nrow(im)
  h.in = ncol(im)
  
  # Create empty matrix
  im.out = matrix(rep(0,w.out*h.out), nrow =w.out, ncol=h.out )
  
  # Compute ratios -- final number of indices is n.out, spaced over range of 1:n.in
  w_ratio = w.in/w.out
  h_ratio = h.in/h.out
  
  # Do resizing -- select appropriate indices
  im.out <- im[ floor(w_ratio* 1:w.out), floor(h_ratio* 1:h.out)]
  
  return(im.out)
}

