##function to get matrix size
getxy <- function(x,y) {
  
  #x=80.22713
  x.dot <- 25+ (x-floor(x))
  #y=32.22814
  y.dot <- 25 + (y-floor(y))
  
  x1<-(floor(x)-24):((floor(x)+24)-1)
  y1<-(floor(y)-24):((floor(y)+24)-1)
  
  if(min((floor(x)-24):((floor(x)+24)-1)) < 1) {
    x.dot <- 0;    y.dot <- 0
  }
  if(min((floor(y)-24):((floor(y)+24)-1)) < 1) {
    x.dot <- 0;    y.dot <- 0
  }
  
  if(max((floor(x)-24):((floor(x)+24)-1)) > 96) {
    x.dot <- 0;    y.dot <- 0
  }
  if(max((floor(y)-24):((floor(y)+24)-1)) > 96) {
    x.dot <- 0;    y.dot <- 0
  }
  
  return (c(x1,y1,x.dot,y.dot))
}




























##function to get matrix size
getxy <- function(x,y) {
  
  #x=80.22713
  x.dot <- 25+ (x-floor(x))
  #y=32.22814
  y.dot <- 25 + (y-floor(y))
  
  x1<-(floor(x)-24):((floor(x)+24)-1)
  y1<-(floor(y)-24):((floor(y)+24)-1)
  
  if(min((floor(x)-24):((floor(x)+24)-1)) < 1) {
    x1=1:48
    x.dot <- (x+1)
  }
  if(min((floor(y)-24):((floor(y)+24)-1)) < 1) {
  y1=1:48
  y.dot <- (y+1)
  }
  
  if(max((floor(x)-24):((floor(x)+24)-1)) > 96) {
    x1=49:96
    x.dot <- (96-x-2)
  }
  if(max((floor(y)-24):((floor(y)+24)-1)) > 96) {
    y1=49:96 
    y.dot <- (96-y-2)
  }
  
  return (c(x1,y1,x.dot,y.dot))
}


?round
mat5
floor(8.1)

##display the images
layout = layout(matrix(1:4, nc = 2)) 

#Multiply the training data
training.data.patch.im <-NULL
training.data.patch.points <- NULL
for (i in 1 ) {
   
  string=paste('Process Started for Row :',i)
  print (string)
  
  i=1
  im <- matrix(as.integer(training.data[i,31:9246]), nrow=96, ncol=96)
  im.rev<- matrix(rev(im), nrow=96, ncol=96)
  point <- training.data[i,1:30]
  image(1:96, 1:96, im.rev, col=gray((0:255)/255))
  points(96-point$left_eye_center_x,         96-point$left_eye_center_y,         col="green")
  
  #j=1
  for (j in seq(from = 1, to = 30, by = 2)){
    if (is.na(point[,j:(j+1)])==FALSE) {
      xy <- getxy(point[,j],point[,(j+1)])
      im2<-im[xy[1:48],xy[49:96]]
      im2.rev<- matrix(rev(im2), nrow=48, ncol=48)
      image(1:48, 1:48, im2.rev, col=gray((0:255)/255))
      points(48-(25+xy[97]),48-(25+xy[98]),         col="green")
    }
  }
}
 







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


install.packages("fields")
library(fields)
## Original data (4x4)
rr <- matrix(rev(as.integer(training.data[i,31:9246])), nrow=96, ncol=96)
ss <- ResizeMat(rr, c(48,48)) 
tt <- ResizeMat(rr, c(24,24)) 
xx <- ResizeMat(tt, c(96,96)) 

## Plot for comparison
par(mfcol=c(2,2), mar=c(1,1,2,1))
image(1:96, 1:96, rr, col=gray((0:255)/255))
image(1:48, 1:48, ss, col=gray((0:255)/255))
image(1:24, 1:24, tt, col=gray((0:255)/255))
image(1:96, 1:96, xx, col=gray((0:255)/255))



##display the images
layout = layout(matrix(1:4, nc = 2)) 







##display the images
layout = layout(matrix(1:4, nc = 2)) 

for (i in 1){
  im.train <- training.data[i,31:9246]
  d.train <- training.data[i,1:30]
  d.train<-d.train/2
  
  
  im <- matrix(rev(as.integer(im.train)), nrow=96, ncol=96)
  im <- ResizeMat(im, c(48,48)) 
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






################################################################################
training.data <- training.data.explode
dim(training.data)
#Multiply the training data                      patchs
training.data.patch.im <-NULL
training.data.patch.points <- NULL
for (i in 1:dim(training.data)[1] ) {
  
  string=paste('Process Started for Row :',i)
  print (string)
  
  #i=2
  im <- matrix(as.integer(training.data[i,31:9246]), nrow=96, ncol=96)
  im.48 <-ResizeMat(im, c(48,48))
  training.data.patch.im = rbind(training.data.patch.im,as.data.frame(matrix(im.48, ncol = 2304, byrow = TRUE)))
  
  point <- training.data[i,1:30]
  training.data.patch.points = rbind(training.data.patch.points,(point/2))
  
  #j=1
  for (j in seq(from = 1, to = 30, by = 2)){
    if (is.na(point[,j:(j+1)])==FALSE) {
      
      xy <- getxy(point[,j],point[,(j+1)])
      
      if (xy[97:98]!=c(0,0)) {
        im2<-im[xy[1:48],xy[49:96]]
        training.data.patch.im = rbind(training.data.patch.im,as.data.frame(matrix(im2, ncol = 2304, byrow = TRUE)))
        new.points <- point
        new.points[,j] <- xy[97]
        new.points[,(j+1)] <- xy[98]
        new.points[,setdiff(1:30,c(j,j+1))] <- NA
        training.data.patch.points = rbind(training.data.patch.points,new.points)
        }
    }
  }
}

dim(training.data.patch.im)

dim(training.data.patch.points)

##display the images
layout = layout(matrix(1:4, nc = 2)) 

for (i in 1:dim(training.data.patch.im[1:8,])[1]){
  im.train <- training.data.patch.im[i,]
  d.train <- training.data.patch.points[i,]
  
  
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



