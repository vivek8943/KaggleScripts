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


#get resized data
training.data.resize.im <-NULL
training.data.resize.points <- NULL
for (i in 1:dim(training.data[1,])[1] ) {
  
  string=paste('Process Started for Row :',i)
  print (string)
  
  im <- matrix(as.integer(training.data[i,31:9246]), nrow=96, ncol=96)
  im.48 <-ResizeMat(im, c(48,48))
  training.data.resize.im = rbind(training.data.resize.im,
                                  as.data.frame(matrix(im.48, ncol = 2304, byrow = TRUE)))
  
  point <- training.data[i,1:30]
  training.data.resize.points = rbind(training.data.resize.points,(point/2))
}

dim(training.data.resize.im)
dim(training.data.resize.points)


##display the images
layout = layout(matrix(1:2, nc = 2)) 

for (i in 1:1){
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



for (i in 1){
  im.train <- training.data[i,31:9246]
  d.train <- training.data[i,1:30]
  
  
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


