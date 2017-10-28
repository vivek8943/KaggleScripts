
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
for (i in 1:dim(training.data[1,])[1] ) {
  
  string=paste('Process Started for Row :',i)
  print (string)
  
  im <- matrix(as.integer(training.data[i,31:9246]), nrow=96, ncol=96)
  training.data.explode.im = rbind(training.data.explode.im,
                                   as.data.frame(matrix(im, ncol = 9216, byrow = TRUE)))
  training.data.explode.points = rbind(training.data.explode.points,training.data[i,1:30])
  
  for (j in 1:3) {
    im <- apply(im, 1, rev)
    training.data.explode.im = rbind(training.data.explode.im,
                                     as.data.frame(matrix(im, ncol = 9216, byrow = TRUE)))
    
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

for (i in 1:dim(training.data.explode.im)[1]){
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
