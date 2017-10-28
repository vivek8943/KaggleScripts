
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
training.data.flip.im <-NULL
training.data.flip.points <- NULL
#for (i in 1:dim(training.data[3,])[1] ) {
for (i in 1 ) {
  
  string=paste('Process Started for Row :',i)
  print (string)
  
  im <- matrix(as.integer(training.data[i,31:9246]), nrow=96, ncol=96)
  training.data.flip.im = rbind(training.data.flip.im,
                                as.data.frame(matrix(im, ncol = 9216, byrow = TRUE)))
  
  points <-training.data[i,1:30]
  training.data.flip.points = rbind(training.data.flip.points,points)
  
  #Flip
  im <- im[nrow(im):1,]
  training.data.flip.im = rbind(training.data.flip.im,
                                as.data.frame(matrix(im, ncol = 9216, byrow = TRUE)))
    
  for (k in seq(from = 1, to = 30, by = 2)) {
    points[,k]<-96-points[,k]
  }
  training.data.flip.points = rbind(training.data.flip.points,points)

}


training.data.flip.im$id<-1:dim(training.data.flip.im)[1]
training.data.flip.points$id<-1:dim(training.data.flip.points)[1]
training.data.flip <- data.frame(merge(training.data.flip.points,training.data.flip.im,by="id"))
training.data.flip$id <- NULL

training.data.flip[1:35]

dim(training.data.flip.im)
dim(training.data.flip.points)

##display the images
layout = layout(matrix(1:2, nc = 2)) 

for (i in 1:dim(training.data.flip.im)[1]){
  im.train <- training.data.flip.im[i,]
  d.train <- training.data.flip.points[i,]
  
  
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
