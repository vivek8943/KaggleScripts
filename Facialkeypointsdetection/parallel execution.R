training.data <- training.data.explode
dim(training.data)
# Add an ID to this image data
training.data$id<-1:dim(training.data)[1]

#Multiply the training data                      patchs
training.data.patch.im <-NULL
training.data.patch.points <- NULL

#convert image data to integer
training.data.patch.im <- foreach(image = training.data, .combine=rbind) %dopar% {
  print(image[9247])
  im <- matrix(as.integer(image[31:9246]), nrow=96, ncol=96)
  im.48 <-ResizeMat(im, c(48,48))
  as.data.frame(matrix(im.48, ncol = 2304, byrow = TRUE))
  
}
  
  point <- image[1:30]
  training.data.patch.points = rbind(training.data.patch.points,(point/2))
  
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
