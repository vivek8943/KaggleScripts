# Autoencoder 

str(training.data[1,31:9246])
training.data.first.hex<-as.h2o(localH2O, training.data)
training.data.first.hex[1:9216]

AutoencoderModel <- h2o.deeplearning(x = c(31:9246),  training_frame = training.data.first.hex,hidden=c(1000,500,250),epochs = 10,autoencoder = T,input_dropout_ratio=0.2,activation = "RectifierWithDropout",l1=1e-5)

#anomalies   = h2o.anomaly(object = AutoencoderModel, testing.data.hex)
#recon_error <- as.data.frame(anomalies)
#plot.ts(recon_error)
#test_recon <- h2o.predict(AutoencoderModel, testing.data.hex)
#str(test_recon)
# Plot the reconstruction error and add a line for error in the 90th percentile

#quantile  = h2o.quantile(anomalies$Reconstruction.MSE)
#threshold = quantile["90%"]
#plot(recon_error$Reconstruction.MSE)
#abline(h=threshold)


train_features_deep <- h2o.deepfeatures( training.data.hex, object=AutoencoderModel, layer=3)
myX = c(1:nfeatures)
myY = nfeatures+1

#-----------------------------------------
tfd <- as.data.frame(train_features_deep)
tfd$id<-1:dim(tfd)[1]

tdh <- as.data.frame(training.data.hex[,1:30])
tdh$id<-1:dim(tdh)[1]

tfd.tdh <- data.frame(merge(tfd,tdh,by="id"))
tfd.tdh$id=NULL
train_dimreduced <- tfd.tdh
colnames(train_dimreduced)
train_dimreduced<-as.h2o(localH2O, train_dimreduced)
#-------------------------------------------
head(train_dimreduced)
gc()
model_1 <- h2o.deeplearning(x = c(1:250), y = 251,  training_frame = train_dimreduced)
model_2 <- h2o.deeplearning(x = c(1:250), y = 252,  training_frame = train_dimreduced)
model_3 <- h2o.deeplearning(x = c(1:250), y = 253,  training_frame = train_dimreduced)
model_4 <- h2o.deeplearning(x = c(1:250), y = 254 , training_frame = train_dimreduced)
model_5 <- h2o.deeplearning(x = c(1:250), y = 255 , training_frame = train_dimreduced)
model_6 <- h2o.deeplearning(x = c(1:250), y = 256 , training_frame = train_dimreduced)
model_7 <- h2o.deeplearning(x = c(1:250), y = 257 , training_frame = train_dimreduced)
model_8 <- h2o.deeplearning(x = c(1:250), y = 258 , training_frame = train_dimreduced)
model_9 <- h2o.deeplearning(x = c(1:250), y = 259 , training_frame = train_dimreduced)
gc()
model_10 <- h2o.deeplearning(x = c(1:250), y = 260 , training_frame = train_dimreduced)
model_11 <- h2o.deeplearning(x = c(1:250), y = 261 , training_frame = train_dimreduced)
model_12 <- h2o.deeplearning(x = c(1:250), y = 262 , training_frame = train_dimreduced)
model_13 <- h2o.deeplearning(x = c(1:250), y = 263 , training_frame = train_dimreduced)
model_14 <- h2o.deeplearning(x = c(1:250), y = 264 , training_frame = train_dimreduced)
model_15 <- h2o.deeplearning(x = c(1:250), y = 265 , training_frame = train_dimreduced)
model_16 <- h2o.deeplearning(x = c(1:250), y = 266 , training_frame = train_dimreduced)
model_17 <- h2o.deeplearning(x = c(1:250), y = 267 , training_frame = train_dimreduced)
model_18 <- h2o.deeplearning(x = c(1:250), y = 268 , training_frame = train_dimreduced)
model_19 <- h2o.deeplearning(x = c(1:250), y = 269 , training_frame = train_dimreduced)
model_20 <- h2o.deeplearning(x = c(1:250), y = 270 , training_frame = train_dimreduced)
model_21 <- h2o.deeplearning(x = c(1:250), y = 271 , training_frame = train_dimreduced)
gc()
model_22 <- h2o.deeplearning(x = c(1:250), y = 272 , training_frame = train_dimreduced)
model_23 <- h2o.deeplearning(x = c(1:250), y = 273 , training_frame = train_dimreduced)
model_24 <- h2o.deeplearning(x = c(1:250), y = 274 , training_frame = train_dimreduced)
model_25 <- h2o.deeplearning(x = c(1:250), y = 275 , training_frame = train_dimreduced)
model_26 <- h2o.deeplearning(x = c(1:250), y = 276 , training_frame = train_dimreduced)
model_27 <- h2o.deeplearning(x = c(1:250), y = 277 , training_frame = train_dimreduced)
model_28 <- h2o.deeplearning(x = c(1:250), y = 278 , training_frame = train_dimreduced)
model_29 <- h2o.deeplearning(x = c(1:250), y = 279 , training_frame = train_dimreduced)
model_30 <- h2o.deeplearning(x = c(1:250), y = 280 , training_frame = train_dimreduced)

# prediction
testing.data.hex<-as.h2o(localH2O, testing.data)
str(testing.data.hex)

AutoencoderModeltest <- h2o.deeplearning(x = c(1:9216),  training_frame = testing.data.hex,hidden=c(1000,500,250),epochs = 10,autoencoder = T,input_dropout_ratio=0.2,activation = "RectifierWithDropout",l1=1e-5)
test_features_deep <- h2o.deepfeatures( testing.data.hex, object=AutoencoderModeltest, layer=3)

#model 1
testing.data.hex<-as.h2o(localH2O, test_features_deep)
prediction.hex1 <- h2o.predict(model_1, testing.data.hex)
colnames(train_dimreduced)
colnames(test_features_deep)
colnames(prediction.hex1)
test_features_deep=data.refresh(prediction.hex1,train_dimreduced,test_features_deep,1)


#model 2
testing.data.hex<-as.h2o(localH2O, testing.data)
prediction.hex2 <- h2o.predict(model_2, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,2)

#model 3
testing.data.hex<-as.h2o(localH2O, testing.data[,c(2:1,3:dim(testing.data)[2])])
prediction.hex3 <- h2o.predict(model_3, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,3)

#model 4
testing.data.hex<-as.h2o(localH2O, testing.data[,c(3:1,4:dim(testing.data)[2])])
prediction.hex4 <- h2o.predict(model_4, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,4)

#model 5
testing.data.hex<-as.h2o(localH2O, testing.data[,c(4:1,5:dim(testing.data)[2])])
prediction.hex5 <- h2o.predict(model_5, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,5)

#model 6
testing.data.hex<-as.h2o(localH2O, testing.data[,c(5:1,6:dim(testing.data)[2])])
prediction.hex6 <- h2o.predict(model_6, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,6)

#model 7
testing.data.hex<-as.h2o(localH2O, testing.data[,c(6:1,7:dim(testing.data)[2])])
prediction.hex7 <- h2o.predict(model_7, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,7)

#model 8
testing.data.hex<-as.h2o(localH2O, testing.data[,c(7:1,8:dim(testing.data)[2])])
prediction.hex8 <- h2o.predict(model_8, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,8)

#model 9
testing.data.hex<-as.h2o(localH2O, testing.data[,c(8:1,9:dim(testing.data)[2])])
prediction.hex9 <- h2o.predict(model_9, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,9)

#model 10
testing.data.hex<-as.h2o(localH2O, testing.data[,c(9:1,10:dim(testing.data)[2])])
prediction.hex10 <- h2o.predict(model_10, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,10)

#model 11
testing.data.hex<-as.h2o(localH2O, testing.data[,c(10:1,11:dim(testing.data)[2])])
prediction.hex11 <- h2o.predict(model_11, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,11)

#model 12
testing.data.hex<-as.h2o(localH2O, testing.data[,c(11:1,12:dim(testing.data)[2])])
prediction.hex12 <- h2o.predict(model_12, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,12)

#model 13
testing.data.hex<-as.h2o(localH2O, testing.data[,c(12:1,13:dim(testing.data)[2])])
prediction.hex13 <- h2o.predict(model_13, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,13)

#model 14
testing.data.hex<-as.h2o(localH2O, testing.data[,c(13:1,14:dim(testing.data)[2])])
prediction.hex14 <- h2o.predict(model_14, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,14)

#model 15
testing.data.hex<-as.h2o(localH2O, testing.data[,c(14:1,15:dim(testing.data)[2])])
prediction.hex15 <- h2o.predict(model_15, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,15)

#model 16
testing.data.hex<-as.h2o(localH2O, testing.data[,c(15:1,16:dim(testing.data)[2])])
prediction.hex16 <- h2o.predict(model_16, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,16)

#model 17
testing.data.hex<-as.h2o(localH2O, testing.data[,c(16:1,17:dim(testing.data)[2])])
prediction.hex17 <- h2o.predict(model_17, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,17)

#model 18
testing.data.hex<-as.h2o(localH2O, testing.data[,c(17:1,18:dim(testing.data)[2])])
prediction.hex18 <- h2o.predict(model_18, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,18)

#model 19
testing.data.hex<-as.h2o(localH2O, testing.data[,c(18:1,19:dim(testing.data)[2])])
prediction.hex19 <- h2o.predict(model_19, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,19)

#model 20
testing.data.hex<-as.h2o(localH2O, testing.data[,c(19:1,20:dim(testing.data)[2])])
prediction.hex20 <- h2o.predict(model_20, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,20)

#model 21
testing.data.hex<-as.h2o(localH2O, testing.data[,c(20:1,21:dim(testing.data)[2])])
prediction.hex21 <- h2o.predict(model_21, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,21)

#model 22
testing.data.hex<-as.h2o(localH2O, testing.data[,c(21:1,22:dim(testing.data)[2])])
prediction.hex22 <- h2o.predict(model_22, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,22)

#model 23
testing.data.hex<-as.h2o(localH2O, testing.data[,c(22:1,23:dim(testing.data)[2])])
prediction.hex23 <- h2o.predict(model_23, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,23)

#model 24
testing.data.hex<-as.h2o(localH2O, testing.data[,c(23:1,24:dim(testing.data)[2])])
prediction.hex24 <- h2o.predict(model_24, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,24)

#model 25 
testing.data.hex<-as.h2o(localH2O, testing.data[,c(24:1,25:dim(testing.data)[2])])
prediction.hex25 <- h2o.predict(model_25, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,25)

#model 26
testing.data.hex<-as.h2o(localH2O, testing.data[,c(25:1,26:dim(testing.data)[2])])
prediction.hex26 <- h2o.predict(model_26, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,26)

#model 27
testing.data.hex<-as.h2o(localH2O, testing.data[,c(26:1,27:dim(testing.data)[2])])
prediction.hex27 <- h2o.predict(model_27, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,27)

#model 28
testing.data.hex<-as.h2o(localH2O, testing.data[,c(27:1,28:dim(testing.data)[2])])
prediction.hex28 <- h2o.predict(model_28, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,28)

#model 29
testing.data.hex<-as.h2o(localH2O, testing.data[,c(28:1,29:dim(testing.data)[2])])
prediction.hex29 <- h2o.predict(model_29, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,29)

#model 30
testing.data.hex<-as.h2o(localH2O, testing.data[,c(29:1,30:dim(testing.data)[2])])
prediction.hex30 <- h2o.predict(model_30, testing.data.hex)
testing.data=data.refresh(prediction.hex,training.data,testing.data,30)

predicted.test.data=cbind(data.frame(ImageId = 1:nrow(predicted.test.data)),as.data.frame(prediction.hex1[,1]),as.data.frame(prediction.hex2[,1]),as.data.frame(prediction.hex3[,1]),as.data.frame(prediction.hex4[,1]),as.data.frame(prediction.hex5[,1]),as.data.frame(prediction.hex6[,1]),as.data.frame(prediction.hex7[,1]),as.data.frame(prediction.hex8[,1]),as.data.frame(prediction.hex9[,1]),as.data.frame(prediction.hex10[,1]),
                          as.data.frame(prediction.hex11[,1]),as.data.frame(prediction.hex12[,1]),as.data.frame(prediction.hex13[,1]),as.data.frame(prediction.hex14[,1]),as.data.frame(prediction.hex15[,1]),as.data.frame(prediction.hex16[,1]),as.data.frame(prediction.hex17[,1]),as.data.frame(prediction.hex18[,1]),as.data.frame(prediction.hex19[,1]),as.data.frame(prediction.hex20[,1]),
                                        as.data.frame(prediction.hex21[,1]),as.data.frame(prediction.hex22[,1]),as.data.frame(prediction.hex23[,1]),as.data.frame(prediction.hex24[,1]),as.data.frame(prediction.hex25[,1]),as.data.frame(prediction.hex26[,1]),as.data.frame(prediction.hex27[,1]),as.data.frame(prediction.hex28[,1]),as.data.frame(prediction.hex29[,1]),as.data.frame(prediction.hex30[,1]))
---------------------------------------------------------------------------------------------------
colnames(predicted.test.data)<-c('ImageId',colnames(training.data)[1:30])

colnames(predicted.test.data)
-----------------------------------
col.name=colnames(training.data)[1:30]

colnames(predicted.test.data) <- col.name
colnames(predicted.test.data)
