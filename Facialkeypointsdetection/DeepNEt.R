install.packages("deepnet")
library(deepnet)
x <- as.matrix(training.data[,31:9246])
y <- as.matrix(training.data[,1:30])
m3 <- dbn.dnn.train(x = x, y = y, hidden = c(5,5))

nn.test(m3, x, y)


training.data
summary(testing.data)


nn.predict(nn, x)
yy <- nn.predict(m3, testing.data)


summary(yy)
dim(yy)
