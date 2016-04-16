library(fma)
library(fpp)
library(ResourceSelection)
library(forecast)
fancydata = fancy
plot(fancydata, xlab="Year", ylab="", main="FancyData")
log_fancy = log(fancydata)
plot(log_fancy, xlab="Year", ylab="", main="LogFancyData")

log_fancy = log(fancydata)
surf_dummy = rep(0, length(fancydata))
surf_dummy[seq_along(surf_dummy)%%12 == 3] <- 1
surf_dummy[3] <- 0 #festival started one year later
surf_dummy = ts(surf_dummy, freq = 12, start=c(1987,1))
my_data <- data.frame(log_fancy,surf_dummy)

plot(log_fancy, xlab="Year", ylab="", main="LogFancyData")

fit = tslm(log_fancy ~ trend + season + surf_dummy, data=my_data)
summary(fit)

fitd = tslm(log_fancy ~ trend + season , data=log_fancy)
res <- residuals(fitd)
plot(res,ylab="Residuals", xlab="Time", main="Residuals against time") 

#fitted values
plot(log_fancy, xlab="Year", ylab="", main="Sales")
lines(fitted(fit), col=2)
legend("topright", lty=1, col=c(1,2), legend = c("Actual", "Predicted"))


#Residuals plotted against predicted sales.
plot(fitted(fit), res, xy.lines=FALSE, xy.labels=FALSE, 
     xlab="Predicted values", ylab="Residuals", 
     main="LogSales")
abline(0, 0)
Acf(res, main = "ACF of residuals")

year <- seq(1:84)
month <- seq (1:84)
year [1:12] <- 1987
year [13:24] <- 1988
year [25:36] <- 1989
year [37:48] <- 1990
year [49:60] <- 1991
year [61:72] <- 1992
year [73:84] <- 1993
month[c(1,13,25,37,49,61,73)] <- "Jan"
month[c(2,14,26,38,50,62,74)] <- "Feb"
month[c(3,15,27,39,51,63,75)] <- "Mar"
month[c(4,16,28,40,52,64,76)] <- "Apr"
month[c(5,17,29,41,53,65,77)] <- "May"
month[c(6,18,30,42,54,66,78)] <- "Jun"
month[c(7,19,31,43,55,67,79)] <- "Jul"
month[c(8,20,32,44,56,68,80)] <- "Aug"
month[c(9,21,33,45,57,69,81)] <- "Sep"
month[c(10,22,34,46,58,70,82)] <- "Oct"
month[c(11,23,35,47,59,71,83)] <- "Nov"
month[c(12,24,36,48,60,72,84)] <- "Dec"
times <- paste(month, year)
boxplot(res~times)

fitf = tslm(log_fancy ~ trend + season , data=log_fancy)
coefficients(fitf)


dwtest(fitf, alt="two.sided")

fit2 = lm(log_fancy ~ trend + season)
summary(fitf)
forecast(fitf)
fcast <- forecast(fitf, h=36)
plot(fcast, main="Forecasts")

fcast[2]
predict_data_log = data.frame(fcast[2])
predict_data_raw = exp(predict_data_log)
year1 <- seq(1:36)
month1 <- seq (1:36)
year1 [1:12] <- 1994
year1 [13:24] <- 1995
year1 [25:36] <- 1996

month1[c(1,13,25)] <- "Jan"
month1[c(2,14,26)] <- "Feb"
month1[c(3,15,27)] <- "Mar"
month1[c(4,16,28)] <- "Apr"
month1[c(5,17,29)] <- "May"
month1[c(6,18,30)] <- "Jun"
month1[c(7,19,31)] <- "Jul"
month1[c(8,20,32)] <- "Aug"
month1[c(9,21,33)] <- "Sep"
month1[c(10,22,34)] <- "Oct"
month1[c(11,23,35)] <- "Nov"
month1[c(12,24,36)] <- "Dec"
times1 <- paste(month1, year1)
predict_data_raw = data.frame(times1,predict_data_raw)
predict_data_raw



