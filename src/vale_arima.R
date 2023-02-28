rm(list=objects())
graphics.off()
###############packages
rmse = function(y, ychap, digits=0){
  return(round(sqrt(mean((y-ychap)^2, na.rm=TRUE)), digits=digits))
}

library(mgcv)
library(forecast)
library(tidyverse)

###Load Datasets
load("Data/Data0.Rda")
load("Data/Data1.Rda")

#Faire des train et test à partir de Data0
sel_a <- which(Data0$Year<=2019)
sel_b <- which(Data0$Year>2019)

train <- Data0[sel_a, ]
test <- Data0[sel_b, ]

# formula from previous submissions
equation <- "Load ~ Load.1:as.factor(WD) + BH + Christmas_break +
Summer_break + DLS + s(Temp) + s(Temp_s99_max, Temp_s99_min) +
s(Load.7) + s(Time, k=7) + s(toy, k =30, bs = 'cc', by=as.factor(WD)) +
s(Temp, Time, k=20)"

gam.fit <- gam(equation%>%as.formula, data=train)
gam.predict <- predict(gam.fit, newdata = test)
rmse(gam.predict, test$Load) # [1] 1132

# plot residuals
plot(gam.predict, type="lines")
lines(test$Load, col="red")

# add arima
arima.fit <- forecast::Arima(gam.predict, order = c(1,1,2), seasonal = c(0,0,2))
arima.predict <- fitted(arima.fit)
lines(arima.predict, col="blue")
rmse(arima.predict, test$Load) # [1] 3472

# try average
rmse((arima.predict + gam.predict)/2, test$Load) # [1] 2007

# check residuals
forecast::checkresiduals(arima.fit)

# try new arima fit
plot(test$Load, type = "l")
arima.fit <- forecast::Arima(gam.predict, order = c(7,1,7), seasonal = c(7,1,7))
arima.predict <- fitted(arima.fit)
lines(arima.predict, col="blue")
rmse(arima.predict, test$Load) # [1] 3472
adf.test(gam.predict)
acf(gam.predict)
