rm(list=objects())
library(tidyverse)
library(lubridate)
library(mgcv)
library(qgam)
library(forecast)

rmse = function(y, ychap, digits=0){
  return(round(sqrt(mean((y-ychap)^2, na.rm=TRUE)), digits=digits))
}

load("Data/Data0.Rda")
load("Data/Data1.Rda")

Data0$Time <- as.numeric(Data0$Date)

sel_a <- which(Data0$Year<=2019)
sel_b <- which(Data0$Year>2019)

Data1 <- Data0[sel_b, ]
Data0 <- Data0[sel_a, ]

help("mgcv-package")

### try the same with qgam and add "mood WeekDays" :
#######BEST SCORE EVER EVER 705 ---> ROAD TO BE EMPLOYED BY EDF
WD = Data0$WeekDays
index = which(Data0$GovernmentResponseIndex>=70)
WD[index]="Saturday"
Data00 = cbind(Data0,WD)

WD = Data1$WeekDays
index = which(Data1$GovernmentResponseIndex>=70)
WD[index]="Saturday"
Data11 = cbind(Data1, WD)
'''
mod0 <- qgam(Load ~ Load.1:as.factor(WeekDays) + BH + Christmas_break
             + Summer_break + DLS + s(Temp) + s(Temp_s99_max, Temp_s99_min)
             + s(Load.7) + s(Time, k=7) + s(toy, k =30, bs = "cc", by=as.factor(WD))
             + s(Temp, Time, k=20), 
             data=Data00, qu=0.4)

gam.check(mod0)
qgam.pred <- predict(mod0, Data11)
'''

equation <- "Load ~ Load.1:as.factor(WeekDays) + BH + Christmas_break + Summer_break + DLS + s(Temp) + s(Temp_s99_max, Temp_s99_min)+ s(Load.7) + s(Time, k=7) + s(toy, k =30, bs = 'cc', by=as.factor(WD))+ s(Temp, Time, k=20)"

gam.0<-qgam(equation%>%as.formula, data=Data00, qu=0.4)
gam.forecast <- predict(gam.0, newdata=Data11)

X <- predict(gam.0, newdata=Data00, type='terms')

for (j in 1:ncol(X)){
  X[,j] <- (X[,j]-mean(X[,j])) / sd(X[,j])
}
  
X <- cbind(X,1)
d <- ncol(X)

y <- Data11$Load
# static 
ssm <- viking::statespace(X, y)
ssm
gam9.kalman.static <- ssm$pred_mean%>%tail(nrow(Data11))

length(y%>%tail(nrow(Data11)))
rmse(y=Data11$Load, ychap=gam.forecast)
rmse(y=Data11$Load, ychap=gam9.kalman.static)


