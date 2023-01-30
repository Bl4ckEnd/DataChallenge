rm(list=objects())
graphics.off()

setwd("~/M1 Maths&IA/S2/Modelisation predictive/DataChallenge")
library(tidyverse)
library(lubridate)
library(mgcv)
library(bestglm)

load("Data/Data0.Rda")
load("Data/Data1.Rda")

rmse<-function(y, ychap, digits=0)
{
  return(round(sqrt(mean((y-ychap)^2,na.rm=TRUE)),digits=digits))
}

#########Mise en forme des données
Data0$Time <- as.numeric(Data0$Date)
Data1$Time <- as.numeric(Data1$Date)

sel_a <- which(Data0$Year<=2019)
sel_b <- which(Data0$Year>2019)

########Modele
mod0 = gam(Load ~ WeekDays*DLS+Load.1*WeekDays + Load.7*GovernmentResponseIndex+s(toy, k = 5, bs = "cc") +
             s(Temp_s99_max-Temp_s95_min, k = 5, bs='cc')+s(Temp, k = 5, bs = "cc") , data=Data0[sel_a,])

pred.mod0 = predict(mod0, newdata=Data0[sel_b,])
plot(Data0$Date[sel_b],pred.mod0, type='l', col='black')
lines(Data0$Date[sel_b],Data0$Load[sel_b], col='red')

rmse(y=Data0$Load[sel_b], ychap=pred.mod0)

forecast <- predict(mod0, newdata=Data1)
submit <- read_delim( file="Data/sample_submission.csv", delim=",")
submit$Load <- forecast
write.table(submit, file="Data/submission:L4.csv", quote=F, sep=",", dec='.',row.names = F)









