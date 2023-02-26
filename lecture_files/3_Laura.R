rm(list=objects())
graphics.off()

setwd("~/M1 Maths&IA/S2/Modelisation predictive/DataChallenge")

library(tidyverse)
library(lubridate)
library(dplyr)
library(mgcv)

Data0 <- read_delim("Data/new_Data0.csv", delim=",")
Data1<- read_delim("Data/new_Data1.csv", delim=",")

sel_a <- which(Data0$Year<=2019)
sel_b <- which(Data0$Year>2019)

model <- gam(Load ~ WeekDays + s(Temp, Temp_s95_min), data = Data0[sel_a,])
summary(model)  
plot(model, residuals=T, rug=T, se=F, pch=20)
model.forecast <- predict(model, newdata=Data0[sel_b,])


plot(Data0$Date, Data0$Load, type='l')
lines(Data0$Date[sel_b], model.forecast, col='red')
