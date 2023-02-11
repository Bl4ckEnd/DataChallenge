rm(list=objects())
library(tidyverse)
library(lubridate)
library(mgcv)
library(qgam)
library(forecast)

load("Data/Data0.Rda")
load("Data/Data1.Rda")

sel_a <- which(Data0$Year<=2019)
sel_b <- which(Data0$Year>2019)

source('R/score.R')
source("R/mape.R")

help("mgcv-package")


####################
####################

### try the same with qgam - gam with quantile regression
mod0 <- qgam(Load ~ WeekDays*Load.1 + BH + Christmas_break
             + Summer_break + DLS
             + s(Load.7) + s(Time) + s(Temp) + s(toy, k =5, bs = "cc", by=as.factor(WeekDays))
             + s(Temp_s99_min, Temp_s99_max)+s(Temp, Time), 
             data=Data0, qu=0.5)

qgam.pred <- predict(mod0, Data1)
rmse(Data0[sel_b,]$Load, qgam.pred)
summary(mod.qgam)

# create submission : submission_qgamL1.csv
submit <- read_delim( file="Data/sample_submission.csv", delim=",")
submit$Load <- qgam.pred
write.table(submit, file="Data/submission_qgamL1.csv", quote=F, sep=",", dec='.',row.names = F)


### try the same with qgam - gam with quantile regression
mod0 <- qgam(Load ~ WeekDays*Load.1 + BH + Christmas_break
               + Summer_break + DLS
               + s(Load.7) + s(Time) + s(Temp) + s(toy, k =5, bs = "cc", by=as.factor(WeekDays))
               + s(Temp_s99_min, Temp_s99_max)+s(Temp, Time), 
               data=Data0, qu=0.4)

qgam.pred <- predict(mod0, Data1)
rmse(Data0[sel_b,]$Load, qgam.pred)
summary(mod.qgam)

# create submission: submission_qgamL2.csv
submit <- read_delim( file="Data/sample_submission.csv", delim=",")
submit$Load <- qgam.pred
write.table(submit, file="Data/submission_qgamL2.csv", quote=F, sep=",", dec='.',row.names = F)


### try the same with qgam - gam with quantile regression (je crée des classes de Restrinctions covid (GRI20) et je fais des fonctions Load.7 by =GRI20)
GRI20= cut(Data0$GovernmentResponseIndex,breaks=c(-1, 10, 30, 50, 70,100), labels=c("low", "considerable", "middle", "hight","extreme"))
Data00 = cbind(Data0,GRI20)

GRI20= cut(Data1$GovernmentResponseIndex,breaks=c(-1, 10, 30, 50, 70,100), labels=c("low", "considerable", "middle", "hight","extreme"))
Data11 = cbind(Data1, GRI20)

mod0 <- qgam(Load ~ WeekDays*Load.1 + BH + Christmas_break
             + Summer_break + DLS 
             + s(Load.7, k=5, bs = "cr", by = as.factor(GRI20)) + s(Time) + s(Temp) + s(toy, k =5, bs = "cc", by=as.factor(WeekDays))
             + s(Temp_s99_min, Temp_s99_max)+s(Temp, Time), 
             data=Data00, qu=0.4)

qgam.pred <- predict(mod0, Data11)
rmse(Data00$Load[sel_b], qgam.pred)

plot(qgam.pred, type='l')
lines(Data0$Load[sel_b],type='l', col='red')

# create submission: submission_qgamL3.csv 
submit <- read_delim( file="Data/sample_submission.csv", delim=",")
submit$Load <- qgam.pred
write.table(submit, file="Data/submission_qgamL3.csv", quote=F, sep=",", dec='.',row.names = F)
###WORST SCORE EVER


