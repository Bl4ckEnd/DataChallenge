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


######## test different gam models
g1 <- gam(formula = Load ~ WeekDays + s(Load.1, k=10, bs="cr") + s(Temp, k=10, bs="cr"), data=Data0)
g1.predicted = predict.gam(g1, Data0)
rmse(Data0$Load, g1.predicted)


### full model
g2 <- gam(formula = Load ~ WeekDays + Christmas_break + Summer_break + DLS + Month + BH + Movement
          + GovernmentResponseIndex
          + s(TauxPopMovement, k=5, bs="cr")
          + s(Load.1, k=15, bs="cr")
          + s(Load.7, k=15, bs="cr")
          + s(Temp, k=15, bs="cc")
          + s(Temp_s95, k=10, bs="cr")
          + s(Temp_s99, k=10, bs="cr")
          + s(Temp_s95_min, k=10, bs="cr")
          + s(Temp_s99_min, k=10, bs="cr")
          + s(toy, k=10, bs="cr")
          + s(Temp_s95, k=10, bs="cr"), data=Data0[sel_a,])

g2.predicted <- predict.gam(g2, Data0[sel_b,])
rmse(Data0$Load[sel_b], g2.predicted)

### try validation set
g2_sela <- gam(formula = Load ~ WeekDays + Christmas_break + Summer_break + DLS + Month + BH
          + GovernmentResponseIndex + Movement
          + s(TauxPopMovement, k=5, bs="cr")
          + s(Load.1, k=10, bs="cr")
          + s(Load.7, k=10, bs="cr")
          + s(Temp, k=10, bs="cr")
          + s(Temp_s95, k=10, bs="cr")
          + s(Temp_s99, k=10, bs="cr")
          + s(Temp_s95_min, k=10, bs="cr")
          + s(Temp_s99_min, k=10, bs="cr")
          + s(toy, k=10, bs="cr")
          + s(Temp_s95, k=10, bs="cr"), data=Data0[sel_a,])

g2_sela.predicted <- predict.gam(g2, newdata = Data0[sel_b,])
rmse(Data0[sel_b,]$Load, g2_sela.predicted)
plot(Data0[sel_b,]$Date, Data0[sel_b,]$Load, type="l")
lines(Data0[sel_b,]$Date, g2_sela.predicted, type="l", col=alpha("red", alpha=0.4))


######### plots
plot(Data0$Date, Data0$Load, type="l")
lines(Data0$Date, g1.predicted, type="l", col=alpha("red", 0.4))

##### lm
lm.data0 = Data0[sel_a,]
lm.data0$Summer_break = as.factor(lm.data0$Summer_break)
lm.data0$Christmas_break = as.factor(lm.data0$Christmas_break)
lm.data1 = Data0[sel_b,]
lm.data1$Summer_break = as.factor(lm.data1$Summer_break)
lm.data1$Christmas_break = as.factor(lm.data1$Christmas_break)


mod.lm = lm(Load~., data=lm.data0)
lm.pred = predict(mod.lm, newdata=lm.data1)
rmse(lm.data1$Load, lm.pred)


data1.predicted = predict.gam(g2, Data1)


####################
####################
# Create base-model from paper

mod.gam <- gam(Load ~ WeekDays2 + BH + Christmas_break + Load.1
               + Summer_break + DLS
               + s(Load.7) + s(Time) + s(Temp) + s(toy, k = 3, bs = "cc"),
               data=Data0[sel_a,])

gam.pred <- predict(mod.gam, Data0[sel_b,])
rmse(Data0[sel_b,]$Load, gam.pred)
summary(mod.gam)

### try the same with qgam - gam with quantile regression
mod.qgam <- qgam(Load ~ WeekDays2 + BH + Christmas_break + Load.1
               + Summer_break + DLS
               + s(Load.7) + s(Time) + s(Temp) + s(toy, k = 3, bs = "cc", by=WeekDays2)
               + s(Temp_s99_min, temp_s99_max)
               + s(Temp, Time),
               data=Data0[sel_a,], qu = 0.5)

qgam.pred <- predict(mod.qgam, Data0[sel_b,])
rmse(Data0[sel_b,]$Load, qgam.pred)
summary(mod.qgam)
# if in summary edf = k-1 , k is probably too small

#### use quantile regression for submission
mod.qgam <- qgam(Load ~ WeekDays2 + BH + Christmas_break + Load.1
                 + Summer_break + DLS
                 + s(Load.7) + s(Time) + s(Temp) + s(toy, k = 3, bs = "cc", by=WeekDays2)
                 + s(Temp_s99_min, temp_s99_max)
                 + s(Temp, Time),
                 data=Data0, qu = 0.5)

qgam.pred <- predict(mod.qgam, Data1)

plot(mod.qgam$residuals)

# create submission
submit <- read_delim( file="Data/sample_submission.csv", delim=",")
submit$Load <- qgam.pred
write.table(submit, file="Data/submission_qgam.csv", quote=F, sep=",", dec='.',row.names = F)
