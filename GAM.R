rm(list=objects())
library(tidyverse)
library(lubridate)
library(mgcv)


Data0 <- read_delim("Data/new_Data0.csv", delim=",")
Data1<- read_delim("Data/new_Data1.csv", delim=",")

Data0$Time <- as.numeric(Data0$Date)
Data1$Time <- as.numeric(Data1$Date)

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

# create submission
submit <- read_delim( file="Data/sample_submission.csv", delim=",")
submit$Load <- data1.predicted
write.table(submit, file="Data/gam_g2.csv", quote=F, sep=",", dec='.',row.names = F)
