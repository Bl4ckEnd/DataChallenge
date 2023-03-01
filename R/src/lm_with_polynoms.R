rm(list=objects())
library(tidyverse)
library(lubridate)
library(mgcv)

load("Data/Data0.Rda")
load("Data/Data1.Rda")

sel_a <- which(Data0$Year<=2019)
sel_b <- which(Data0$Year>2019)

source('R/score.R')
source("R/mape.R")


mod <- lm(Load ~ ., data=Data0)
summary(mod)

######### test lm with polynomials

mod0 <- lm(Load ~ Temp + WeekDays + Load.1 + Temp^2 + WeekDays^2 + Load.1^2, data=Data0[sel_a,])
mod0.pred <- predict(mod0, Data0[sel_b,])
rmse(Data0[sel_b,]$Load, mod0.pred)

mod <- lm(Load ~ Temp + Time + DLS + Summer_break + Christmas_break
           + WeekDays + WeekDays2 + toy
           + poly(Load.1, 2) + poly(Temp_s99, 4) + poly(Load.7, 3)
           , data=Data0[sel_a,])
mod.pred <- predict(mod, Data0[sel_b,])
rmse(Data0[sel_b,]$Load, mod.pred)

summary(mod)

mod <- lm(Load ~ . + I(Temp^2) + I(Load.1^2) + I(Load.7^2) + I(GovernmentResponseIndex^2), data=Data0[sel_a,])
mod.pred <- predict(mod, Data0[sel_b,])
rmse(Data0[sel_b,]$Load, mod.pred)

##### create a submission
mod <- lm(Load ~ . + I(Temp^2) + I(Load.1^2) + I(Load.7^2) + I(GovernmentResponseIndex^2), data=Data0)
mod.full_pred <- predict(mod, Data1)

submit <- read_delim( file="Data/sample_submission.csv", delim=",")
submit$Load <- mod.full_pred
write.table(submit, file="Data/submission_lm_with_polynomial.csv", quote=F, sep=",", dec='.',row.names = F)
