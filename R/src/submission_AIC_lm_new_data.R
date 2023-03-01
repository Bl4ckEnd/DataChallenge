rm(list=objects())
library(tidyverse)
library(lubridate)
library(MASS)

Data0 <- read_delim("Data/new_Data0.csv", delim=",")
Data1<- read_delim("Data/new_Data1.csv", delim=",")

Data0$Time <- as.numeric(Data0$Date)
Data1$Time <- as.numeric(Data1$Date)

source('R/score.R')
source("R/mape.R")


mod = lm(Load ~., data=Data0)
mod0 <- lm(Load ~ WeekDays, data=Data0)
stepAIC(mod)
mod2 = lm(formula = Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s99 + 
            Temp_s95_max + Temp_s99_min + Temp_s99_max + toy + WeekDays + 
            BH + Month + DLS + Summer_break + Christmas_break + GovernmentResponseIndex, 
          data = Data0)
AIC(mod2)
AIC(mod)

lm.forecast <- predict(mod, newdata=Data1)
submit <- read_delim( file="Data/sample_submission.csv", delim=",")
submit$Load <- lm.forecast
write.table(submit, file="Data/submission_full_lm.csv", quote=F, sep=",", dec='.',row.names = F)