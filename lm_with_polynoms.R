rm(list=objects())
library(tidyverse)
library(lubridate)
library(mgcv)


Data0 <- read_delim("Data/new_Data0.csv", delim=",")
Data1<- read_delim("Data/new_Data1.csv", delim=",")

Data0$WeekDays2 <- forcats::fct_recode(Data0$WeekDays, 'WorkDay'='Thursday' ,'WorkDay'='Tuesday', 'WorkDay' = 'Wednesday')
Data1$WeekDays2 <- forcats::fct_recode(Data1$WeekDays, 'WorkDay'='Thursday' ,'WorkDay'='Tuesday', 'WorkDay' = 'Wednesday')

Data0$Summer_break <- factor(Data0$Summer_break)
Data1$Summer_break <- factor(Data1$Summer_break)

Data0$DLS = factor(Data0$DLS)
Data1$DLS = factor(Data1$DLS)

Data0$Month = factor(Data0$Month)
Data1$Month = factor(Data1$Month)

Data0$Christmas_break = factor(Data0$Christmas_break)
Data1$Christmas_break = factor(Data1$Christmas_break)


Data0$Time <- as.numeric(Data0$Date)
Data1$Time <- as.numeric(Data1$Date)

TR_0 <- read_delim("Data/df_TempRess.csv", delim=",")
TR_1 <- read_delim("Data/df_TempRes1.csv", delim=",")

Data0 <- merge(Data0,TR_0,by=c("Date"))
Data1 <- merge(Data1,TR_1,by=c("Date"))

write.csv(Data0, file="Data/FinalData0.csv")
write.csv(Data1, file="Data/FinalData1.csv")

save(Data0, file="Data/Data0.Rda")
save(Data1, file="Data/Data1.Rda")

load("Data/Data0.Rda")

sel_a <- which(Data0$Year<=2019)
sel_b <- which(Data0$Year>2019)

source('R/score.R')
source("R/mape.R")

Data0

mod <- lm(Load ~ ., data=Data0)
summary(mod)

######### test lm with polynomials
mod0 <- lm(Load ~ Temp + WeekDays + Load.1 + Temp^2 + WeekDays^2 + Load.1^2, data=Data0)
mod0.pred <- predict(mod0, Data0)
rmse(Data0$Load, mod0.pred)
