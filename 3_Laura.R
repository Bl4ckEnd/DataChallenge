rm(list=objects())
graphics.off()
setwd("~/M1 Maths&IA/S2/Modelisation predictive/DataChallenge")
library(tidyverse)
library(lubridate)
library(mgcv)
library(bestglm)

###Data importation
Data0 <- read_delim("Data/new_Data0.csv", delim=",")
Data1<- read_delim("Data/new_Data1.csv", delim=",")

TR_0 <- read_delim("Data/df_TempRess.csv", delim=",")
TR_1 <- read_delim("Data/df_TempRes1.csv", delim=",")

Data0 <- merge(Data0,TR_0,by=c("Date"))
Data1 <- merge(Data1,TR_1,by=c("Date"))

Data0$Time <- as.numeric(Data0$Date)
Data1$Time <- as.numeric(Data1$Date)

sel_a <- which(Data0$Year<=2019)
sel_b <- which(Data0$Year>2019)

Data0$WeekDays2 <- forcats::fct_recode(Data0$WeekDays, 'WorkDay'='Thursday' ,'WorkDay'='Tuesday', 'WorkDay' = 'Wednesday')
Data1$WeekDays2 <- forcats::fct_recode(Data1$WeekDays, 'WorkDay'='Thursday' ,'WorkDay'='Tuesday', 'WorkDay' = 'Wednesday')

#### function definition
rmse<-function(y, ychap, digits=0)
{
  return(round(sqrt(mean((y-ychap)^2,na.rm=TRUE)),digits=digits))
}
#Data0$WeekDays2 = factor(Data0$WeekDays2)
#Data0$Date = factor(Data0$Date)
#Data0$Time = factor(Data0$Time)

##### training models
mod0 <- gam(Load ~ Date+WeekDays2+DLS+Month+TauxPopMovement+
              s(Load.1,k=10, bs ="cr")+
              s(Load.7,k=10, bs ="cr")+
              s(Temp_s95_min,k=10, bs ="cr")+
              s(Temp_s95_max,k=10, bs ="cr")+
              s(Temp_s99_max,k=10, bs ="cr")+
              s(Temp_s99_min,k=10, bs ="cr")+
              s(Temp,k=10, bs ="cr")+
              s(HI,k=10, bs ="cr"),data = Data0[sel_a,], method="ML",optimizer="outer")


mod0.forecast <- predict(mod0, newdata=Data0[sel_b,])
rmse(y=Data0$Load[sel_b], ychap=mod0.forecast)

forecast <- predict(mod0, newdata=Data1)
submit <- read_delim( file="Data/sample_submission.csv", delim=",")
submit$Load <- forecast
write.table(submit, file="Data/submission:L1.csv", quote=F, sep=",", dec='.',row.names = F)





