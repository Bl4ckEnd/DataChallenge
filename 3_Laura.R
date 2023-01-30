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


Data0$WeekDays2 = factor(Data0$WeekDays2)
#Data0$Date = factor(Data0$Date)
#Data0$Time = factor(Data0$Time)
Data0$DLS = factor(Data0$DLS)
Data0$Month = factor(Data0$Month)
Data0$Summer_break = factor(Data0$Summer_break)
Data0$Christmas_break = factor(Data0$Christmas_break)

Data1$DLS = factor(Data1$DLS)
Data1$Month = factor(Data1$Month)
Data1$Summer_break = factor(Data1$Summer_break)
Data1$Christmas_break = factor(Data1$Christmas_break)


##### training models

#rl = glm(Load~WeekDays2+Month+DLS+Summer_break+Christmas_break , data=Data0[sel_a,])

'''
mod0 = gam(Load ~ toy + TauxPopMovement +
             s(HI, k = 10, bs = "cc") +
             s(Temp_s95_min, k = 10, bs = "cc") + 
             s(Temp_s95_max, k=10, bs="cc")+
             s(Load.7, k=10, bs="cc"), data=Data0[sel_a,])

#pp = seq(0,1,0.01)
#pred.rl = predict(rl, newdata=Data0[sel_b,])
pred.mod0 = predict(mod0, newdata=Data0[sel_b,])

fct = function(p){
  n = length(p)
  result = rep(0,n)
  for (i in 1:n){
    pp = p[i]
    mod0.forecast = (pp)*pred.mod0 + (1-pp)*pred.rl
    result[i] = rmse(y=Data0$Load[sel_b], ychap=mod0.forecast)
    }
  return(result)
}
plot(pp, fct(pp))
pp[which.min(fct(pp))]
RMSE_min = fct(pp[which.min(fct(pp))]); RMSE_min
'''

rl = glm(Load~WeekDays2+Month+DLS+Year+toy+ TauxPopMovement, data=Data0[sel_a,])
mod0 = gam(Load ~ s(HI, k = 10, bs = "cc") +
             s(Temp_s95_min, k = 10, bs = "cc") +
             s(Temp_s95_max, k = 10, bs = "cc") +
             s(Load.1, k=10, bs="cr")+
             s(Load.7, k=10, bs="cc"), data=Data0[sel_a,])


#pred.rl = predict(rl, newdata=Data0[sel_b,])
pred.mod0 = predict(mod0, newdata=Data0[sel_b,])
#plot(0.95*pred.mod0 + (1-0.95)*pred.rl, type='l', col='black')
lines(Data0$Load[sel_b], col='red')

rmse(y=Data0$Load[sel_b], ychap=pred.mod0)


forecast <- predict(mod0, newdata=Data1)
submit <- read_delim( file="Data/sample_submission.csv", delim=",")
submit$Load <- forecast
write.table(submit, file="Data/submission:L3.csv", quote=F, sep=",", dec='.',row.names = F)





