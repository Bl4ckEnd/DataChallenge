rm(list=objects())
library(tidyverse)
library(lubridate)
library(mgcv)
library(qgam)
library(forecast)

rmse = function(y, ychap, digits=0){
  return(round(sqrt(mean((y-ychap)^2, na.rm=TRUE)), digits=digits))
}

help("mgcv-package")

### try the same with qgam and add "mood WeekDays" :

'''
mod0 <- qgam(Load ~ Load.1:as.factor(WeekDays) + BH + Christmas_break
             + Summer_break + DLS + s(Temp) + s(Temp_s99_max, Temp_s99_min)
             + s(Load.7) + s(Time, k=7) + s(toy, k =30, bs = "cc", by=as.factor(WD))
             + s(Temp, Time, k=20), 
             data=Data00, qu=0.4)

gam.check(mod0)
qgam.pred <- predict(mod0, Data11)
'''

############ APPLICATION A TOUT LE JEU DE DONNÉES
load("Data/Data0.Rda")
load("Data/Data1.Rda")

Data0$Time <- as.numeric(Data0$Date)

WD = Data0$WeekDays
index = which(Data0$GovernmentResponseIndex>=70)
WD[index]="Saturday"
Data0 = cbind(Data0,WD)

WD = Data1$WeekDays
index = which(Data1$GovernmentResponseIndex>=70)
WD[index]="Saturday"
Data1 = cbind(Data1, WD)


equation <- "Load ~ Load.1:as.factor(WeekDays) + BH + Christmas_break + Summer_break + DLS + s(Temp) + s(Temp_s99_max, Temp_s99_min)+ s(Load.7) + s(Time, k=7) + s(toy, k =30, bs = 'cc', by=as.factor(WD))+ s(Temp, Time, k=20)"

gam.0<-qgam(equation%>%as.formula, data=Data0, qu=0.4)
gam.forecast <- predict(gam.0, newdata=Data1)
Data1 = Data1[,-20]
#Submission 14: Data1 = add_column(Data1, Load=lead(Data1$Load.1, default=mean(Data1$Load.1)), .after = "Date")
Data1 = cbind(Data1, Load=gam.forecast) #submission 12 best so far

Data = rbind(Data0,Data1)

X <- predict(gam.0, newdata=Data, type='terms')

for (j in 1:ncol(X)){
  X[,j] <- (X[,j]-mean(X[,j])) / sd(X[,j])
}

X <- cbind(X,1)
d <- ncol(X)

y <- Data$Load

#static
ssm <- viking::statespace(X, y)

#Dynamic
#ssm_dyn <- viking::select_Kalman_variances(ssm, X[1:3051,], y[1:3051], q_list = 2^(-30:0), p1 = 1, ncores = 6)
#saveRDS(ssm_dyn, "Results/ssm_dyn.RDS")
ssm_dyn = readRDS("Results/ssm_dyn.RDS")

ssm_dyn <- predict(ssm_dyn, X, y, type='model', compute_smooth = TRUE)
gam9.kalman.Dyn <- ssm_dyn$pred_mean%>%tail(nrow(Data1))
rmse(y=Data1$Load, ychap=gam9.kalman.Dyn)
plot(ssm_dyn, pause=F, window_size = 14, date = Data$Date)

plot(ssm_dyn, pause=F, window_size = 14, date = Data$Date)

##### SUBMISSION qgamL12 BEST SO FAR
#submit <- read_delim( file="Data/sample_submission.csv", delim=",")
#submit$Load <- gam9.kalman.Dyn
#write.table(submit, file="Data/submission_qgamL15.csv", quote=F, sep=",", dec='.',row.names = F)

##### SUBMISSION qgamL13 SAME AS BEFORE
# using expectation-maximization
ssm_em <- viking::select_Kalman_variances(ssm, X[1:3051,], y[1:3051], method = 'em', n_iter = 10^3,
                                          Q_init = diag(d), verbose = 10, mode_diag = T)
ssm_em <- predict(ssm_em, X, y, type='model', compute_smooth = TRUE)

gam9.kalman.Dyn.em <- ssm_em$pred_mean%>%tail(nrow(Data1))
plot(ssm_em, pause=F, window_size = 14, date = Data$Date, sel = sel_b)
rmse(y=Data1$Load, ychap=gam9.kalman.Dyn.em)

plot(Data1$Date, Data1$Load.1, type='l')
lines(Data1$Date, gam9.kalman.Dyn.em, col='red')










