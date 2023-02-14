rm(list=objects())
library(tidyverse)
library(lubridate)
library(mgcv)
library(qgam)
library(forecast)

rmse = function(y, ychap, digits=0){
  return(round(sqrt(mean((y-ychap)^2, na.rm=TRUE)), digits=digits))
}

load("Data/Data0.Rda")
load("Data/Data1.Rda")
Data = load("Data/Data0.Rda")

Data0$Time <- as.numeric(Data0$Date)

sel_a <- which(Data0$Year<=2019)
sel_b <- which(Data0$Year>2019)

Data1 <- Data0[sel_b, ]
Data0 <- Data0[sel_a, ]

help("mgcv-package")

### try the same with qgam and add "mood WeekDays" :
WD = Data0$WeekDays
index = which(Data0$GovernmentResponseIndex>=70)
WD[index]="Saturday"
Data0 = cbind(Data0,WD)

WD = Data1$WeekDays
index = which(Data1$GovernmentResponseIndex>=70)
WD[index]="Saturday"
Data11 = cbind(Data1, WD)
'''
mod0 <- qgam(Load ~ Load.1:as.factor(WeekDays) + BH + Christmas_break
             + Summer_break + DLS + s(Temp) + s(Temp_s99_max, Temp_s99_min)
             + s(Load.7) + s(Time, k=7) + s(toy, k =30, bs = "cc", by=as.factor(WD))
             + s(Temp, Time, k=20), 
             data=Data00, qu=0.4)

gam.check(mod0)
qgam.pred <- predict(mod0, Data11)
'''

equation <- "Load ~ Load.1:as.factor(WeekDays) + BH + Christmas_break + Summer_break + DLS + s(Temp) + s(Temp_s99_max, Temp_s99_min)+ s(Load.7) + s(Time, k=7) + s(toy, k =30, bs = 'cc', by=as.factor(WD))+ s(Temp, Time, k=20)"

gam.0<-qgam(equation%>%as.formula, data=Data00, qu=0.4)
gam.forecast <- predict(gam.0, newdata=Data11)

X <- predict(gam.0, newdata=Data0, type='terms')

for (j in 1:ncol(X)){
  X[,j] <- (X[,j]-mean(X[,j])) / sd(X[,j])
}
  
X <- cbind(X,1)
d <- ncol(X)

y <- Data11$Load

ssm <- viking::statespace(X, y)

ssm_dyn <- viking::select_Kalman_variances(ssm, X[sel_a, ], y[sel_a], q_list = 2^(-30:0), p1 = 1, ncores = 6)

ssm_dyn <- readRDS("Results/ssm_dyn2.RDS")

ssm_dyn <- predict(ssm_dyn, X, y, type='model', compute_smooth = TRUE)
gam9.kalman.Dyn <- ssm_dyn$pred_mean%>%tail(nrow(Data1))
rmse(y=Data1$Load, ychap=gam9.kalman.Dyn)
plot(ssm_dyn, pause=F, window_size = 14, date = Data$Date)

plot(ssm_dyn, pause=F, window_size = 14, date = Data$Date, sel = sel_b)


# using expectation-maximization
ssm_em <- viking::select_Kalman_variances(ssm, X[sel_a,], y[sel_a], method = 'em', n_iter = 10^3,
                                          Q_init = diag(d), verbose = 10, mode_diag = T)
ssm_em <- predict(ssm_em, X, y, type='model', compute_smooth = TRUE)
ssm_em <-readRDS("Results/ssm_em.RDS")

gam9.kalman.Dyn.em <- ssm_em$pred_mean%>%tail(nrow(Data1))

