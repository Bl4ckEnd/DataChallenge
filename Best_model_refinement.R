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


rm(list=objects())
###############packages
rmse = function(y, ychap, digits=0){
  return(round(sqrt(mean((y-ychap)^2, na.rm=TRUE)), digits=digits))
}

library(mgcv)
library(yarrr)
library(magrittr)
library(forecast)
library(tidyverse)
library(ranger)
library(opera)


load("Data/Data0.Rda")
load("Data/Data1.Rda")
WD = Data0$WeekDays
index = which(Data0$GovernmentResponseIndex>=70)
WD[index]="Saturday"
Data0 = cbind(Data0,WD)

WD = Data1$WeekDays
index = which(Data1$GovernmentResponseIndex>=70)
WD[index]="Saturday"
Data1 = cbind(Data1, WD)

###Mise en place des dataframes
Data_train = Data0
Data_test = Data1
Data_train$Time <- as.numeric(Data_train$Date)
Data_test$Time <- as.numeric(Data_test$Date)
names(Data_train)
names(Data_test)
dim(Data_train)
dim(Data_test)



sel_a <- which(Data_train$Year<=2019)
sel_b <- which(Data_train$Year>2019)

Data0 <- Data_train[sel_a, ]
Data1 <- Data_train[sel_b, ]

####Random forest
Ntree <- 500
mtry <- 13
equation <- Load ~  Time + toy + Temp + Load.1 + Load.7 + Temp_s99  + BH + Temp_s95_max + 
  Temp_s99_max + Summer_break  + Christmas_break + 
  Temp_s95_min +Temp_s99_min + DLS + GovernmentResponseIndex + WD
rf<- ranger::ranger(equation, data=Data0, num.trees = Ntree, mtry=mtry)
rf.forecast <-  predict(rf, data=Data1)$predictions
rf.forecast.all <-  predict(rf, data=Data1, predict.all = TRUE)$predictions

plot(Data1$Date, Data1$Load, type='l')
for(i in c(1:500))
{
  lines(Data1$Date, rf.forecast.all[,i], type='l', col='lightblue')
}
lines(Data1$Date,rf.forecast, col='blue')

rf.forecast_update <- rf.forecast

for(i in c(1: (nrow(Data1)-1)))
{
  rf<- ranger::ranger(equation, data=rbind(Data0, Data1[1:i,]), num.trees = Ntree, mtry=mtry)
  rf.forecast_update[i+1] <- predict(rf, data=Data1[1:i+1,], predict.all = F)$predictions%>%tail(1)
  print(i)
}
#out of bag prediction error
rf$prediction.error%>%sqrt

#test error
rf.forecast <- predict(rf, data=Data1)$prediction
rmse(y=Data1$Load, ychap=rf.forecast)

plot(Data1$Date, Data1$Load, type='l')
lines(Data1$Date,rf.forecast, type='l', col='red')
lines(Data1$Date,rf.forecast_update, type='l', col='blue')

#PAS GENIAL EN VRAI
rf.forecast <- predict(rf, data=Data_test)$prediction
rmse(y=Data_test$Load.1, ychap=rf.forecast)
plot(Data_test$Date, Data_test$Load.1, type='l')
lines(Data_test$Date,rf.forecast, type='l', col='red')

rf<- ranger::ranger(equation, data=Data0, num.trees = Ntree, mtry=mtry, importance =  'permutation')

####################################################################################
#####RF GAM
#####################################################################################
####Best submission so far!!!!! ---> 567.24 

equation <- "Load ~ Load.1:as.factor(WeekDays) + BH + Christmas_break + Summer_break + DLS + s(Temp) + s(Temp_s99_max, Temp_s99_min)+ s(Load.7) + s(Time, k=7) + s(toy, k =30, bs = 'cc', by=as.factor(WD))+ s(Temp, Time, k=20)"
gam9<-qgam(equation%>%as.formula, data=Data_train, qu=0.4)
gam9.forecast <- predict(gam9, newdata=Data_test)

Nblock<-10
borne_block<-seq(1, nrow(Data_train), length=Nblock+1)%>%floor
block_list<-list()
l<-length(borne_block)
for(i in c(2:(l-1)))
{
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]]<-c(borne_block[l-1]:(borne_block[l]))


blockRMSE<-function(equation, block)
{
  g<- qgam(as.formula(equation), data=Data_train[-block,], qu=0.4)
  forecast<-predict(g, newdata=Data_train[block,])
  return(forecast)
} 

Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
Block_residuals <- Data_train$Load-Block_forecast



####estimation of GAM, GAM effects
g <- gam9
g.forecast <- predict(g, newdata=Data_test)
terms0 <- predict(g, newdata=Data_train, type='terms')
terms1 <- predict(g, newdata=Data_test, type='terms')
colnames(terms0) <- paste0("gterms_", c(1:ncol(terms0)))
colnames(terms1) <- paste0("gterms_", c(1:ncol(terms1)))

Data0_rf <- data.frame(Data_train, terms0)
residualsCV <- Block_residuals

Data0_rf$residuals <- residualsCV
Data0_rf$res.48 <- c(residualsCV[1], residualsCV[1:(length(residualsCV)-1)])
Data0_rf$res.336 <- c(residualsCV[1:7], residualsCV[1:(length(residualsCV)-7)])

Data_test = add_column(Data_test, Load=lead(Data_test$Load.1, default=mean(Data_test$Load.1)), .after = "Date")
Data1_rf <- data.frame(Data_test, terms1)
residuals <- Data1_rf$Load - gam9.forecast
Data1_rf$residuals <- residuals
Data1_rf$res.48 <- c(residuals[1], residuals[1:(length(residuals)-1)])
Data1_rf$res.336 <- c(residuals[1:7], residuals[1:(length(residuals)-7)])

cov <- "Time + toy + Temp + Load.1 + Load.7 + Temp_s99 + WeekDays + BH + Temp_s95_max + Temp_s99_max + Summer_break  + Christmas_break + 
Temp_s95_min +Temp_s99_min + DLS + GovernmentResponseIndex + res.48 + res.336 +"
gterm <-paste0("gterms_", c(1:ncol(terms0)))
gterm <- paste0(gterm, collapse='+')
cov <- paste0(cov, gterm, collapse = '+')
formule_rf <- paste0("residuals", "~", cov)
rf_gam<- ranger::ranger(formule_rf, data = Data0_rf, importance =  'permutation')
rf_gam.forecast <- predict(rf_gam, data = Data1_rf)$predictions+ g.forecast

rmse(y=Data_test$Load, ychap=rf_gam.forecast)
rf_gam$variable.importance%>%sort


Block_residuals.ts <- ts(Block_residuals, frequency=7)
fit.arima.res <- auto.arima(Block_residuals.ts,max.p=3,max.q=4, max.P=2, max.Q=2, trace=T,ic="aic", method="CSS")
#Best model: ARIMA(1,1,2)(2,0,2)[7] with block using gam
# Best model: ARIMA(1,0,3)(2,0,0)[7] with qgam
#saveRDS(fit.arima.res, "Results/tif.arima.res.RDS")
ts_res_forecast <- ts(c(Block_residuals.ts, Data_test$Load-gam9.forecast),  frequency= 7)
refit <- Arima(ts_res_forecast, model=fit.arima.res)
prevARIMA.res <- tail(refit$fitted, nrow(Data_test))
gam9.arima.forecast <- gam9.forecast + prevARIMA.res


##### SUBMISSION qgamL16 BEST SO FAR
#submit <- read_delim( file="Data/sample_submission.csv", delim=",")
#submit$Load <- gam9.arima.forecast
#write.table(submit, file="Data/submission_qgamL16.csv", quote=F, sep=",", dec='.',row.names = F)


formula <- "Load ~Load.1 + WeekDays + Load.7 + Temp  + Temp_s99_max + Temp_s99_min + WeekDays + WD + BH+ toy + Summer_break + DLS + Christmas_break + HI "
slm <- lm(formula%>%as.formula, data=Data_train)
pred = predict(slm, newdata=Data_test)
plot(Data_test$Load, type='l')
lines(pred, type='l', col='red')
rmse(Data_test$Load[1:274], pred[1:274])

formula <- "Load ~Load.1 + WeekDays + Load.7 + I(Load.7^2)+ Temp + I(Temp^2) + Temp_s99_max + I(Temp_s99_max^2) + Temp_s99_min + WeekDays + WD + BH+ toy + Summer_break + DLS + Christmas_break + HI "
slm <- lm(formula%>%as.formula, data=Data_train)
pred = predict(slm, newdata=Data_test)
plot(Data_test$Load, type='l')
lines(pred, type='l', col='red')
rmse(Data_test$Load[1:274], pred[1:274])

formule <- "Load ~ Month + Temp_s95_min + Temp_s95_max + HI + TauxPopMovement +Time + toy + Temp + Load.1 + Load.7 + WD + BH + Temp_s99_min + Temp_s99_max + Summer_break  + Christmas_break  + DLS"

rf<- ranger::ranger(formule, data = Data_train, importance =  'permutation')
rf.forecast <- predict(rf, data = Data_test)$predictions

plot(Data_test$Load, type='l')
lines(rf.forecast, type='l', col='red')
rmse(Data_test$Load[1:274], rf.forecast[1:274])

gam9<-qgam(equation%>%as.formula, data=Data_train, qu=0.4)
gam9.forecast <- predict(gam9, newdata=Data_test)

