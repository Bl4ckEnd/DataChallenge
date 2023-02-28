rm(list=objects())
library(mgcv)
library(yarrr)
library(qgam)
library(magrittr)
library(forecast)
library(tidyverse)
library(ranger)
library(opera)
rmse = function(y, ychap, digits=0){
  return(round(sqrt(mean((y-ychap)^2, na.rm=TRUE)), digits=digits))
}

load("Data/Data0.Rda")
load("Data/Data1.Rda")

###Mise en place des dataframes
Data_train = Data0
Data_test = Data1
Data_train$Time <- as.numeric(Data_train$Date)
Data_test$Time <- as.numeric(Data_test$Date)
names(Data_train)
names(Data_test)
dim(Data_train)
dim(Data_test)

#Faire des train et test à partir de Data0
sel_a <- which(Data_train$Year<=2019)
sel_b <- which(Data_train$Year>2019)

Data0 <- Data_train[sel_a, ]
Data1 <- Data_train[sel_b, ]
Data_test = add_column(Data_test, Load=lead(Data_test$Load.1, default=mean(Data_test$Load.1)), .after = "Date")

######## lm
formula <- "Load ~Load.1 + WeekDays + Load.7 + Temp  + Temp_s99_max + Temp_s99_min + WeekDays + WD + BH+ toy + Summer_break + DLS + Christmas_break + HI "
slm <- lm(formula%>%as.formula, data=Data0)
pred = predict(slm, newdata=Data1)

plot(Data1$Date, Data1$Load, type='l')
lines(Data1$Date,pred, type='l', col='red')
rmse(Data1$Load, pred)

#######Polynomial lm
formula <- "Load ~Load.1 + WeekDays + Load.7 + I(Load.7^2) + Temp + I(Temp^2) + Temp_s99_max + I(Temp_s99_max^2) + Temp_s99_min + WeekDays + WD + BH+ toy + Summer_break + DLS + Christmas_break + HI "
slm <- lm(formula%>%as.formula, data=Data0)
pred = predict(slm, newdata=Data1) #2153

plot(Data1$Date,Data1$Load, type='l')
lines(Data1$Date,pred, type='l', col='red')
rmse(Data1$Load, pred) #2000


#####Random forest
formule <- "Load ~ Month + Temp_s95_min + Temp_s95_max + HI + TauxPopMovement +Time + toy + Temp + Load.1 + Load.7 + WD + BH + Temp_s99_min + Temp_s99_max + Summer_break  + Christmas_break  + DLS"

rf<- ranger::ranger(formule, data = Data0, importance =  'permutation')
rf.forecast <- predict(rf, data = Data1)$predictions

plot(Data1$Date,Data1$Load, type='l')
lines(Data1$Date,rf.forecast, type='l', col='red')
rmse(Data1$Load, rf.forecast)

####qgam 
equation <- "Load ~ Load.1:as.factor(WeekDays) + HI + BH + Christmas_break + Summer_break + DLS + s(Temp) + s(Temp_s99_max, Temp_s99_min)+ s(Load.7) + s(Time, k=7) + s(toy, k =30, bs = 'cc', by=as.factor(WD))+ s(Temp, Time, k=20)"

gam9<-qgam(equation%>%as.formula, data=Data0, qu=0.4)
gam9.forecast <- predict(gam9, newdata=Data1)

plot(Data1$Date,Data1$Load, type='l')
lines(Data1$Date,gam9.forecast, type='l', col='red')
rmse(Data1$Load, gam9.forecast)


### ARIMA Model
arima.fit <- forecast::Arima(gam9.forecast, order = c(1,1,2), seasonal = c(0,0,2))
arima.predict <- fitted(arima.fit)

plot(Data1$Date, Data1$Load, type='l')
lines(Data1$Date, arima.predict, col="blue")
rmse(arima.predict, Data1$Load)


######online learning
# static 
equation <- "Load ~ Load.1:as.factor(WeekDays) + HI + BH + Christmas_break + Summer_break + DLS + s(Temp) + s(Temp_s99_max, Temp_s99_min)+ s(Load.7) + s(Time, k=7) + s(toy, k =30, bs = 'cc', by=as.factor(WD))+ s(Temp, Time, k=20)"
gam9<-qgam(equation%>%as.formula, data=Data0, qu=0.4)
gam9.forecast <- predict(gam9, newdata=Data1)

X <- predict(gam9, newdata=Data0, type='terms')
y = Data0$Load
###scaling columns
for (j in 1:ncol(X)){
  X[,j] <- (X[,j]-mean(X[,j])) / sd(X[,j])
}
X <- cbind(X,1)
d <- ncol(X)

X_test <- predict(gam9, newdata=Data1, type='terms')
for (j in 1:ncol(X_test)){
  X_test[,j] <- (X_test[,j]-mean(X_test[,j])) / sd(X_test[,j])
}
X_test <- cbind(X_test,1)
y_test <- Data1$Load

#dynamic

ssm <- viking::statespace(X, y)
gam9.kalman.static <- ssm$pred_mean%>%tail(nrow(Data1))
ssm_dyn <- viking::select_Kalman_variances(ssm, X, y, q_list = 2^(-30:0), p1 = 1, ncores = 6)

ssm_dyn <- predict(ssm_dyn, X_test, y_test, type='model', compute_smooth = TRUE)
gam9.kalman.Dyn <- ssm_dyn$pred_mean%>%tail(nrow(Data1))
plot(Data1$Date, Data1$Load, type='l')
lines(Data1$Date, gam9.kalman.Dyn, type='l', col='red')

#Mini aggregation d'experts 
experts <- cbind(gam9.forecast, pred,rf.forecast, gam9.kalman.static, arima.predict)%>%as.matrix
nom_exp <- c("qgam", "polynomial lm", "rf", "Kalman", "Arima")
colnames(experts) <-  nom_exp

rmse_exp <- apply(experts, 2, rmse, y=Data1$Load)
sort(rmse_exp)
cumsum_exp <- apply(Data1$Load-experts, 2, cumsum)

or <- oracle(Y=Data1$Load, experts);or
plot(Data1$Date, Data1$Load, type='l')
lines(Data1$Date, or$prediction, type='l', col='red')
#CRAZY RESULT: 746 (but oracle)
rmse(or$prediction, y=Data1$Load)

expertsM3000 <- experts-2000
expertsP3000 <- experts+2000
experts <- cbind(experts, expertsM3000, expertsP3000)
colnames(experts) <-c(nom_exp, paste0(nom_exp,  "M"), paste0(nom_exp,  "P"))
cumsum_exp <- apply(Data1$Load-experts, 2, cumsum)


agg <- mixture(Y = Data1$Load, experts = experts, model="BOA", loss.gradient=TRUE)
summary(agg)
# see why it does not work plot(agg)

or <- oracle(Y=Data1$Load, experts);

plot(Data1$Date, Data1$Load, type='l')
lines(Data1$Date, agg$prediction, type='l', col='red')
rmse(agg$prediction, y=Data1$Load) #1070
#with Data1 it's really improved: 1900 -> 1651

##### SUBMISSION qgamL19 : agg: even though is 720 --> overall score is much better 
models = c(Data1$Load, pred, rf.forecast, gam9.forecast, agg$prediction)
K <-ncol(models)
col <- rev(RColorBrewer::brewer.pal(n = max(min(K,6),4),name = "Spectral"))[1:min(K,6)]
nom_mod <- c("Real_load", "polynomial_lm", "Random_forest", "qgam", "agg_exp")

plot(Data1$Date,Data1$Load, type='l', ylab='Real_load', col="darkblue", main="Modèles pour l'aggrégation d'experts")
lines(Data1$Date,pred, type='l', ylab = "polynomial_lm", col='darkgreen')
#Random forest
lines(Data1$Date,rf.forecast, type='l',  ylab='rf',col= "yellow")
#qgam
lines(Data1$Date,gam9.forecast, type='l', ylab='qgam',col="orange")
#Aggregation d'experts
lines(Data1$Date, agg$prediction, type='l', ylab='agg_exp', col='red')
legend("bottomleft", col=col, legend=nom_mod, lty=1, bty='n')


