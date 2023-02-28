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

Data0$Time <- as.numeric(Data0$Date)

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

#Faire des train et test à partir de Data0
sel_a <- which(Data_train$Year<=2019)
sel_b <- which(Data_train$Year>2019)

Data0 <- Data_train[sel_a, ]
Data1 <- Data_train[sel_b, ]

equation <- "Load ~ Load.1:as.factor(WeekDays) + HI + BH + Christmas_break + Summer_break + DLS + s(Temp) + s(Temp_s99_max, Temp_s99_min)+ s(Load.7) + s(Time, k=7) + s(toy, k =30, bs = 'cc', by=as.factor(WD))+ s(Temp, Time, k=20)"


formula <- "Load ~Load.1 + WeekDays + Load.7 + Temp  + Temp_s99_max + Temp_s99_min + WeekDays + WD + BH+ toy + Summer_break + DLS + Christmas_break + HI "
slm <- lm(formula%>%as.formula, data=Data_train)
pred = predict(slm, newdata=Data_test)
plot(Data_test$Load, type='l')
lines(pred, type='l', col='red')
rmse(Data_test$Load[1:274], pred[1:274])

formula <- "Load ~Load.1 + WeekDays + Load.7 + I(Load.7^2) + Temp + I(Temp^2) + Temp_s99_max + I(Temp_s99_max^2) + Temp_s99_min + WeekDays + WD + BH+ toy + Summer_break + DLS + Christmas_break + HI "
slm <- lm(formula%>%as.formula, data=Data_train)
pred = predict(slm, newdata=Data_test)
plot(Data_test$Load, type='l')
lines(pred, type='l', col='red')
rmse(Data_test$Load[1:274], pred[1:274])

formule <- "Load ~ Month + Temp_s95_min + Temp_s95_max + HI + TauxPopMovement +Time + toy + Temp + Load.1 + Load.7 + WD + BH + Temp_s99_min + Temp_s99_max + Summer_break  + Christmas_break  + DLS"

rf<- ranger::ranger(formule, data = Data_train, importance =  'permutation')
rf.forecast <- predict(rf, data = Data_test)$predictions

Data_test = add_column(Data_test, Load=lead(Data_test$Load.1, default=mean(Data_test$Load.1)), .after = "Date")
plot(Data_test$Load, type='l')
lines(rf.forecast, type='l', col='red')
rmse(Data_test$Load[1:274], rf.forecast[1:274])

#Best 1056 in whole rmse 
gam9<-qgam(equation%>%as.formula, data=Data_train, qu=0.4)
gam9.forecast <- predict(gam9, newdata=Data_test)

plot(Data_test$Load, type='l')
lines(gam9.forecast, type='l', col='red')

rmse(Data_test$Load[1:274], gam9.forecast[1:274])

#Mini aggregation d'experts 
experts <- cbind(gam9.forecast, pred,rf.forecast)%>%as.matrix
nom_exp <- c("qgam", "polynomial lm", "rf")
colnames(experts) <-  nom_exp

rmse_exp <- apply(experts, 2, rmse, y=Data_test$Load)
sort(rmse_exp)
cumsum_exp <- apply(Data_test$Load-experts, 2, cumsum)

or <- oracle(Y=Data_test$Load, experts);or
plot(Data_test$Date, Data_test$Load, type='l')
lines(Data_test$Date, or$prediction, typpe='l', col='red')
#CRAZY RESULT: 770
rmse(or$prediction[1:274], y=Data_test$Load[1:274])

expertsM3000 <- experts-2000
expertsP3000 <- experts+2000
experts <- cbind(experts, expertsM3000, expertsP3000)
colnames(experts) <-c(nom_exp, paste0(nom_exp,  "M"), paste0(nom_exp,  "P"))
cumsum_exp <- apply(Data_test$Load-experts, 2, cumsum)


agg <- mixture(Y = Data_test$Load, experts = experts, loss.gradient=TRUE)
summary(agg)
plot(Data_test$Date, Data_test$Load, type='l')
lines(Data_test$Date, agg$prediction, type='l', col='red')
rmse(agg$prediction[1:274], y=Data_test$Load[1:274]) #837

##### SUBMISSION qgamL19 : agg: even though is 720 --> overall score is much better 

######online learning

X <- predict(gam9, newdata=Data_train, type='terms')
###scaling columns
for (j in 1:ncol(X)){
  X[,j] <- (X[,j]-mean(X[,j])) / sd(X[,j])
}
X <- cbind(X,1)
d <- ncol(X)

X_test <- predict(gam9, newdata=Data_test, type='terms')
for (j in 1:ncol(X_test)){
  X_test[,j] <- (X_test[,j]-mean(X_test[,j])) / sd(X_test[,j])
}
X_test <- cbind(X_test,1)
y_test <- Data_test$Load

# static 
ssm <- viking::statespace(X, y)
gam9.kalman.static <- ssm$pred_mean%>%tail(nrow(Data_test))
ssm_dyn <- viking::select_Kalman_variances(ssm, X, y, q_list = 2^(-30:0), p1 = 1, ncores = 6)

ssm_dyn <- predict(ssm_dyn, X_test, y_test, type='model', compute_smooth = TRUE)
gam9.kalman.Dyn <- ssm_dyn$pred_mean%>%tail(nrow(Data_test))
plot(Data_test$Date, Data_test$Load, type='l')
lines(Data_test$Date, 0.05*gam9.kalman.Dyn + 0.95*gam9.forecast, type='l', col='red')
rmse(Data_test$Load, 0.05*gam9.kalman.Dyn + 0.95*gam9.forecast)

