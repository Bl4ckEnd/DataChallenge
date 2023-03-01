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

###Load Datasets
load("../Data/Data0.Rda")
load("../Data/Data1.Rda")

### Change weekdays --> saturdays if govresponseindex>0.70

###Mise en place des dataframes
Data_train = Data0
Data_test = Data1
Data_train$Time <- as.numeric(Data_train$Date)
Data_test$Time <- as.numeric(Data_test$Date)
Data_train$GRI_factor = as.factor(Data_train$GRI_factor)
Data_test$GRI_factor = as.factor(Data_test$GRI_factor)
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

###Choix des variables 
equation <- "Load~  Time + toy + Temp + Load.1 + Load.7 + Temp_s99 + WeekDays + BH + Temp_s95_max + 
  Temp_s99_max + Summer_break  + Christmas_break + 
  Temp_s95_min +Temp_s99_min + DLS + GovernmentResponseIndex + TauxPopMovement + Movement + HI + WD"
rf <- ranger(equation, data=Data0, importance =  'permutation')

#############importance plot
imp <- rf$variable.importance
imp <- sort(imp)
o <- order(imp, decreasing=T)
nom <- names(imp)
plot(c(1:length(imp)), imp[o], type='h', ylim = c(0, max(imp) + max(imp)/5), xlab='', ylab='Importance (permutation)')
K <- length(imp)
text(tail(c(1:length(imp)), K), tail(imp[o]+max(imp/8), K), labels= tail(nom[o], K), pos=3, srt=90, adj=1, cex=0.6)
points(c(1:length(imp)), imp[o], pch=20)

####### ANOVA pour Movement 
formula1 <- "Load ~ Temp + WeekDays + WD + BH+ toy + Summer_break + DLS + Christmas_break"
formula2 <- "Load ~ Temp + WeekDays + WD + BH+ toy + Summer_break + DLS + Christmas_break + Movement"
small_lm <- lm(formula1%>%as.formula, data=Data0)
large_lm <- lm(formula2%>%as.formula, data=Data0)
anova.res <- anova(small_lm, large_lm)
summary(anova.res)


######## lm
formula <- "Load ~Load.1 + WeekDays + Load.7 + Temp  + Temp_s99_max + Temp_s99_min + WeekDays + WD + BH+ toy + Summer_break + DLS + Christmas_break + HI "
slm <- lm(formula%>%as.formula, data=Data0)
pred = predict(slm, newdata=Data1)

plot(Data1$Date, Data1$Load, type='l', main='lm')
lines(Data1$Date,pred, type='l', col='red')
rmse(Data1$Load, pred)

#######Polynomial lm
formula <- "Load ~Load.1 + WeekDays + Load.7 + I(Load.7^2) + Temp + I(Temp^2) + Temp_s99_max + I(Temp_s99_max^2) + Temp_s99_min + WeekDays + WD + BH+ toy + Summer_break + DLS + Christmas_break + HI "
slm <- lm(formula%>%as.formula, data=Data0)
pred = predict(slm, newdata=Data1) #2153

plot(Data1$Date,Data1$Load, type='l',main='polynomial lm')
lines(Data1$Date,pred, type='l', col='red')
rmse(Data1$Load, pred) #2000


#####Random forest
formule <- "Load ~ Month + Temp_s95_min + Temp_s95_max + HI + TauxPopMovement +Time + toy + Temp + Load.1 + Load.7 + WD + BH + Temp_s99_min + Temp_s99_max + Summer_break  + Christmas_break  + DLS"

rf<- ranger::ranger(formule, data = Data0, importance =  'permutation')
rf.forecast <- predict(rf, data = Data1)$predictions

plot(Data1$Date,Data1$Load, type='l', main='rf')
lines(Data1$Date,rf.forecast, type='l', col='red')
rmse(Data1$Load, rf.forecast)

####gam 
equation <- "Load ~ Load.1:as.factor(WeekDays) + HI + BH + Christmas_break + Summer_break + DLS + s(Temp) + s(Temp_s99_max, Temp_s99_min)+ s(Load.7) + s(Time, k=7) + s(toy, k =30, bs = 'cc', by=as.factor(WD))+ s(Temp, Time, k=20)"

gam<-gam(equation%>%as.formula, data=Data0)
gam.forecast <- predict(gam, newdata=Data1)

plot(Data1$Date,Data1$Load, type='l', main='gam')
lines(Data1$Date,gam.forecast, type='l', col='red')
rmse(Data1$Load, gam.forecast)

###check gam
gam.check(gam)

####qgam 
gam9<-qgam(equation%>%as.formula, data=Data0, qu=0.4)
gam9.forecast <- predict(gam9, newdata=Data1)

plot(Data1$Date,Data1$Load, type='l', main='qgam')
lines(Data1$Date,gam9.forecast, type='l', col='red')
rmse(Data1$Load, gam9.forecast)


### ARIMA Model
arima.fit <- forecast::Arima(gam9.forecast, order = c(1,1,2), seasonal = c(0,0,2))
arima.predict <- fitted(arima.fit)

plot(Data1$Date, Data1$Load, type='l', main='arima')
lines(Data1$Date, arima.predict, col="red")
rmse(arima.predict, Data1$Load)


######online learning
# static 
#On doit le faire ici sur tout le jeu train car sinon on n'a pas d'effectifs dans chaque sous modalité
X <- predict(gam9, newdata=Data_train, type='terms')
y = Data_train$Load
###scaling columns
for (j in 1:ncol(X)){
  X[,j] <- (X[,j]-mean(X[,j])) / sd(X[,j])
}
X <- cbind(X,1)
d <- ncol(X)


#dynamic
ssm <- viking::statespace(X, y)
gam9.kalman.static <- ssm$pred_mean%>%tail(nrow(Data1))
#ssm_dyn <- viking::select_Kalman_variances(ssm, X[sel_a,], y[sel_a], q_list = 2^(-30:0), p1 = 1, ncores = 6)
#saveRDS(ssm_dyn, "../Results/Kalman_filter_Data_train.RDS")
#ssm_dyn = readRDS("../Results/Kalman_filter_Data_train.RDS")
ssm_dyn <- predict(ssm_dyn, X, y, type='model', compute_smooth = TRUE)
gam9.kalman.Dyn <- ssm_dyn$pred_mean%>%tail(nrow(Data1))
plot(Data1$Date, Data1$Load, type='l')
lines(Data1$Date, gam9.kalman.Dyn, type='l', col='red')
rmse(Data1$Load, gam9.kalman.Dyn)

####Pipeline
Nblock<-10
borne_block<-seq(1, nrow(Data0), length=Nblock+1)%>%floor
block_list<-list()
l<-length(borne_block)
for(i in c(2:(l-1)))
{
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]]<-c(borne_block[l-1]:(borne_block[l]))


blockRMSE<-function(equation, block)
{
  g<- gam(as.formula(equation), data=Data0[-block,])
  forecast<-predict(g, newdata=Data0[block,])
  return(forecast)
} 

Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
Block_residuals <- Data0$Load-Block_forecast

####estimation of GAM, GAM effects
g <- gam9
g.forecast <- predict(g, newdata=Data1)
terms0 <- predict(g, newdata=Data0, type='terms')
terms1 <- predict(g, newdata=Data1, type='terms')
colnames(terms0) <- paste0("gterms_", c(1:ncol(terms0)))
colnames(terms1) <- paste0("gterms_", c(1:ncol(terms1)))

Data0_rf <- data.frame(Data0, terms0)
residualsCV <- Block_residuals

Data0_rf$residuals <- residualsCV
Data0_rf$res.48 <- c(residualsCV[1], residualsCV[1:(length(residualsCV)-1)])
Data0_rf$res.336 <- c(residualsCV[1:7], residualsCV[1:(length(residualsCV)-7)])

Data1_rf <- data.frame(Data1, terms1)
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

rmse(y=Data1$Load, ychap=rf_gam.forecast)
rf_gam$variable.importance%>%sort


Block_residuals.ts <- ts(Block_residuals, frequency=7)
fit.arima.res <- auto.arima(Block_residuals.ts,max.p=3,max.q=4, max.P=2, max.Q=2, trace=T,ic="aic", method="CSS")
#Best model: ARIMA(2,1,2)(2,0,0)[7]
#saveRDS(fit.arima.res, "../Results/tif.arima.res.RDS")
ts_res_forecast <- ts(c(Block_residuals.ts, Data1$Load-gam9.forecast),  frequency= 7)
refit <- Arima(ts_res_forecast, model=fit.arima.res)
prevARIMA.res <- tail(refit$fitted, nrow(Data1))
gam9.arima.forecast <- gam9.forecast + prevARIMA.res


#####Aggregation d'experts 
experts <- cbind(gam9.forecast, pred,rf.forecast, gam.forecast, gam9.kalman.Dyn, arima.predict, as.numeric(gam9.arima.forecast))%>%as.matrix
nom_exp <- c("qgam", "polynomial_lm", "rf", "gam", "kalman", "arima", "pipeline")
colnames(experts) <-  nom_exp

rmse_exp <- apply(experts, 2, rmse, y=Data1$Load)
sort(rmse_exp)
cumsum_exp <- apply(Data1$Load-experts, 2, cumsum)

#Correction du biais
expertsM2000 <- experts-2000
expertsP2000 <- experts+2000
experts <- cbind(experts, expertsM2000, expertsP2000)
colnames(experts) <-c(nom_exp, paste0(nom_exp,  "M"), paste0(nom_exp,  "P"))
cumsum_exp <- apply(Data1$Load-experts, 2, cumsum)

par(mfrow=c(1,1))
agg <- mixture(Y = Data1$Load, experts = experts, loss.gradient=TRUE)
summary(agg)
####PLOT EXPERT AGGREGATION
plot(agg)
or <- oracle(Y=Data1$Load, experts);or

par(mfrow=c(1,1))
plot(Data1$Date, Data1$Load, type='l')
lines(Data1$Date, or$prediction, type='l', col='yellow')
lines(Data1$Date, agg$prediction, type='l', col='red')
rmse(agg$prediction, y=Data1$Load)
#with Data1 it's really improved: 1900 -> 1339


####FIGURE PLOT 
models = c(Data1$Load, pred, rf.forecast, gam.forecast, gam9.forecast, gam9.kalman.Dyn, gam9.arima.forecast, agg$prediction)
K <-ncol(models)
col <- rev(RColorBrewer::brewer.pal(n = max(min(K,8),6),name = "Spectral"))[1:min(K,8)]
nom_mod <- c("Real_load", "polynomial_lm", "Random_forest", "gam","qgam", "kalman", "pipeline", "agg_exp")

plot(Data1$Date,Data1$Load, type='l', ylab='Real_load', col="darkblue", main="Prédictions",lwd=3 )
lines(Data1$Date,pred, type='l', ylab = "polynomial_lm", col='forestgreen')
#Random forest
lines(Data1$Date,rf.forecast, type='l',  ylab='rf',col= "lightgreen")
#gam
lines(Data1$Date,gam.forecast, type='l', ylab='gam',col="yellow")
#qgam
lines(Data1$Date,gam9.forecast, type='l', ylab='qgam',col="orange")
#Pipeline
lines(Data1$Date,gam9.arima.forecast, type='l', ylab='qgam',col="darkorange")
#Aggregation d'experts
lines(Data1$Date, agg$prediction, type='l', ylab='agg_exp', col='red')
legend("bottomleft", col=col, legend=nom_mod, lty=1, bty='n')
