rm(list=objects())
library(mgcv)
library(yarrr)
library(qgam)
library(magrittr)
library(forecast)
library(tidyverse)
library(ranger)
library(opera)

# Definition de la fonction rmse
rmse = function(y, ychap, digits=0){
  return(round(sqrt(mean((y-ychap)^2, na.rm=TRUE)), digits=digits))
}

### Chargement des Datasets
load("Data/Data0.Rda")
load("Data/Data1.Rda")


### Mise en place des Datasets train et test 
Data_train = Data0
Data_test = Data1
Data_train$Time <- as.numeric(Data_train$Date)
Data_test$Time <- as.numeric(Data_test$Date)

Data_train$GRI_factor = as.factor(Data_train$GRI_factor)
Data_test$GRI_factor = as.factor(Data_test$GRI_factor)

names(Data_train);names(Data_test)
dim(Data_train);dim(Data_test)

# Faire des train et test à partir de Data0
sel_a <- which(Data_train$Year<=2019)
sel_b <- which(Data_train$Year>2019)

Data0 <- Data_train[sel_a, ]
Data1 <- Data_train[sel_b, ]


### Choix des variables avec rf
equation <- "Load~  Time + toy + Temp + Load.1 + Load.7 + Temp_s99 + WeekDays + BH + Temp_s95_max + 
  Temp_s99_max + Summer_break  + Christmas_break + 
  Temp_s95_min +Temp_s99_min + DLS + GovernmentResponseIndex + TauxPopMovement + Movement + HI + WD"
rf <- ranger(equation, data=Data_train, importance =  'permutation')

# figure: importance plot
imp <- rf$variable.importance
imp <- sort(imp)
o <- order(imp, decreasing=T)
nom <- names(imp)
plot(c(1:length(imp)), imp[o], type='h', ylim = c(0, max(imp) + max(imp)/5), xlab='', ylab='Importance (permutation)')
K <- length(imp)
text(tail(c(1:length(imp)), K), tail(imp[o]+max(imp/8), K), labels= tail(nom[o], K), pos=3, srt=90, adj=1, cex=0.6)
points(c(1:length(imp)), imp[o], pch=20)


### ANOVA: vérification de l'importance de Movement
formula1 <- "Load ~ Temp + WeekDays + WD + BH+ toy + Summer_break + DLS + Christmas_break"
formula2 <- "Load ~ Temp + WeekDays + WD + BH+ toy + Summer_break + DLS + Christmas_break + Movement"
small_lm <- lm(formula1%>%as.formula, data=Data_train)
large_lm <- lm(formula2%>%as.formula, data=Data_train)
anova.res <- anova(small_lm, large_lm)
summary(anova.res)

### Modèle linéaire simple
formula <- "Load ~Load.1 + WeekDays + Load.7 + Temp + Temp_s99_max + Temp_s99_min + WeekDays + WD + BH+ toy + Summer_break + DLS + Christmas_break + HI "
slm <- lm(formula%>%as.formula, data=Data_train)
pred1 = predict(slm, newdata=Data_test)

### Modèle linéaire polynomial 
formula <- "Load ~Load.1 + WeekDays + Load.7 + I(Load.7^2) + Temp + I(Temp^2) + Temp_s99_max + I(Temp_s99_max^2) + Temp_s99_min + WeekDays + WD + BH+ toy + Summer_break + DLS + Christmas_break + HI "
slm <- lm(formula%>%as.formula, data=Data_train)
pred = predict(slm, newdata=Data_test)

plot(Data_test$Date,pred1, type='l', col='red', main='Modèles linéaires', xlab = 'Date') #lm 
lines(Data_test$Date, pred, type='l', col='blue') #lm polynomial version

### Random forest
formule <- "Load ~ Month + Temp_s95_min + Temp_s95_max + HI + TauxPopMovement +Time + toy + Temp + Load.1 + Load.7 + WD + BH + Temp_s99_min + Temp_s99_max + Summer_break  + Christmas_break  + DLS"

rf<- ranger::ranger(formule, data = Data_train, importance =  'permutation')
rf.forecast <- predict(rf, data = Data_test)$predictions

plot(Data_test$Date,rf.forecast, type='l', col='red', main='forêt aléatoire', xlab = 'Date')

### GAM
equation <- "Load ~ Load.1:as.factor(WeekDays) + HI + BH + Christmas_break + Summer_break + DLS + s(Temp) + s(Temp_s99_max, Temp_s99_min)+ s(Load.7) + s(Time, k=7) + s(toy, k =30, bs = 'cc', by=as.factor(WD))+ s(Temp, Time, k=20)"

#gam<-gam(equation%>%as.formula, data=Data_train)
#saveRDS(gam, "Results/full_set_models/gam.RDS")
gam = readRDS("Results/full_set_models/gam.RDS")
gam.forecast <- predict(gam, newdata=Data_test)

#Optimisation de la fonction GAM
gam.check(gam)

plot(Data_test$Date,gam.forecast, type='l', col='red', main='gam', xlab = 'Date')

### Régréssion quantile sur GAM: qgam 
#gam9<-qgam(equation%>%as.formula, data=Data_train, qu=0.4)
#saveRDS(gam9, "Results/full_set_models/qgam.RDS")
gam9 = readRDS("Results/full_set_models/qgam.RDS")
gam9.forecast <- predict(gam9, newdata=Data_test)

plot(Data_test$Date,gam9.forecast, type='l', col='red', main='qgam', xlab = 'Date')

### Arima
arima.fit <- forecast::Arima(gam9.forecast, order = c(1,1,2), seasonal = c(0,0,2))
arima.predict <- fitted(arima.fit)

plot(Data_test$Date, arima.predict, col="red", type='l', main='arima', xlab = 'Date')

### Apprentissage en ligne
# Calcul de la version statique  
Data_test = add_column(Data_test, Load=lead(Data_test$Load.1, default=mean(Data_test$Load.1)), .after = "Date")
Data = dplyr::bind_rows(Data_train, Data_test[-21]) #on fait une concatenation 

X <- predict(gam9, newdata=Data, type='terms')
y = Data$Load

###standarisation des colonnes 
for (j in 1:ncol(X)){
  X[,j] <- (X[,j]-mean(X[,j])) / sd(X[,j])
}
X <- cbind(X,1)
d <- ncol(X)

# Version dynamique
ssm <- viking::statespace(X, y)
gam9.kalman.static <- ssm$pred_mean%>%tail(nrow(Data_test))
#ssm_dyn <- viking::select_Kalman_variances(ssm, X[1:3051,], y[1:3051], q_list = 2^(-30:0), p1 = 1, ncores = 6)
#saveRDS(ssm_dyn, "Results/full_set_models/kalman_dyn.RDS")
ssm_dyn = readRDS("Results/full_set_models/kalman_dyn.RDS")
ssm_dyn <- predict(ssm_dyn, X, y, type='model', compute_smooth = TRUE)
gam9.kalman.Dyn <- ssm_dyn$pred_mean%>%tail(nrow(Data_test))

plot(Data_test$Date, gam9.kalman.Dyn, type='l', col='red', main='kalman filter', xlab = 'Date')

### Mise en place d'une pipeline
# on divise en 10 blocks
Nblock<-10
borne_block<-seq(1, nrow(Data_train), length=Nblock+1)%>%floor
block_list<-list()
l<-length(borne_block)
for(i in c(2:(l-1)))
{
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]]<-c(borne_block[l-1]:(borne_block[l]))

# calcul du rmse sur les blocks
blockRMSE<-function(equation, block)
{
  g<- gam(as.formula(equation), data=Data_train[-block,])
  forecast<-predict(g, newdata=Data_train[block,])
  return(forecast)
} 

# predictions sur les blocks
#Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
#saveRDS(Block_forecast, "Results/full_set_models/block_forecast.RDS")
Block_forecast = readRDS("Results/full_set_models/block_forecast.RDS")

# calcul des residus
Block_residuals <- Data_train$Load-Block_forecast

# estimation des effets GAM 
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

Data1_rf <- data.frame(Data_test, terms1)
residuals <- Data1_rf$Load - gam9.forecast
Data1_rf$residuals <- residuals
Data1_rf$res.48 <- c(residuals[1], residuals[1:(length(residuals)-1)])
Data1_rf$res.336 <- c(residuals[1:7], residuals[1:(length(residuals)-7)])

# calcul d'une forêt aléatoire sur les résidus 
cov <- "Time + toy + Temp + Load.1 + Load.7 + Temp_s99 + WeekDays + BH + Temp_s95_max + Temp_s99_max + Summer_break  + Christmas_break + 
Temp_s95_min +Temp_s99_min + DLS + GovernmentResponseIndex + res.48 + res.336 +"
gterm <-paste0("gterms_", c(1:ncol(terms0)))
gterm <- paste0(gterm, collapse='+')
cov <- paste0(cov, gterm, collapse = '+')
formule_rf <- paste0("residuals", "~", cov)
rf_gam<- ranger::ranger(formule_rf, data = Data0_rf, importance =  'permutation')
rf_gam.forecast <- predict(rf_gam, data = Data1_rf)$predictions+ g.forecast

rf_gam$variable.importance%>%sort

Block_residuals.ts <- ts(Block_residuals, frequency=7)
#fit.arima.res <- auto.arima(Block_residuals.ts,max.p=3,max.q=4, max.P=2, max.Q=2, trace=T,ic="aic", method="CSS")
#saveRDS(fit.arima.res, "Results/full_set_models/tif.arima.res.RDS")
fit.arima.res = readRDS("Results/full_set_models/tif.arima.res.RDS")
#Best model: ARIMA(1,0,1)(1,0,0)[7] with zero mean 
ts_res_forecast <- ts(c(Block_residuals.ts, Data_test$Load-gam9.forecast),  frequency= 7)
refit <- Arima(ts_res_forecast, model=fit.arima.res)
prevARIMA.res <- tail(refit$fitted, nrow(Data_test))
gam9.arima.forecast <- gam9.forecast + prevARIMA.res

plot(as.numeric(gam9.arima.forecast), type='l', col='red', main='Pipeline', xlab = 'Date')

### Aggregation d'experts
experts <- cbind(gam9.forecast, pred,rf.forecast, gam.forecast, arima.predict, gam9.kalman.Dyn, as.numeric(gam9.arima.forecast))%>%as.matrix
nom_exp <- c("qgam", "polynomial_lm", "rf", "gam", "arima","Kalman_dyn", "pipeline")
colnames(experts) <-  nom_exp

rmse_exp <- apply(experts, 2, rmse, y=Data_test$Load)
sort(rmse_exp)
cumsum_exp <- apply(Data_test$Load-experts, 2, cumsum)

par(mfrow=c(1,1))
K <-ncol(experts)
col <- rev(RColorBrewer::brewer.pal(n = max(min(K,11),4),name = "Spectral"))[1:min(K,11)]
matplot(cumsum_exp, type='l', col=col, lty=1, lwd=2)
par(new=T)
plot(Data1$GovernmentResponseIndex, lwd=2, type='l', axes=F)
legend("topleft", col=col, legend=colnames(experts), lty=1, bty='n')
par(new=T)

##Correction bias
expertsM2000 <- experts-2000
expertsP2000 <- experts+2000
experts <- cbind(experts, expertsM2000, expertsP2000)
colnames(experts) <-c(nom_exp, paste0(nom_exp,  "M"), paste0(nom_exp,  "P"))
cumsum_exp <- apply(Data_test$Load-experts, 2, cumsum)


agg<- mixture(Y = Data_test$Load, experts = experts, loss.gradient=TRUE)
summary(agg)

plot(agg)

# Voir l'allure de l'oracle
or <- oracle(Y=Data_test$Load, experts);

par(mfrow=c(1,1))
plot(Data_test$Date, or$prediction, type='l', col='red', main='Expert aggregation', xlab = 'Date')
lines(Data_test$Date, agg$prediction, type='l', col='orange')

### Creation d'une figure finale: 
models = c(pred, rf.forecast, gam9.forecast, gam.forecast, gam9.kalman.Dyn, gam9.arima.forecast, agg$prediction)
K <-ncol(models)
col <- rev(RColorBrewer::brewer.pal(n = max(min(K,7),6),name = "Spectral"))[1:min(K,7)]
nom_mod <- c("polynomial_lm", "Random_forest", "qgam", "gam", "kalman","pipeline", "agg_exp")

# Modèle linéaire polynomial
plot(Data_test$Date,pred, type='l', ylab = "polynomial_lm", col='blue', main='Comparaison des modèles', xlab = 'Date')

# Forêt aléatoire
lines(Data_test$Date,rf.forecast, type='l',  ylab='rf',col= "darkgreen")

# gam
lines(Data_test$Date,gam9.forecast, type='l', ylab='qgam',col="lightgreen")

# qgam
lines(Data_test$Date,gam.forecast, type='l', ylab='qgam',col="yellow")

# Pipeline
lines(Data_test$Date,gam9.arima.forecast, type='l', ylab='qgam',col="orange")

# Kalman 
lines(Data_test$Date,gam9.kalman.Dyn, type='l', ylab='qgam',col="red")

# Aggregation d'experts
lines(Data_test$Date, agg$prediction, type='l', ylab='agg_exp', col='darkred')

legend("topleft", col=col, legend=nom_mod, lty=1, bty='n')
