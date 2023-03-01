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

##### Choix des variables avec rf
equation <- "Load~  Time + toy + Temp + Load.1 + Load.7 + Temp_s99 + WeekDays + BH + Temp_s95_max + 
  Temp_s99_max + Summer_break  + Christmas_break + 
  Temp_s95_min +Temp_s99_min + DLS + GovernmentResponseIndex + TauxPopMovement + Movement + HI + WD"
rf <- ranger(equation, data=Data_train, importance =  'permutation')

#############importance plot
imp <- rf$variable.importance
imp <- sort(imp)
o <- order(imp, decreasing=T)
nom <- names(imp)
plot(c(1:length(imp)), imp[o], type='h', ylim = c(0, max(imp) + max(imp)/5), xlab='', ylab='Importance (permutation)')
K <- length(imp)
text(tail(c(1:length(imp)), K), tail(imp[o]+max(imp/8), K), labels= tail(nom[o], K), pos=3, srt=90, adj=1, cex=0.6)
points(c(1:length(imp)), imp[o], pch=20)


#### ANOVA
formula1 <- "Load ~ Temp + WeekDays + WD + BH+ toy + Summer_break + DLS + Christmas_break"
formula2 <- "Load ~ Temp + WeekDays + WD + BH+ toy + Summer_break + DLS + Christmas_break + Movement"
small_lm <- lm(formula1%>%as.formula, data=Data_train)
large_lm <- lm(formula2%>%as.formula, data=Data_train)
anova.res <- anova(small_lm, large_lm)
summary(anova.res)


######## lm
formula <- "Load ~Load.1 + WeekDays + Load.7 + Temp + Temp_s99_max + Temp_s99_min + WeekDays + WD + BH+ toy + Summer_break + DLS + Christmas_break + HI "
slm <- lm(formula%>%as.formula, data=Data_train)
pred1 = predict(slm, newdata=Data_test)

rmse(Data_test$Load, pred1)

#######Polynomial lm
formula <- "Load ~Load.1 + WeekDays + Load.7 + I(Load.7^2) + Temp + I(Temp^2) + Temp_s99_max + I(Temp_s99_max^2) + Temp_s99_min + WeekDays + WD + BH+ toy + Summer_break + DLS + Christmas_break + HI "
slm <- lm(formula%>%as.formula, data=Data_train)
pred = predict(slm, newdata=Data_test) #2153

plot(Data_test$Date,pred1, type='l', col='red', main='Lm') #lm 
lines(Data_test$Date, pred, type='l', col='blue') #lm polynomial version
rmse(Data_test$Load, pred) #2000


#####Random forest
formule <- "Load ~ Month + Temp_s95_min + Temp_s95_max + HI + TauxPopMovement +Time + toy + Temp + Load.1 + Load.7 + WD + BH + Temp_s99_min + Temp_s99_max + Summer_break  + Christmas_break  + DLS"

rf<- ranger::ranger(formule, data = Data_train, importance =  'permutation')
rf.forecast <- predict(rf, data = Data_test)$predictions

plot(Data_test$Date,rf.forecast, type='l', col='red', main='rf')
rmse(Data_test$Load, rf.forecast)

####gam
equation <- "Load ~ Load.1:as.factor(WeekDays) + HI + BH + Christmas_break + Summer_break + DLS + s(Temp) + s(Temp_s99_max, Temp_s99_min)+ s(Load.7) + s(Time, k=7) + s(toy, k =30, bs = 'cc', by=as.factor(WD))+ s(Temp, Time, k=20)"

gam<-gam(equation%>%as.formula, data=Data_train)
gam.forecast <- predict(gam, newdata=Data_test)
gam.check(gam)

plot(Data_test$Date,gam.forecast, type='l', col='red', main='gam')
rmse(Data_test$Load, gam.forecast)

####qgam 
gam9<-qgam(equation%>%as.formula, data=Data_train, qu=0.4)
gam9.forecast <- predict(gam9, newdata=Data_test)

plot(Data_test$Date,gam9.forecast, type='l', col='red', main='qgam')
rmse(Data_test$Load, gam9.forecast)


### ARIMA Model
arima.fit <- forecast::Arima(gam9.forecast, order = c(1,1,2), seasonal = c(0,0,2))
arima.predict <- fitted(arima.fit)

plot(Data_test$Date, arima.predict, col="red", main='arima')
rmse(arima.predict, Data_test$Load)


######online learning
# static 
#On doit le faire ici sur tout le jeu train car sinon on n'a pas d'effectifs dans chaque sous modalité
equation <- "Load ~ Load.1:as.factor(WeekDays) + HI + BH + Christmas_break + Summer_break + DLS + s(Temp) + s(Temp_s99_max, Temp_s99_min)+ s(Load.7) + s(Time, k=7) + s(toy, k =30, bs = 'cc', by=as.factor(WD))+ s(Temp, Time, k=20)"

Data = dplyr::bind_rows(Data_train, Data_test[-21])

X <- predict(gam9, newdata=Data, type='terms')
y = Data$Load
###scaling columns
for (j in 1:ncol(X)){
  X[,j] <- (X[,j]-mean(X[,j])) / sd(X[,j])
}
X <- cbind(X,1)
d <- ncol(X)


#dynamic
ssm <- viking::statespace(X, y)
gam9.kalman.static <- ssm$pred_mean%>%tail(nrow(Data_test))
ssm_dyn <- viking::select_Kalman_variances(ssm, X[1:3051,], y[1:3051], q_list = 2^(-30:0), p1 = 1, ncores = 6)

ssm_dyn <- predict(ssm_dyn, X, y, type='model', compute_smooth = TRUE)
gam9.kalman.Dyn <- ssm_dyn$pred_mean%>%tail(nrow(Data_test))

plot(Data_test$Date, gam9.kalman.Dyn, type='l', col='red', main='kalman filter')
rmse(Data_test$Load, gam9.kalman.Dyn) #1670

####### Pipeline
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
  g<- gam(as.formula(equation), data=Data_train[-block,])
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
#saveRDS(fit.arima.res, "../Results/tif.arima.res.RDS")
ts_res_forecast <- ts(c(Block_residuals.ts, Data_test$Load-gam9.forecast),  frequency= 7)
refit <- Arima(ts_res_forecast, model=fit.arima.res)
prevARIMA.res <- tail(refit$fitted, nrow(Data_test))
gam9.arima.forecast <- gam9.forecast + prevARIMA.res

plot(as.numeric(gam9.arima.forecast), type='l', col='red', main='Pipeline')

#Mini aggregation d'experts 
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
plot(Data1$GovernmentResponseIndex, lwd=2, type='l', axes=F, ylab='GRI')
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
# see why it does not work 
plot(agg)

or <- oracle(Y=Data_test$Load, experts);
par(mfrow=c(1,1))
plot(Data_test$Date, or$prediction, type='l', col='red', main='Expert aggregation')
lines(Data_test$Date, agg$prediction, type='l', col='orange')
rmse(agg$prediction[1:274], y=Data_test$Load[1:274]) #813

##### SUBMISSION qgamL19 : agg: even though is 720 --> overall score is much better 
models = c(pred, rf.forecast, gam.forecast, gam9.forecast, gam9.kalman.Dyn, gam9.arima.forecast, agg$prediction)
K <-ncol(models)
col <- rev(RColorBrewer::brewer.pal(n = max(min(K,6),4),name = "Spectral"))[1:min(K,6)]
nom_mod <- c("polynomial_lm", "Random_forest", "gam", "qgam", "kalman","pipeline", "agg_exp")

#Polynomial lm
plot(Data_test$Date,pred, type='l', ylab = "polynomial_lm", col='lightgreen', main='Model comparison')
#Random forest
lines(Data_test$Date,rf.forecast, type='l',  ylab='rf',col= "darkgreen")
#gam
lines(Data_test$Date,gam.forecast, type='l', ylab='qgam',col="yellow")
#qgam
lines(Data_test$Date,gam9.forecast, type='l', ylab='qgam',col="orange")
#pipeline
lines(Data_test$Date,gam9.arima.forecast, type='l', ylab='qgam',col="darkorange")
#kalman 
lines(Data_test$Date,gam9.kalman.Dyn, type='l', ylab='qgam',col="red")
#Aggregation d'experts
lines(Data_test$Date, agg$prediction, type='l', ylab='agg_exp', col='darkred')
legend("topleft", col=col, legend=nom_mod, lty=1, bty='n')

