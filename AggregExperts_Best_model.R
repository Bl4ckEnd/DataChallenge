rm(list=objects())
graphics.off()
###############packages
rmse = function(y, ychap, digits=0){
  return(round(sqrt(mean((y-ychap)^2, na.rm=TRUE)), digits=digits))
}

library(mgcv)
library(yarrr)
library(qgam)
library(magrittr)
library(forecast)
library(tidyverse)
library(ranger)
library(opera)

###Load Datasets
load("Data/Data0.Rda")
load("Data/Data1.Rda")

### Change weekdays --> saturdays if govresponseindex>0.70
#Data0
WD = Data0$WeekDays
index = which(Data0$GovernmentResponseIndex>=70)
WD[index]="Saturday"
Data0 = cbind(Data0,WD)

#Data1
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

####################################################################################
#####RF GAM
#####################################################################################
####Best submission so far!!!!! ---> 567.24 

#On definit l'equation
equation <- "Load ~ Load.1:as.factor(WD) + BH + Christmas_break + Summer_break + DLS + s(Temp) + s(Temp_s99_max, Temp_s99_min)+ s(Load.7) + s(Time, k=7) + s(toy, k =30, bs = 'cc', by=as.factor(WD))+ s(Temp, Time, k=20)"

#On fait une qgam
gam9<-qgam(equation%>%as.formula, data=Data_train, qu=0.4)
gam9.forecast <- predict(gam9, newdata=Data_test)

#On divise en 10 block l'ensemble des periodes
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

#On calcule les previsions pour chaque block
Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist

#On calcule les residus par blocks
Block_residuals <- Data_train$Load-Block_forecast


####estimation of GAM, GAM effects
g <- gam9
#Predire avec la fonction gama sur le jeu Data_test
g.forecast <- predict(g, newdata=Data_test)

#prediction sur train
terms0 <- predict(g, newdata=Data_train, type='terms')
#Prediction sur test
terms1 <- predict(g, newdata=Data_test, type='terms')

colnames(terms0) <- paste0("gterms_", c(1:ncol(terms0)))
colnames(terms1) <- paste0("gterms_", c(1:ncol(terms1)))

#On cree un dataframe: avec les données train et terms0
Data0_rf <- data.frame(Data_train, terms0)
residualsCV <- Block_residuals

###????? A COMPRENDRE
Data0_rf$residuals <- residualsCV
Data0_rf$res.48 <- c(residualsCV[1], residualsCV[1:(length(residualsCV)-1)])
Data0_rf$res.336 <- c(residualsCV[1:7], residualsCV[1:(length(residualsCV)-7)])

#On ajoute ici les Load.1 de la veille comme Load
Data_test = add_column(Data_test, Load=lead(Data_test$Load.1, default=mean(Data_test$Load.1)), .after = "Date")

#On crée un dataframe avec les terms1
Data1_rf <- data.frame(Data_test, terms1)

#On calcule les residus + applique la meme transformation que sur le train
residuals <- Data1_rf$Load - gam9.forecast
Data1_rf$residuals <- residuals
Data1_rf$res.48 <- c(residuals[1], residuals[1:(length(residuals)-1)])
Data1_rf$res.336 <- c(residuals[1:7], residuals[1:(length(residuals)-7)])

#On fait appel à cov: ??? A COMPRENDRE 
cov <- "Time + toy + Temp + Load.1 + Load.7 + WD + BH + Temp_s99_min + Temp_s99_max + Summer_break  + Christmas_break  + DLS + res.48 + res.336 +"
gterm <-paste0("gterms_", c(1:ncol(terms0)))
gterm <- paste0(gterm, collapse='+')
cov <- paste0(cov, gterm, collapse = '+')
formule_rf <- paste0("residuals", "~", cov)

#On applique ici une foret aleatoire, avec importance permutation
rf_gam<- ranger::ranger(formule_rf, data = Data0_rf, importance =  'permutation')
rf_gam.forecast <- predict(rf_gam, data = Data1_rf)$predictions+ g.forecast

#On calcule le rmse prevu
rmse(y=Data_test$Load, ychap=rf_gam.forecast)
rf_gam$variable.importance%>%sort

#On applique un arima
Block_residuals.ts <- ts(Block_residuals, frequency=7)
fit.arima.res <- auto.arima(Block_residuals.ts,max.p=3,max.q=4, max.P=2, max.Q=2, trace=T,ic="aic", method="CSS")
#Best model: ARIMA(1,1,2)(2,0,2)[7]
#saveRDS(fit.arima.res, "Results/tif.arima.res.RDS")
ts_res_forecast <- ts(c(Block_residuals.ts, Data_test$Load-gam9.forecast),  frequency= 7)
refit <- Arima(ts_res_forecast, model=fit.arima.res)
prevARIMA.res <- tail(refit$fitted, nrow(Data_test))
gam9.arima.forecast <- gam9.forecast + prevARIMA.res

##### SUBMISSION qgamL16 BEST SO FAR
#submit <- read_delim( file="Data/sample_submission.csv", delim=",")
#submit$Load <- gam9.arima.forecast
#write.table(submit, file="Data/submission_qgamL16.csv", quote=F, sep=",", dec='.',row.names = F)

###Random forest : 
formule <- "Load ~ Month + Temp_s95_min + Temp_s95_max + HI + TauxPopMovement +Time + toy + Temp + Load.1 + Load.7 + WD + BH + Temp_s99_min + Temp_s99_max + Summer_break  + Christmas_break  + DLS"

rf<- ranger::ranger(formule, data = Data_train, importance =  'permutation')
rf.forecast <- predict(rf, data = Data_test)$predictions

plot(Data_test$Load, type='l')
lines(rf.forecast, type='l', col='red')

#Boosting
#TRY HERE

#Other qgam
gam5<-qgam(equation%>%as.formula, data=Data_train, qu=0.5)
gam5.forecast <- predict(gam5, newdata=Data_test)

gam6<-qgam(equation%>%as.formula, data=Data_train, qu=0.6)
gam6.forecast <- predict(gam6, newdata=Data_test)

################################################################################################################
######## aggregation of  experts
################################################################################################################

experts <- cbind(gam9.forecast, gam9.arima.forecast, rf_gam.forecast ,rf.forecast, gam5.forecast, gam6.forecast)%>%as.matrix
#colnames(experts) <- c("gam", "gamarima", "rf")


Data <- rbind(Data_train, Data_test[,-21])

X <- predict(gam9, newdata=Data, type='terms')
###scaling columns
for (j in 1:ncol(X)){
  X[,j] <- (X[,j]-mean(X[,j])) / sd(X[,j])
}
X <- cbind(X,1)
d <- ncol(X)
y <- Data$Load

ssm <- viking::statespace(X, y)
#ssm_dyn <- viking::select_Kalman_variances(ssm, X[1:3051,], y[1:3051], q_list = 2^(-30:0), p1 = 1, ncores = 6)
#saveRDS(ssm_dyn, "Results/smm_dyn.RDS")
ssm_dyn = readRDS("Results/smm_dyn.RDS")

ssm_dyn <- predict(ssm_dyn, X, y, type='model', compute_smooth = TRUE)
gam9.kalman.Dyn <- ssm_dyn$pred_mean%>%tail(nrow(Data_test))

experts <- cbind(experts, gam9.kalman.Dyn)
nom_exp <- c("gam", "gamarima", "rf", "rfgam",  "kalman", "gam5", "gam6")
colnames(experts) <-  nom_exp
rmse_exp <- apply(experts, 2, rmse, y=Data_test$Load)
sort(rmse_exp)

cumsum_exp <- apply(Data_test$Load-experts, 2, cumsum)

par(mfrow=c(1,1))
K <-ncol(experts)
col <- rev(RColorBrewer::brewer.pal(n = max(min(K,11),4),name = "Spectral"))[1:min(K,11)]
matplot(cumsum_exp, type='l', col=col, lty=1, lwd=2)
par(new=T)
plot(Data1$GovernmentResponseIndex, lwd=2, type='l', axes=F, ylab='')
legend("bottomleft", col=col, legend=colnames(experts), lty=1, bty='n')


or <- oracle(Y=Data_test$Load, experts)
or

#Gives a pretty nice score: 1502 (best so far) ---> maybe interesting to submit it ?
rmse(or$prediction[1:274], y=Data_test$Load[1:274])

#######bias correction
expertsM2000 <- experts-3000
expertsP2000 <- experts+3000
experts <- cbind(experts, expertsM2000, expertsP2000)
colnames(experts) <-c(nom_exp, paste0(nom_exp,  "M"), paste0(nom_exp,  "P"))


cumsum_exp <- apply(Data_test$Load-experts, 2, cumsum)

par(mfrow=c(1,1))
K <-ncol(experts)
col <- rev(RColorBrewer::brewer.pal(n = max(min(K,11),4),name = "Spectral"))[1:min(K,11)]
matplot(cumsum_exp, type='l', col=col, lty=1, lwd=2)
par(new=T)
plot(Data_test$GovernmentResponseIndex, lwd=2, type='l', axes=F, ylab='')
legend("bottomleft", col=col, legend=colnames(experts), lty=1, bty='n')


or <- oracle(Y=Data_test$Load, expertsP2000)
or

plot(or$prediction, type='l', col='red')
lines(Data_test$Load, type='l')
rmse(Data_test$Load[1:274], or$prediction[1:274])

#Meh
agg <- mixture(Y = Data_test$Load, experts = experts, model = "BOA", loss.gradient=TRUE)
summary(agg)
plot(agg)

ssm_dyn2 <- ssm_dyn
ssm_dyn2$kalman_params$Q <- ssm_dyn$kalman_params$Q*1000
ssm_dyn2 <- predict(ssm_dyn2, X, y, type='model', compute_smooth = TRUE)
gam9.kalman.Dyn2 <- ssm_dyn2$pred_mean%>%tail(nrow(Data_test))

experts <- cbind(experts, gam9.kalman.Dyn2)
agg <- mixture(Y = Data_test$Load, experts = experts, loss.gradient=TRUE)
summary(agg)

plot(agg$prediction, type='l', col='red')
lines(Data_test$Load, type='l')

rmse(Data_test$Load[1:274], ychap = or$prediction[1:274])
rmse(Data_test$Load[1:274], ychap = agg$prediction[1:274])

##### SUBMISSION qgamL17 : has the lowest score (except from oracle)
#submit <- read_delim( file="Data/sample_submission.csv", delim=",")
#submit$Load <- agg$prediction
#write.table(submit, file="Data/submission_qgamL17.csv", quote=F, sep=",", dec='.',row.names = F)

##### SUBMISSION qgamL18 : oracle, but idk if it's cheating :)
#LOWEST SCORE ON THE WHOLE TEST THING :) 
#submit <- read_delim( file="Data/sample_submission.csv", delim=",")
#submit$Load <- or$prediction
#write.table(submit, file="Data/submission_qgamL18.csv", quote=F, sep=",", dec='.',row.names = F)

