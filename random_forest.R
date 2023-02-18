########################################################
########join with electricity data
########################################################
rm(list=objects())
###############packages
rmse = function(y, ychap, digits=0){
  return(round(sqrt(mean((y-ychap)^2, na.rm=TRUE)), digits=digits))
}

library(mgcv)
library(yarrr)
library(magrittr)
library(forecast)
library(qgam)
library(tidyverse)
library(ranger)
library(opera)
source('R/score.R')


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


############################################################################################################
####################################ranger
############################################################################################################


names(Data)

equation <- Load~  Time + toy + Temp + Load.1 + Load.7 + Temp_s99 + WeekDays + BH + Temp_s95_max + 
  Temp_s99_max + Summer_break  + Christmas_break + 
  Temp_s95_min +Temp_s99_min + DLS + GovernmentResponseIndex
rf <- ranger(equation, data=Data0, importance =  'permutation')
rf$r.squared
rf$mtry

#out of bag prediction error
rf$prediction.error%>%sqrt

#test error
rf.forecast <- predict(rf, data=Data1)$prediction
rmse(y=Data1$Load, ychap=rf.forecast)

#Mtry influence
num.trees <- 10
mtry <- c(1:13)
R2 <- array(0, dim=13)
forecast.error <- array(0, dim=13)
oob.error <- array(0, dim=13)
for(i  in mtry)
{
  rf <- ranger(equation, data=Data0, importance =  'permutation', mtry=i, num.trees=num.trees)
  R2[i] <- rf$r.squared
  oob.error[i] <- rf$prediction.error%>%sqrt
  rf.forecast <- predict(rf, data=Data1)$prediction
  forecast.error[i] <- rmse(y=Data1$Load, ychap=rf.forecast)
  print(i)
}


#100: 2399
#500: 2376
#1000: 2380
min(oob.error)
min(forecast.error)


plot(oob.error, type='b', pch=20, ylim=range(oob.error, forecast.error))
lines(forecast.error, type='b', pch=22, col='red')
par(new=T)
plot(R2, type='b', col='blue', axes=FALSE, ylab='')


#############importance plot
imp <- rf$variable.importance
sort(imp)
o <- order(imp, decreasing=T)
nom <- names(imp)
plot(c(1:length(imp)), imp[o], type='h', ylim = c(0, max(imp) + max(imp)/5), xlab='', ylab='Importance (permutation)')
K <- length(imp)
text(tail(c(1:length(imp)), K), tail(imp[o]+max(imp/8), K), labels= tail(nom[o], K), pos=3, srt=90, adj=1)
points(c(1:length(imp)), imp[o], pch=20)

#######################################
#############online learning
#######################################
Ntree <- 500
mtry <- 13
equation <- Load~  Time + toy + Temp + Load.1 + Load.7 + Temp_s99 + WeekDays + BH + Temp_s95_max + Temp_s99_max + Summer_break  + Christmas_break + 
  Temp_s95_min +Temp_s99_min + DLS + GovernmentResponseIndex
rf<- ranger::ranger(equation, data=Data0, num.trees = Ntree, mtry=mtry)
rf.forecast <-  predict(rf, data=Data1)$predictions
rf.forecast.all <-  predict(rf, data=Data1, predict.all = TRUE)$predictions
dim(rf.forecast.all)

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


rmse(y=Data1$Load, ychap=rf.forecast)
rmse(y=Data1$Load, ychap=rf.forecast_update)

plot(Data1$Date, Data1$Load, type='l')
lines(Data1$Date,rf.forecast, type='l', col='red')
lines(Data1$Date,rf.forecast_update, type='l', col='blue')

#saveRDS(object = rf.forecast_update, "Results/rf.forecast_update.RDS")
rf.forecast_update  <- readRDS("Results/rf.forecast_update.RDS")


rf<- ranger::ranger(equation, data=rbind(Data0, Data1), num.trees = Ntree, mtry=mtry, importance =  'permutation')



rf0<- ranger::ranger(equation, data=Data0, num.trees = Ntree, mtry=mtry, importance =  'permutation')
rf1<- ranger::ranger(equation, data=rbind(Data0, Data1), num.trees = Ntree, mtry=mtry, importance =  'permutation')

#############importance plot
par(mfrow=c(2,1))
imp <- rf0$variable.importance
sort(imp)
o <- order(imp, decreasing=T)
nom <- names(imp)
plot(c(1:length(imp)), imp[o], type='h', ylim = c(0, max(imp) + max(imp)/5), xlab='', ylab='Importance (permutation)')
K <- length(imp)
text(tail(c(1:length(imp)), K), tail(imp[o]+max(imp/8), K), labels= tail(nom[o], K), pos=3, srt=90, adj=1)
points(c(1:length(imp)), imp[o], pch=20)

imp <- rf1$variable.importance
sort(imp)
o <- order(imp, decreasing=T)
nom <- names(imp)
plot(c(1:length(imp)), imp[o], type='h', ylim = c(0, max(imp) + max(imp)/5), xlab='', ylab='Importance (permutation)')
K <- length(imp)
text(tail(c(1:length(imp)), K), tail(imp[o]+max(imp/8), K), labels= tail(nom[o], K), pos=3, srt=90, adj=1)
points(c(1:length(imp)), imp[o], pch=20)

rf0$variable.importance%>%sort
rf1$variable.importance%>%sort


#####extra trees
equation <- Load~  Time + toy + Temp + Load.1 + Load.7 + Temp_s99 + WeekDays + BH + Temp_s95_max + Temp_s99_max + Summer_break  + Christmas_break + 
  Temp_s95_min +Temp_s99_min + DLS + GovernmentResponseIndex
rf.random<- ranger::ranger(equation, data=Data0, 
                    num.trees = 10, importance =  'permutation',
                    splitrule = "extratrees", mtry=1, num.random.splits=1)

rf.random$r.squared
rf.random$prediction.error%>%sqrt

rf.forecast.all <-  predict(rf.random, data=Data1, predict.all = TRUE)$predictions
dim(rf.forecast.all)

plot(Data1$Date, Data1$Load, type='l')
for(i in c(1:500))
{
  lines(Data1$Date, rf.forecast.all[,i], type='l', col='lightblue')
}
lines(Data1$Date,rf.forecast, col='blue')














####################################################################################
#####RF GAM
#####################################################################################
equation <- "Load ~ Load.1:as.factor(WD) +WeekDays  + s(Temp_s99_max, Temp_s95_max)+s(Temp_s99_min, Temp_s95_min) + s(Load.7) + s(Time, k=7) + s(toy, k =30, bs = 'cc', by=as.factor(WD))"
gam9<-qgam(equation%>%as.formula, data=Data0, qu=0.4)
gam9.forecast <- predict(gam9, newdata=Data1)

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


imp <- rf_gam$variable.importance
sort(imp)
o <- order(imp, decreasing=T)
nom <- names(imp)
plot(c(1:length(imp)), imp[o], type='h', ylim = c(0, max(imp) + max(imp)/5), xlab='', ylab='Importance (permutation)')
K <- length(imp)
text(tail(c(1:length(imp)), K), tail(imp[o]+max(imp/8), K), labels= tail(nom[o], K), pos=3, srt=90, adj=1)
points(c(1:length(imp)), imp[o], pch=20)

rf_gam$variable.importance%>%sort


Block_residuals.ts <- ts(Block_residuals, frequency=7)
fit.arima.res <- auto.arima(Block_residuals.ts,max.p=3,max.q=4, max.P=2, max.Q=2, trace=T,ic="aic", method="CSS")
#best model: ARIMA(1,0,2)(2,0,1)[7] with zero mean
#saveRDS(fit.arima.res, "Results/tif.arima.res.RDS")
ts_res_forecast <- ts(c(Block_residuals.ts, Data1$Load-gam9.forecast),  frequency= 7)
refit <- Arima(ts_res_forecast, model=fit.arima.res)
prevARIMA.res <- tail(refit$fitted, nrow(Data1))
gam9.arima.forecast <- gam9.forecast + prevARIMA.res


################################################################################################################
######## aggregation of  experts
################################################################################################################

experts <- cbind(gam9.forecast, gam9.arima.forecast, rf.forecast, rf_gam.forecast)%>%as.matrix
#colnames(experts) <- c("gam", "gamarima", "rf", "rfgam")

Data <- read_delim("Data/train.csv", delim=",")
ssm_dyn <- readRDS("Results/ssm_dyn2.RDS")

X <- predict(gam9, newdata=Data, type='terms')
###scaling columns
for (j in 1:ncol(X))
  X[,j] <- (X[,j]-mean(X[,j])) / sd(X[,j])
X <- cbind(X,1)
d <- ncol(X)
y <- Data$Load

ssm_dyn <- predict(ssm_dyn, X, y, type='model', compute_smooth = TRUE)
gam9.kalman.Dyn <- ssm_dyn$pred_mean%>%tail(nrow(Data1))
experts <- cbind(experts, gam9.kalman.Dyn)
nom_exp <- c("gam", "gamarima", "rf", "rfgam",  "kalman")
colnames(experts) <-  nom_exp
rmse_exp <- apply(experts, 2, rmse, y=Data1$Load)
sort(rmse_exp)

cumsum_exp <- apply(Data1$Load-experts, 2, cumsum)

par(mfrow=c(1,1))
K <-ncol(experts)
col <- rev(RColorBrewer::brewer.pal(n = max(min(K,11),4),name = "Spectral"))[1:min(K,11)]
matplot(cumsum_exp, type='l', col=col, lty=1, lwd=2)
par(new=T)
plot(Data1$GovernmentResponseIndex, lwd=2, type='l', axes=F, ylab='')
legend("bottomleft", col=col, legend=colnames(experts), lty=1, bty='n')


or <- oracle(Y=Data1$Load, experts)
or


#######bias correction
expertsM2000 <- experts-2000
expertsP2000 <- experts+2000
experts <- cbind(experts, expertsM2000, expertsP2000)
colnames(experts) <-c(nom_exp, paste0(nom_exp,  "M"), paste0(nom_exp,  "P"))


cumsum_exp <- apply(Data1$Load-experts, 2, cumsum)

par(mfrow=c(1,1))
K <-ncol(experts)
col <- rev(RColorBrewer::brewer.pal(n = max(min(K,11),4),name = "Spectral"))[1:min(K,11)]
matplot(cumsum_exp, type='l', col=col, lty=1, lwd=2)
par(new=T)
plot(Data1$GovernmentResponseIndex, lwd=2, type='l', axes=F, ylab='')
legend("bottomleft", col=col, legend=colnames(experts), lty=1, bty='n')


or <- oracle(Y=Data1$Load, experts)
or


agg <- mixture(Y = Data1$Load, experts = experts, model = "MLpol", loss.gradient=FALSE)
summary(agg)

agg <- mixture(Y = Data1$Load, experts = experts, model = "MLpol", loss.gradient=TRUE)
summary(agg)

agg <- mixture(Y = Data1$Load, experts = experts, model = "BOA", loss.gradient=TRUE)
summary(agg)
plot(agg)


ssm_dyn2 <- ssm_dyn
ssm_dyn2$kalman_params$Q <- ssm_dyn$kalman_params$Q*1000
ssm_dyn2 <- predict(ssm_dyn2, X, y, type='model', compute_smooth = TRUE)
gam9.kalman.Dyn2 <- ssm_dyn2$pred_mean%>%tail(nrow(Data1))



experts <- cbind(experts, gam9.kalman.Dyn2)
agg <- mixture(Y = Data1$Load, experts = experts, model = "MLpol", loss.gradient=TRUE)
summary(agg)

plot(agg)


##################################################################################################################################
########################new features
##################################################################################################################################

Data_train <- read_delim("Data/train.csv", delim=",")
Data_test<- read_delim("Data/test.csv", delim=",")
Data_train$Time <- as.numeric(Data_train$Date)
Data_test$Time <- as.numeric(Data_test$Date)
names(Data_train)
names(Data_test)
dim(Data_train)
dim(Data_test)


Data_test <- subset(Data_test, select = -Id )
Data_test <- add_column(Data_test, Load =lead(Data_test$Load.1, default = mean(Data_test$Load.1)), .after = "Date")
Data  <- rbind(Data_train, Data_test)

google <- readRDS(file='Data/google.RDS')
Data <- left_join(Data, google, by=c('Date'='date'))
par(mfrow=c(1,1))
plot(Data$Date, Data$Auverg_grocery_and_pharmacy, type='l')

sel <- which(Data$Date=="2020-02-15")
Data_a <- Data[1:sel,]
Data_b <- Data[-(1:sel),]

range(Data_a$Date)
range(Data_b$Date)


########################################################
########missing values corrections
########################################################
nb_na <- lapply(Data_b, function(x){which(is.na(x))%>%length})%>%unlist
nb_na%>%sort
plot(Data_b$Date, Data_b$Britta_transit_stations, type='l')

# Britta_transit_stations_na <- Data_b$Britta_transit_stations
# sel_na <- which(is.na(Data_b$Britta_transit_stations))
# rf_na <- ranger(Britta_transit_stations ~ WeekDays+GovernmentResponseIndex, data=Data_b[-sel_na,])
# rf_na
# Data_b$Britta_transit_stations[sel_na] <- predict(rf_na, data=Data_b[sel_na,])$prediction

Data_b_na <- Data_b
col.na <- which(nb_na>0)

for(j in c(1:length(col.na)))
{
  sel_na <- which(is.na(Data_b[,col.na[j]]))
  eq <- paste0(names(Data_b)[col.na[j]], "~ WeekDays + GovernmentResponseIndex")
  rf_na <- ranger(as.formula(eq), data=Data_b[-sel_na, ])
  rf_na
  Data_b[sel_na,col.na[j]] <- predict(rf_na, data=Data_b[sel_na,])$prediction
}


summary(Data_b)

plot(Data_b$Date, Data_b$Britta_transit_stations, col='red', type='b')
lines(Data_b$Date, Data_b_na$Britta_transit_stations, type='l')




Ntree <- 500
mtry <- 13
equation <- Load~  Time + toy + Temp + Load.1 + Load.7 + Temp_s99 + WeekDays + BH + Temp_s95_max + Temp_s99_max + Summer_break  + Christmas_break + 
  Temp_s95_min +Temp_s99_min + DLS + GovernmentResponseIndex
rf<- ranger::ranger(equation, data=Data_a, num.trees = Ntree, mtry=mtry)
rf.fitted <-  predict(rf, data=Data_a)$predictions
rf.forecast <-  predict(rf, data=Data_b)$predictions

Data_a$residuals <- Data_a$Load-rf.fitted
Data_b$residuals <- Data_b$Load-rf.forecast

Data_a$res.48 <- c(Data_a$residuals[1], Data_a$residuals [1:(length(Data_a$residuals )-1)])
Data_a$res.336 <- c(Data_a$residuals[1:7], Data_a$residuals [1:(length(Data_a$residuals )-7)])

Data_b$res.48 <- c(Data_b$residuals[1], Data_b$residuals [1:(length(Data_b$residuals )-1)])
Data_b$res.336 <- c(Data_b$residuals[1:7], Data_b$residuals [1:(length(Data_b$residuals )-7)])


newdata <- names(Data_a)[22:99]
equation_res_newdata <- paste0("residuals~  Time + toy + Temp + Load.1 + Load.7 + Temp_s99 + WeekDays + BH + Temp_s95_max + Temp_s99_max + Summer_break  + Christmas_break + 
  Temp_s95_min +Temp_s99_min + DLS + GovernmentResponseIndex + res.48 + res.336 + ", '+', paste0(newdata, collapse  ='+'))

equation_res <- paste0("residuals~  Time + toy + Temp + Load.1 + Load.7 + Temp_s99 + WeekDays + BH + Temp_s95_max + Temp_s99_max + Summer_break  + Christmas_break + 
  Temp_s95_min +Temp_s99_min + DLS + GovernmentResponseIndex + res.48 + res.336")

rf.forecast_residuals <- array(0, dim=nrow(Data_b))
rf.forecast_residuals_newdata <- array(0, dim=nrow(Data_b))
VarImp <- NULL
for(i in c(6: (nrow(Data_b)-1)))
{
  rf_res_newdata<- ranger::ranger(equation_res_newdata, data=Data_b[1:i,], num.trees = 10, importance =  'permutation')
  VarImp <- rbind(VarImp, rf_res_newdata$variable.importance)
  rf.forecast_residuals_newdata[i+1] <- predict(rf_res_newdata, data=Data_b[1:i+1,], predict.all = F)$predictions%>%tail(1)
  
  rf_res<- ranger::ranger(equation_res, data=Data_b[1:i,], num.trees = 10, importance =  'permutation')
  rf.forecast_residuals[i+1] <- predict(rf_res, data=Data_b[1:i+1,], predict.all = F)$predictions%>%tail(1)
  
  print(i)
}

rf.newdata.forecast <- rf.forecast+rf.forecast_residuals_newdata
rf.update.forecast <- rf.forecast+rf.forecast_residuals

length(rf.update.forecast)
rf.newdata.forecast-rf.update.forecast
  
rmse(y=Data_b$Load, ychap=rf.forecast)
rmse(y=Data_b$Load, ychap=rf.newdata.forecast)
rmse(y=Data_b$Load, ychap=rf.update.forecast)

plot(Data_b$Date, Data_b$Load, type='l')
lines(Data_b$Date, rf.forecast, col='red')
lines(Data_b$Date,rf.newdata.forecast, col='blue')
lines(Data_b$Date, rf.update.forecast, col='green')

colnames(VarImp)

o <- colSums(VarImp, na.rm=TRUE)%>%order(decreasing=TRUE)


plot(VarImp[,o[1]], type='l', ylim=range(VarImp))

colnames(VarImp)[o]

plot(Data_b$Date[-c(1:6)], VarImp[, "Britta_retail_and_recreation" ], type='l')






##################################################################################################################################


rf <- ranger(equation, data=Data0, importance =  'permutation') # Error: Missing data in columns































library(mobForest)


d1 <- which(as.character(Data$Date)=="2020-03-15")
Data0 <- Data[1:d1,]
Data1 <- Data[-c(1:d1),]


form <- Load ~ Temp + Load.1 + Load.7 + Temp_s99 + as.numeric(Date)
partition_vars <- c("toy", "Temp_s95_min", "Temp_s99_min", "WeekDays", "BH")


form <- Load ~ Temp + Load.1 + Load.7 + Temp_s99 
partition_vars <- c("toy", "Temp_s95_min", "Temp_s99_min", "WeekDays", "BH")

rfout <- mobforest.analysis(form, , partition_vars=partition_vars, mobforest_controls = mobforest.control(ntree = 10, mtry = 2, 
                                    replace = TRUE, alpha = 0.05, bonferroni = TRUE, minsplit = 10), data = Data0,
                                    processors = 4, model = linearModel, seed = 1111)



form <- as.formula(Load ~Load.1)
partition_vars <- c("toy")

rfout <- mobforest.analysis(form, , partition_vars=partition_vars, mobforest_controls = mobforest.control(ntree = 10, mtry = 2, 
                            replace = TRUE, alpha = 0.05, bonferroni = TRUE, minsplit = 10), data = Data0,
                            processors = 1, model = linearModel, seed = 1111)








library(mlbench)
set.seed(1111)
# Random Forest analysis of model based recursive partitioning load data
data("BostonHousing", package = "mlbench")
BostonHousing <- BostonHousing[1:90, c("rad", "tax", "crim", "medv", "lstat")]

# Recursive partitioning based on linear regression model medv ~ lstat with 3
# trees.  1 core/processor used. 
rfout <- mobforest.analysis(as.formula(medv ~ lstat), c("rad", "tax", "crim"),
                            mobforest_controls = mobforest.control(ntree = 3, mtry = 2, replace = TRUE,
                                                                   alpha = 0.05, bonferroni = TRUE, minsplit = 25), data = BostonHousing,
                            processors = 1, model = linearModel, seed = 1111)
## Not run: 
rfout  


names(BostonHousing)






equation <- "Load~ s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +s(Temp_s99,k=10, bs='cr')+ WeekDays +BH  + 
te(Temp_s95_max, Temp_s99_max) + te(Temp_s95_min, Temp_s99_min)"
gam9<-gam(equation%>%as.formula, data=Data)
gam9.forecast <- predict(gam9, newdata=Data_test)

X <- predict(gam9, newdata=Data, type='terms')

###scaling columns
for (j in 1:ncol(X))
  X[,j] <- (X[,j]-mean(X[,j])) / sd(X[,j])
X <- cbind(X,1)
d <- ncol(X)

y <- Data$Load

# static 
ssm <- viking::statespace(X, y)
ssm
gam9.kalman.static <- ssm$pred_mean%>%tail(nrow(Data_test))

length(y%>%tail(nrow(Data_test)))
rmse(y=y%>%tail(nrow(Data_test)), ychap=gam9.forecast)
rmse(y=y%>%tail(nrow(Data_test)), ychap=gam9.kalman.static)


plot(Data_test$Date, y%>%tail(nrow(Data_test)), type='l')
lines(Data_test$Date, gam9.forecast, col='red')
lines(Data_test$Date, gam9.kalman.static, col='blue')

plot(ssm, pause=F, window_size = 14, date = data$Date, sel = test)
plot(ssm, pause=F, window_size = 14, date = data$Date)





