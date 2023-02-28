rm(list=objects())
setwd("~/Documents/M1/Modelisation predictive/Modelisation predictive") #fac
setwd("~/M1 Maths&IA/S2/Modelisation predictive/DataChallenge") #computer

#Importation des librairies
library(tidyverse)
library(lubridate)
library(forecast)
#source('R/score.R')

rmse = function(y, ychap, digits=0){
  return(round(sqrt(mean((y-ychap)^2, na.rm=TRUE)), digits=digits))
}


#Importation du jeu de données
Data0 = read_delim("Data/train.csv", delim=",")
Data1 = read_delim("Data/test.csv",delim=",")

Data0$Time = as.numeric(Data0$Date)
Data1$Time = as.numeric(Data1$Date)

#on simule une periode d'apprentissage et une donnée de test
#plus realiste p/r a notre objectif: garder les données depuis decembre
sel_a = which(Data0$Year<=2019) 
sel_b = which(Data0$Year>2019)

###############feature engineering

#utilisation de lm
mod0 = lm(Load~WeekDays, data=Data0[sel_a,])
summary(mod0)
# t-value c'est le test de Student + pvalue 
#regroupement de modalités
#0.08 de R^2 --> modele explique - de 10% de la variance
mod0.forecast = predict(mod0,newdata=Data0[sel_b,]) #pervision sur l'echqntillon b
rmse(y=Data0$Load[sel_b],ychap=mod0.forecast) #evaluation ereur de prevision
#affiche mssg d'erreur regarder + tard

# modele modele non identifiable: 7 niv de facteurs alors q suffisant avec 6 --> enleve 1 x verifier l'identifiabilité
# ~~ one hot encoder --> Weekdays decomposition with 0&1s


## Bloc CV 
Nblock = 8 
borne_block = seq(1,nrow(Data0), length=Nblock+1)%>%floor
block_list = list()
l = length(borne_block)
for(i in c(2:(l-1))){
  block_list[[i-1]] = c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]] = c(borne_block[l-1]:(borne_block[l]))


fitmod = function(eq,block){
  mod = lm(eq, data =Data0[-block,])
  mod.cvpred = predict(mod, newdata=Data0[block,])
  return(mod.cvpred)
}

mod0.cvpred = lapply(block_list, fitmod, eq = "Load ~ WeekDays")%>%unlist
rmse(y=Data0$Load, ychap=mod0.cvpred, digits=2)



#### regroupement par modalités
Data0$WeekDays2 = forcats::fct_recode(Data0$WeekDays, 'WorkDay'='Thursday','WorkDay'='Tuesday', 'WorkDay'='Wednesday' )
mod0 = lm(Load~WeekDays2, data=Data0[sel_a,])
summary(mod0)
mod0.forecast = predict(mod0,newdata=Data0[sel_b,])
rmse(y=Data0$Load[sel_b], ychap=mod0.forecast)

mod0.cvpred = lapply(block_list, fitmod, eq="Load~WeekDays2")%>%unlist
rmse(y=Data0$Load, ychap=mod0.cvpred, digits=2)

########### Polynomial transformers
mod1 = lm(Load~WeekDays2+Temp, data=Data0[sel_a,])
summary(mod1)
mod1.forecast = predict(mod1, newdata=Data0[sel_b,])
rmse(y=Data0$Load[sel_b], ychap=mod1.forecast)
mod1.cvpred = lapply(block_list, fitmod, eq="Load~WeekDays2 + Temp")%>%unlist
rmse(y=Data0$Load, ychap=mod1.cvpred)


plot(Data0[sel_a,]$Temp, Data0[sel_a,]$Load)
plot(Data0[sel_a,]$Temp, mod1$residuals) #pas bon 



mod2 = lm(Load~WeekDays2+Temp + I(Temp^2), data=Data0[sel_a,])
summary(mod2)
mod2.forecast = predict(mod2, newdata=Data0[sel_b,])
rmse(y=Data0$Load[sel_b], ychap=mod2.forecast)
mod2.cvpred = lapply(block_list, fitmod, eq="Load~WeekDays2 + Temp + I(Temp^2)")%>%unlist
rmse(y=Data0$Load, ychap=mod2.cvpred)


plot(Data0[sel_a,]$Temp, Data0[sel_a,]$Load)
plot(Data0[sel_a,]$Temp, mod2$residuals) 

plot(Data0$Date, Data0$Load - mod2.cvpred, type='l')
lines(Data0$Date[sel_b], Data0$Load[sel_b]-mod2.forecast, col='red')
#on voit: sorte de periodicité annuelle (on ne l'a pas ajouté dans le modele)
### qd on ajoutera cela, on vera des effets de periodicite 
### on devrait avoit des residus gaussiens ----> on n'a pas des points vers le haut mais oui vers le bas (jours feries)
### idée de modeliser la saisonalité, jours feries ...


#### Variance des scores par bloc
mod1.rmse_bloc = lapply(block_list, function(x){rmse(y=Data0$Load[x], ychap=mod1.cvpred[x])})%>%unlist
mod2.rmse_bloc = lapply(block_list, function(x){rmse(y=Data0$Load[x], ychap=mod2.cvpred[x])})%>%unlist

col = yarr::piratepal("basel")
boxplot(cbind(mod1.rmse_bloc, mod2.rmse_bloc), col=col[1:2], ylim=c(2000,7000))
####COPIER LA CORRECTION 



###truncated power fcts
plot(Data0$Temp, Data0$Load, pch =20)

Data0$Temp_trunc1 = pmax(Data0$Temp-15,0) #temperatures trunquées
Data0$Temp_trunc2 = pmax(Data0$Temp-20,0)


mod3 = lm(Load~WeekDays2 + Temp + Temp_trunc1 + Temp_trunc2, data = Data0[sel_a,])
mod3.forecast = predict(mod3, newdata=Data0[sel_b,])
summary(mod3)
rmse(y=Data0$Load[sel_b], ychap=mod3.forecast)

#copier correction

###le mod modelise mieux le pheno de temperature


##copier bases de fouier


######Comment faire une soumission
#on fait un predict du modeme
#on garde l'exemple de sujet de soumission et on remplace la colonne load par notre prediction
#on applique le code donné et on soumet sur la plateforme










