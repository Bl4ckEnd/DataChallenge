rm(list=objects())
graphics.off()

setwd("~/M1 Maths&IA/S2/Modelisation predictive/DataChallenge")
library(tidyverse)
library(lubridate)
library(mgcv)
library(bestglm)
library(qgam)

load("Data/Data0.Rda")
load("Data/Data1.Rda")

#########Mise en forme des données
Data0$Time <- as.numeric(Data0$Date)
Data1$Time <- as.numeric(Data1$Date)

sel_a <- which(Data0$Year<=2019)
sel_b <- which(Data0$Year>2019)

par(mfrow=c(1,1))
g0 = gam(Load~s(Temp, k=3, bs='cr'), data=Data0[sel_a,])

summary(g0) #GCV: erreur de validation croisée par erreur quadratique 
plot(Data0$Temp[sel_a], g0$residuals, pch=16)

g_prov = gam(g0$residuals~s(Data0$Temp[sel_a], k=20, bs='cr'), data=Data0[sel_a,])
summary(g_prov)
#si j'ameliore mon k --> donne idée de cb je peux ameliorer en rmse
#R2 = 0.84 et gagné en gcd

g1 = gam(Load ~ s(Temp, k=10, bs='cr'),data=Data0[sel_a,])
summary(g1)
plot(Data0$Temp[sel_a], g0$residuals, pch=16, col='grey')
points(Data0$Temp[sel_a], g1$residuals, pch=16)

(g0$gcv.ubre - g1$gcv.ubre)/g0$gcv.ubre
sqrt(g0$gcv.ubre)
sqrt(g1$gcv.ubre)

plot(g1, residuals=T)

##VOIR VALIDATION CROISÉE SUR LE TP REGRESSION LINEAIRE

#On va faire des gam avec tous les effets

gam1 = gam(Load ~ s(Date, k=3, bs='cr')+s(toy, k=30, bs='cr')+s(Temp, k=10, bs='cr'), data=Data0[sel_a,])
summary(gam1)
plot(gam1, residuals=T)

###VOIR CORRECTION DU PROF
mod0 = gam(Load ~ WeekDays2*DLS+BH+ WeekDays*Load.1+ GovernmentResponseIndex +
              s(Temp_s95_max - Temp_s95_min)+
            + s(as.numeric(Date), k=3, bs='cr')+ s(toy,k=30, bs='cc', by=as.factor(WeekDays))
            +s(Time, k=5)+s(Temp,k=10, bs='cr', by=as.factor(WeekDays2)),
            data=Data0)
#cree une fct pour chq type de jour
summary(mod0)
#plot(mod0, residuals = T)
pred.mod0 = predict(mod0, newdata=Data0[sel_b,])
plot(pred.mod0, type='l', col='black')
lines(Data0$Load[sel_b], col='red')

rmse(y=Data0$Load[sel_b], ychap=pred.mod0)
#deux regimes dans l'erreur de notre modele (Été-hiver)--> mélange gaussien toujours 
#jours feries sont importants--> bonne idée d'améliration du modèle 

##SUBMISSION 7
forecast <- predict(mod0, newdata=Data1)
submit <- read_delim( file="Data/sample_submission.csv", delim=",")
submit$Load <- forecast
#write.table(submit, file="Data/submission:L8.csv", quote=F, sep=",", dec='.',row.names = F)


####EXPLORER BLOCK RESIDUALS









