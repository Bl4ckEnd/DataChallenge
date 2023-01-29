rm(list=objects())
setwd("~/M1 Maths&IA/S2/Modelisation predictive/DataChallenge")#computer
library(tidyverse)

#######THIS WON'T WORK
#Telechargement des données
Data0 = read_delim("Data/train.csv", delim=",")
Data1 = read_delim("Data/test.csv",delim=",")
Temperatures = read_delim("Data/donnees-synop-essentielles-omm.csv")
#as.posix

#selection des variables d'interet
features =c(2, 65, 10)
Temp = Temperatures[,features]

#definition des coefficients pour calculer la temperature resentie
coef = c(-8.785, 1.611, 2.339, -0.146, -1.231*10**(-2), -1.642*10**(-2), 2.212*10**(-3),7.255*10**(-4), -3.582*10**(-6))

#fonction calculant pour chaque ligne la temperature ressentie 
Heat_Index = function(tab){
  t = as.numeric(tab[2])
  H = as.numeric(tab[3])/100
  resultat = coef[1] + coef[2]*t + coef[3]*H + coef[4]*t*H + coef[5]*(t**2) + coef[6]*(H**2) + coef[7]*(t**2)*H + coef[8]*t*(H**2) + coef[9]*(t**2)*(H**2)
  return(resultat)
}

#on cree un vecteur que l'on rajoutera a Temp
n = dim(Temp)[1]
HI = rep(0, n)
HI = apply(Temp, 1, Heat_Index)

Temp = cbind(Temp, HI)
Temp = drop_na(Temp)

#ligne 1 et ligne 2 ont les mêmes dates
substr(Temp[2,1], 1, 10) == substr(Temp[1,1], 1, 10)

n = dim(Temp)[1]
nD = dim(Data0)[1]
Temp_ressentie = rep(0, nD)

Temp[,1] = as.Date(substring(Temp[,1],1,10))

df_TR = Temp %>% group_by(Date)  %>%
  summarise(HI = mean(HI))

#write.csv(df_TR, "Data/df_TempRess.csv", row.names=FALSE)

########THIS WILL WORK

Data_New = left_join(Data0, df_TR, by="Date")

#Je regarde les variations
plot(Data_New$Date, Data_New$HI, type='l')
col.tr = adjustcolor(col='black',alpha=0.25)
plot(Data_New$HI, Data_New$Load, pch=3, col=col.tr)

plot(Data_New$Date%>%head(,n=7*3), Data_New$HI%>%head(,n=7*3), type='l')
lines(Data_New$Date%>%head(,n=7*3), Data_New$Temp_s95%>%head(,n=7*3), col='blue') #superpose une courbe a un graphique
lines(Data_New$Date%>%head(,n=7*3), Data_New$Temp_s99%>%head(,n=7*3), col='red')

plot(Data_New$Date%>%head(,n=7*5), Data_New$HI%>%head(,n=7*5), type='l')
lines(Data_New$Date%>%head(,n=7*5), Data_New$Temp_s95%>%head(,n=7*5), col='blue') #superpose une courbe a un graphique
lines(Data_New$Date%>%head(,n=7*5), Data_New$Temp_s99%>%head(,n=7*5), col='red')

par(mfrow=c(1,1))
col.tr1 = adjustcolor(col='black',alpha=0.25)
col.tr2 = adjustcolor(col='red',alpha=0.25)
plot(Data_New$HI, Data_New$Load, pch=3, col=col.tr1)
points(Data_New$Temp_s99, Data_New$Load, pch=3, col=col.tr2)



