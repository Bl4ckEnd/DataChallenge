rm(list=objects())
setwd("~/Documents/M1/Modelisation predictive") #fac

#Importation des librairies
library(tidyverse)
library(lubridate)
library(forecast)

#####OBJECTIF DU PROJET: PREDIRE LA CONSOMMATION VERS LA FIN DU JEU DE DONNEES (après 2020)

#Importation du jeu de données
Data0 = read_delim("train.csv", delim=",")
Data1 = read_delim("test.csv",delim=",")

range(Data0$Date)
range(Data1$Date)

names(Data0)
summary(Data0)

###### ETUDE DES VARIABLES ############

###Variables sur la consomation d'energie
#Load: conso des français en Mega Watts -> pics dependent de l'heure de la journée (gros volumes: train 20MW)
#Regarder Load1 et Load7


###############Variables sur la Temperature
#Temp: température moyenne/jour, moyenne spatiale (32 stations meteo dans la france avec poids uniformes) --> moyenné en temps et space
#Temp_snb : lissage de la temperature
      # Temps_{instant t} --> TL_t = alpha*T_t + (1-alpha)*TL_{t-1}
      #Fortes variations de temp dans une meme journéé--> impact sur la conso d'energie
#Temp_s95_max : temp maximale dans une journée
#Temp_s95_min : temp minimale dans une journée


################Variables sur les dates
#Date: chaine de caractere AAAA/MM/JJ
#Toy: regarder schema du prof (Time of Year)
#Weekdays = jours de la semaine en chaine de caractere
#BH : jours fériés --> 
#### Variables liées a la date: Year, Month
#DLS: heure d'hiver et heure d'été


################Variables vacances: 
#Summer Break: Vacances d'été (Vancances d'août)--> faire variables plus fines: vacances paques, fevrier...
#Christmas break: Vacances Noel


##Restrinctions covid
#GovernmentResponseIndex: Degre de restrinctions imposées par le gouvernement

#Pour l'info:   2500 MWtt = -2°C de temperature 

##############Consommation au cours du temps
plot(Data0$Date, Data0$Load, type='l',xlim=range(Data0$Date,Data1$Date)) 

#Observations: variations sur les semaines
## 2 cycles: hebdomadaire et annuel

#Baisses tres en rapport avec la position (en terme de jours) des jours festifs
#Plus de variations sur les maximums que les minimums
# Variation en hiver plus forte: en fonction de la temperature // ete (pas tout le monde a de clime)
#Hiver 2012 dernier hiver le plus froid: plus fortes conso
#Chute: été pas de conso de chauffage + pd les vacances les industries s'arretent --> moins de conso
#Stabilité en termes de conso en France --> desindustrialisation

###Penser a faire des Series de Fourier 

hist(Data0$Load, breaks=100)
#Observations: 
#2 modes de conso: 1ere moitié et 2eme moitié ---> melange gaussien (dependant de la periode)
#conso hiver et conso été 
#on va utilser des probas conditionnelles a des parametres

plot(Data0$Date, Data0$Temp, type='l')

#On voit les basse temperatures en hiver 2012
#Relation --> temperature consomation

a = 1
b = a + 365

plot(Data0$Date[a:b], Data0$Load[a:b], type='l')
#Conso descend en ete 
#Varie lors des jours festifs 


plot(Data0$toy) #Commence a 0 chq année et fini a 1

#transparence
col.tr = adjustcolor(col='black',alpha=0.2)
plot(Data0$toy, Data0$Load, pch=16, col=col.tr)

#Devient plus foncé qd il y a plusieurs points qui se superposent
#Profils de conso sont tres homogenes en ete?
##1/7 samedi, 1/7 dimanche et le reste est presque dans la mm place
#### Modelisation differente en fonction du jour de semaine
#En hiver il y a une variance très grande donc pas de superposition forte 

#################Plot du moi de join
plot(Data0$Load[which(Data0$Month==6)],type='l')

boxplot(Load~WeekDays, data=Data0)
#vendredi et lundi un peu plus bas que les autres jours de la semaine
#Grosse dispersion dans les données: mais profil sur la valeurs mediane 


par(mfrow=c(1,2))
Acf(Data0$Load, lag.max = 7*60,type=c("correlation"))
Acf(Data0$Load, lag.max = 7*60, type= c("partial"))
#a quel point je peux predire ce que va se passer (Y_t, Y_{t +/- h})

par(mfrow=c(1,1))
Acf(Data0$Load, lag.max = 7*3,type=c("correlation")) #on va jusqu'a 3 semaines
#conso du jour j --> conso du jour suivant 

############### Cycle hebdomadaire 
  # correlations permettent aussi de voir les cycles existant
  #si ma conso est tres correle avec ce qui s'est passé la veille ----> correlation transitive
  # On va regarder conditionnelement 

par(mfrow=c(1,1))
Acf(Data0$Load, lag.max = 7*3,type=c("correlation"))

Acf(Data0$Load, lag.max = 7*3,type=c("partial"))
 #on bp moins de correlation entre le jour j et la veille qd on enleve ce qui se passe au milieu

################ Meteo effect/covariates

par(mar=c(5,5,2,5))
par(mfrow=c(1,1))
plot(Data0$Date, Data0$Load, type='l')
par(new=T)
plot(Data0$Date, Data0$Temp, type='l', col="red", axes=F, xlab='',ylab='')
axis(side=4, col='red', col.axis='red')
mtext(side=4, line = 3, 'Temperature', col='red')
legend("top",c("Load","Temperature"), col=c("black","red"),lty=1, ncol=1,bty="n")

col.tr = adjustcolor(col='black',alpha=0.25)
plot(Data0$Temp, Data0$Load, pch=3, col=col.tr)
#effet de la temperature est neutre (ete) on va le strat au vu des jours de la semaine
#en hiver la variabilité est forte donc on ne voit pas l'effet des jours  de la semaine
#Un modele lineaire n'est pas suffisant, on a qqch lineaire par morceaux
#il va falloir faire des transformations

plot(Data0$Date%>%head(,n=7*3), Data0$Temp%>%head(,n=7*3), type='l')
lines(Data0$Date%>%head(,n=7*3), Data0$Temp_s95%>%head(,n=7*3), col='blue') #superpose une courbe a un graphique
lines(Data0$Date%>%head(,n=7*3), Data0$Temp_s99%>%head(,n=7*3), col='red')

plot(Data0$Date%>%head(,n=7*5), Data0$Temp%>%head(,n=7*5), type='l')
lines(Data0$Date%>%head(,n=7*5), Data0$Temp_s99_min%>%head(,n=7*5), col='blue')
lines(Data0$Date%>%head(,n=7*5), Data0$Temp_s99_max%>%head(,n=7*5), col='red')
#J'ai bien les min et les max qui enveloppent la valeur
#RQ = ce sont des min et max journaliers 

par(mfrow=c(1,1))
col.tr1 = adjustcolor(col='black',alpha=0.25)
col.tr2 = adjustcolor(col='red',alpha=0.25)
plot(Data0$Temp, Data0$Load, pch=3, col=col.tr1)
points(Data0$Temp_s99, Data0$Load, pch=3, col=col.tr2)

#####################Lag

names(Data0)
plot(Data0$Load.7, Data0$Load,pch=3)
plot(Data0$Load.1, Data0$Load, pch=3)
#on voit la formation de clusters (3) peut etre week-ends ...

cor(Data0$Load.1, Data0$Load)
cor(Data0$Load.7, Data0$Load)

###################Vacances
boxplot(Load~Christmas_break, data=Data0)
boxplot(Load~Summer_break, data=Data0)
boxplot(Load~BH, data=Data0)


###############DLS
boxplot(Load~DLS, data=Data0)


############### train/Test
#s'interesser a la distribution des variables explicatives dans les 2 sets

par(mfrow=c(1,2))
hist(Data0$Temp)
hist(Data1$Temp)

#c'est deux histogrammes: mq les données dans la partie apprentissage il y a plus de variabilité

#superposition des histogrammes
 par(mfrow=c(1,1))
 hist(Data0$Temp, xlim=range(Data0$Temp, Data1$Temp), col='lightblue')
 par(new=T)
 hist(Data1$Temp, xlim=range(Data0$Temp, Data1$Temp), col=adjustcolor('red',alpha.f=0.25),breaks=50)

 
############## Indice gouvernamentale 
 par(mfrow=c(1,2))
 plot(tail(Data0$Date,365), tail(Data0$GovernmentResponseIndex,365), type='l', ylim=c(0,80))
 plot(Data1$Date, Data1$GovernmentResponseIndex, type='l')
 
 par(mfrow=c(1,1))
plot(Data0$GovernmentResponseIndex, Data0$Load)


#A FAIRE POUR LA SEMAINE PRO
##Reflechir a quel genre de modele de regression lineaire il faudrait appliquer 
## Modele qui prevoit le 1er jour--> mise a jour des param --> prevoit le jour suivant

## Pistes pour recuperer des données (RTE --> recuperer la donnée electrique et prendre QUE le passé)
### Temps d'ensoleillement par jour (T° Synop)
### FABRIQUER LES JOURS PONTS (AVANT-APRES FESTIFS) 
### Vacances scolaires (par zones aussi)
### Donées electriques regionales (eco2mix)
### Enedis (routes locales)--> Données de panels de consommateurs 
### Temperature ressentie: vent et humidité
### Grèves 
### Flux de vehicules
### Google mobility report: indicateurs de changements de mobilité au niveau de la population (commence au moment du confinement)
    # recuperation de l'utilisation de GoogleMaps (bar, restaurants,...)
### Periode d'apprentissage et Test: verifier que la variable existe a la periode que l'on veut predire


