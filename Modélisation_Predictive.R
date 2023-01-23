rm(list=objects())
graphics.off()
setwd("~/Library/CloudStorage/OneDrive-Personal/Paris Saclay/Modélisation prédictive")

# libraries 
library(tidyverse)
library(lubridate)
library(forecast)

# import data
train <- read_delim("Data/train.csv", delim = ",")
test <- read_delim("Data/test.csv", delim = ",")
attach(train)

# check out data
range(test$Date)
range(train$Date)

##### summaries
summary(train)
head(train)

names(train)

##### trend
plot(train$Date, train$Load, type="l", xlim = range(train$Date,test$Date))
hist(train$Load,breaks = 100)
plot(train$Date,train$Temp, type="l")

a <- 1
b <- a + 365
plot(train$Date[a:b], train$Load[a:b], type="l")

plot(train$toy)

col.tr = adjustcolor(col="black",alpha="0.2")
plot(train$toy,train$Load,pch=16, col=col.tr)

#### weekly cycles
plot(train$Load[which(train$Month==6)],type="l")
boxplot(Load~WeekDays)

par(mfrow=c(1,1))
Acf(train$Load, lag.max=7*3, type=c("correlation"))
Acf(Load, lag.max=7*3, type=c("partial"))

par(mfrow=c(1,2))
Acf(Load, lag.max=7*60, type=c("correlation"))
Acf(Load, lag.max=7*60, type=c("partial"))

####### Meteo effect
par(mar=c(5,5,2,5))
par(mfrow=c(1,1))
plot(Date, Load, type="l")
par(new=T)
plot(Temp%>%tail(1000), type="l", col="red", axes=F, xlab="", ylab="")
axis(side=4, col="red", col.axis="red")
mtext(side=4, line=3, "Temperature", col="red")
legend("top",c("Load","Temperature"),col=c("black","red"), lty=1, ncol=1, bty="n")

col.tr <- adjustcolor(col="black", alpha=0.25)
plot(Temp, Load, pch=3, col=col.tr)

plot(Date%>%head(,n=7*3), Temp%>%head(,n=7*3), type="l")
lines(Date%>%head(,n=7*3), Temp_s95%>%head(,n=7*3), col="blue")
lines(Date%>%head(,n=7*3), Temp_s99%>%head(,n=7*3), type="l", col="red")

plot(Date%>%head(,n=7*5), Temp%>%head(,n=7*5), type="l")
lines(Date%>%head(,n=7*5), Temp_s95_min%>%head(,n=7*5), col="blue")
lines(Date%>%head(,n=7*5), Temp_s99_max%>%head(,n=7*5), type="l", col="red")

par(mfrow=c(1,1))
col.tr1 = adjustcolor(col="black", alpha=0.25)
col.tr2 = adjustcolor(col="red", alpha=0.25)
plot(Temp, Load, pch=3, col=col.tr1)
points(Temp_s99, Load, pch=3, col=col.tr2)

########### lag
names(train)
plot(Load.7, Load, pch=3)
plot(Load.1, Load, pch=3)

cor(Load.1, Load)
cor(Load.7, Load)

######### Holidays
boxplot(Load~Christmas_break, data=train[which(train$DLS==1),])
boxplot(Load~Summer_break)
boxplot(Load~BH)

######### DLS
boxplot(Load~DLS)

detach(train)
######### train / test
par(mfrow=c(1,2))

hist(train$Temp)
hist(test$Temp)

range(train$Temp)
range(test$Temp)

par(mfrow=c(1,1))
hist(train$Temp, xlim=range(train$Temp, test$Temp), col="lightblue", breaks = 50)
par(new=T)
hist(test$Temp, xlim=range(train$Temp, test$Temp), col=adjustcolor("red", alpha.f=0.5), breaks = 50)

par(mfrow=c(1,2))
plot(tail(train$Date, 365), tail(train$GovernmentResponseIndex, 365), type = "l", ylim = c(0,80))
plot(tail(test$Date, 365), tail(test$GovernmentResponseIndex, 365), type = "l", ylim = c(0,80))

par(mfrow=c(1,1))
plot(train$GovernmentResponseIndex, train$Load)







# T_t, TL_T = alphaT_t + (1-alpha)TL_t, alpha in (0,1)
# --> factor bank holidays
# idea: find data on hours of light per day
# Temperature -> Synopse open Data
# price of electricity




df = data.frame(train$Load, train$Load.1)
km = kmeans(df, centers = 18)
plot(train$Load.1, train$Load, pch=3, col=km$cluster)



