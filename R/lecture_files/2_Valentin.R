rm(list=objects())
graphics.off()
setwd("~/Library/CloudStorage/OneDrive-Personal/Paris Saclay/Modélisation prédictive")

# libraries 
library(tidyverse)
library(lubridate)
library(forecast)
library(MLmetrics)
library(yarrr)

# import data
Data0 <- read_delim("Data/train.csv", delim = ",")
Data1 <- read_delim("Data/test.csv", delim = ",")

sel_a <- which(Data0$Year<=2019)
sel_b <- which(Data0$Year>2019)

###########
# feature engineering
###########

rmse <- function(y, ychap) {
  z = sqrt(mean((y-ychap)^2))
  z
}

mod0 <- lm(Load ~ WeekDays, data=Data0[sel_a,])
summary(mod0)
mod0.forecast <- predict(mod0, newdata = Data0[sel_b,])
rmse(y=Data0$Load[sel_b], ychap=mod0.forecast)

#### block CV
Nblock <- 8 
borne_block <- seq(1, nrow(Data0), length=Nblock+1)%>%floor
block_list <- list()
l <- length(borne_block)
for (i in c(2:(l-1))) {
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]] <- c(borne_block[l-1]:(borne_block[l]))


fitmod <- function(eq, block) {
  mod <- lm(eq, data=Data0[-block,])
  mod.cvpred <-  predict(mod, newdata=Data0[block,])
  return(mod.cvpred)
}


mod0.cvpred <- lapply(block_list, fitmod, eq="Load~WeekDays")%>%unlist
rmse(y=Data0$Load, ychap = mod0.cvpred)

#### regroupement de modalités
Data0$WeekDays2 <- forcats::fct_recode(Data0$WeekDays, "WorkDay"="Thursday", "WorkDay"="Tuesday", "WorkDay"="Wednesday")
mod0 <- lm(Load~WeekDays2, data=Data0[sel_a,])
summary(mod0)
mod0.forecast <- predict(mod0, newdata = Data0[sel_b,])
rmse(y=Data0$Load[sel_b], ychap=mod0.forecast)

mod0.cvpred <- lapply(block_list, fitmod, eq="Load~WeekDays")%>%unlist
rmse(y=Data0$Load, ychap = mod0.cvpred)


########### polynomial transforms
mod1 <- lm(Load~WeekDays2+Temp, data=Data0[sel_a,])
summary(mod1)
mod1.forecast <- predict(mod1, newdata = Data0[sel_b,])
rmse(y=Data0$Load[sel_b], ychap=mod1.forecast)

mod1.cvpred <- lapply(block_list, fitmod, eq="Load~WeekDays+Temp")%>%unlist
rmse(y=Data0$Load, ychap = mod1.cvpred)

plot(Data0[sel_a,]$Temp,Data0[sel_a,]$Load)
plot(Data0[sel_a,]$Temp,mod1$residuals)

mod2 <- lm(Load~WeekDays2+Temp+I(Temp^2), data=Data0[sel_a,])
summary(mod2)
mod2.forecast <- predict(mod2, newdata = Data0[sel_b,])
rmse(y=Data0$Load[sel_b], ychap=mod2.forecast)

mod2.cvpred <- lapply(block_list, fitmod, eq="Load~WeekDays+Temp+I(Temp^2)")%>%unlist
rmse(y=Data0$Load, ychap = mod1.cvpred)

plot(Data0[sel_a,]$Temp, mod2$residuals)


plot(Data0$Date, Data0$Load-mod2.cvpred, type="l")
lines(Data0$Date[sel_b], Data0$Load[sel_b]-mod2.forecast, col="red")


# variance des scores par bloc?
mod1.rmse_block <- lapply(block_list, function(x){rmse(y=Data0$Load[x], ychap=mod1.cvpred[x])})%>%unlist
mod2.rmse_block <- lapply(block_list, function(x){rmse(y=Data0$Load[x], ychap=mod2.cvpred[x])})%>%unlist

col <- yarrr::piratepal("basel")
boxplot(cbind(mod1.rmse_block, mod2.rmse_block), col=col[1:2], ylim=c(2000,7000))
abline(h~rmse(y=Data0$Load[sel_b], ychap=mod1.forecast), col=col[1], lty="l")

