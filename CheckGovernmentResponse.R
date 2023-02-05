rm(list=objects())
library(tidyverse)
library(lubridate)
library(mgcv)
library(qgam)
library(forecast)

load("Data/Data0.Rda")
load("Data/Data1.Rda")

sel_a <- which(Data0$Year<=2019)
sel_b <- which(Data0$Year>2019)

source('R/score.R')
source("R/mape.R")


########### Plot
n <- length(Data0$Date)
sel <- (n-100):n
plot(Data0$GovernmentResponseIndex[sel])
plot(Data0$Date[sel],Data0$Load[sel], type="l")

pacf(Data0$GovernmentResponseIndex[sel], lag.max = 10)
