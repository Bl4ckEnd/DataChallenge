rm(list=objects())
###############packages
library(mgcv)
library(yarrr)
library(magrittr)
library(forecast)
library(tidyverse)
source('R/score.R')

load("Data/Data0.Rda")
load("Data/Data1.Rda")

Data0 <- read_delim("Data/train.csv", delim=",")
Data1 <- read_delim("Data/test.csv", delim=",")

Data0$Time <- as.numeric(Data0$Date)
Data1$Time <- as.numeric(Data1$Date)


##################################################################
######online learning
##################################################################

equation <- "Load~ s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +s(Temp_s99,k=10, bs='cr')+ WeekDays +BH  + 
te(Temp_s95_max, Temp_s99_max) + te(Temp_s95_min, Temp_s99_min)"

equation <- "Load~  s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +s(Temp_s99,k=10, bs='cr')+ WeekDays"


gam9<-gam(equation%>%as.formula, data=Data0)
gam9.forecast <- predict(gam9, newdata=Data1)

X <- predict(gam9, newdata=Data0, type='terms')
###scaling columns
for (j in 1:ncol(X))
  X[,j] <- (X[,j]-mean(X[,j])) / sd(X[,j])
X <- cbind(X,1)
d <- ncol(X)

y <- Data0$Load

# static 
ssm <- viking::statespace(X, y)
ssm
gam9.kalman.static <- ssm$pred_mean%>%tail(nrow(Data1))

length(y%>%tail(nrow(Data1)))
rmse(y=Data1$Load, ychap=gam9.forecast)
rmse(y=Data1$Load, ychap=gam9.kalman.static)


# create submission: submission_kalman_1_valentin.csv
submit <- read_delim( file="Data/sample_submission.csv", delim=",")
submit$Load <- gam9.kalman.static
write.table(submit, file="Data/submission_kalman_1_valentin.csv", quote=F, sep=",", dec='.',row.names = F)




# dynamic
# using iterative grid search
ssm_dyn <- viking::select_Kalman_variances(ssm, X[sel_a, ], y[sel_a], q_list = 2^(-30:0), p1 = 1, 
                                           ncores = 6)

ssm_dyn <- readRDS("Results/ssm_dyn2.RDS")

ssm_dyn <- predict(ssm_dyn, X, y, type='model', compute_smooth = TRUE)
gam9.kalman.Dyn <- ssm_dyn$pred_mean%>%tail(nrow(Data1))
rmse(y=Data1$Load, ychap=gam9.kalman.Dyn)
plot(ssm_dyn, pause=F, window_size = 14, date = Data$Date)

plot(ssm_dyn, pause=F, window_size = 14, date = Data$Date, sel = sel_b)


# using expectation-maximization
ssm_em <- viking::select_Kalman_variances(ssm, X[sel_a,], y[sel_a], method = 'em', n_iter = 10^3,
                                          Q_init = diag(d), verbose = 10, mode_diag = T)
ssm_em <- predict(ssm_em, X, y, type='model', compute_smooth = TRUE)
ssm_em <-readRDS("Results/ssm_em.RDS")

gam9.kalman.Dyn.em <- ssm_em$pred_mean%>%tail(nrow(Data1))



plot(ssm_em, pause=F, window_size = 14, date = Data$Date, sel = sel_b)
rmse(y=Data1$Load, ychap=gam9.kalman.Dyn.em)

plot(Data1$Date, Data1$Load, type='l')
lines(Data1$Date, gam9.forecast, col='red')
lines(Data1$Date, gam9.kalman.static, col='blue')
lines(Data1$Date, gam9.kalman.Dyn, col='green')
lines(Data1$Date, gam9.kalman.Dyn.em, col='purple')

