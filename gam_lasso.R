rm(list=objects())
###############packages
library(mgcv)
library(yarrr)
library(magrittr)
library(forecast)
library(tidyverse)
source('R/score.R')

#Data <- readRDS("Data/Data_elec.RDS")

load("Data/Data0.Rda")
Data0$Time = as.numeric(Data0$Time)

sel_a <- which(Data0$Year<=2019)
sel_b <- which(Data0$Year>2019)

Data1 <- Data0[sel_b, ]
range(Data1$Date)
Data0 <- Data0[sel_a, ]
range(Data0$Date)


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




#####model 9
rmse = function(y, ychap, digits=0){
  return(round(sqrt(mean((y-ychap)^2, na.rm=TRUE)), digits=digits))
}

equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + WeekDays +BH  + te(Temp_s95_max, Temp_s99_max) + Summer_break  + Christmas_break + te(Temp_s95_min, Temp_s99_min)
Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
Block_residuals <- Data0$Load-Block_forecast
rmse9 <- rmse(Data0$Load, Block_forecast)
rmse9

gam9<-gam(equation, data=Data0)
gam9.forecast<-predict(gam9,  newdata= Data1)
gam9$gcv.ubre%>%sqrt

#####model 9 variante ti
equation <- as.formula("Load~ WeekDays + BH  + Summer_break  + Christmas_break + s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc')+
  s(Temp,k=10, bs='cr') + s(Temp_s99,k=10, bs='cr') +
  ti(Temp_s95_max) + ti(Temp_s99_max) +ti(Temp_s95_min) + ti(Temp_s99_min) +
  ti(Temp_s95_max, Temp_s99_max) + ti(Temp_s95_min, Temp_s99_min) +
  s(Load.1, bs='cr')+ s(Load.7, bs='cr') ")
    
Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
Block_residuals <- Data0$Load-Block_forecast
rmse9_ti <- rmse(Data0$Load, Block_forecast)
rmse9_ti

gam9_ti<-gam(equation, data=Data0)
summary(gam9_ti)
gam9_ti.forecast<-predict(gam9,  newdata= Data1)
gam9_ti$gcv.ubre%>%sqrt


##############################################################################
#############Anova selection
##############################################################################


equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + WeekDays +BH  + te(Temp_s95_max, Temp_s99_max) + Summer_break  + Christmas_break + te(Temp_s95_min, Temp_s99_min)

equation_by <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr', by= as.factor(WeekDays))+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + WeekDays +BH  + te(Temp_s95_max, Temp_s99_max) + Summer_break  + Christmas_break + te(Temp_s95_min, Temp_s99_min)

gam9<-gam(equation, data=Data0)
gam9_by<-gam(equation_by, data=Data0)
gam9$gcv.ubre%>%sqrt
gam9_by$gcv.ubre%>%sqrt

summary(gam9_by)


anova(gam9, gam9_by, test = "Chisq") ####p value <0.05 interaction is significant



###exemple2
equation <- Load~ ti(Load.1, bs='cr')+ ti(Load.7, bs='cr')
equation2 <- Load~ ti(Load.1, bs='cr')+ ti(Load.7, bs='cr') + ti(Load.1, Load.7)
fit1<-gam(equation, data=Data0)
fit2<-gam(equation2, data=Data0)

anova(fit1, fit2, test = "Chisq") ####p value <0.05 interaction is significant


#######removing the "cc" condition
equation <- Load~s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cr') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +
  s(Temp_s99,k=10, bs='cr') + WeekDays +BH  + te(Temp_s95_max, Temp_s99_max) + Summer_break  + Christmas_break + te(Temp_s95_min, Temp_s99_min)
Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
Block_residuals <- Data0$Load-Block_forecast
rmse9_cr <- rmse(Data0$Load, Block_forecast)
rmse9_cr

gam9_cr<-gam(equation, data=Data0)
gam9_cr.forecast<-predict(gam9_cr,  newdata= Data1)

gam9_cr$gcv.ubre%>%sqrt
summary(gam9)
summary(gam9_cr)

terms <- predict(gam9, newdata=Data0, type='terms')
terms_cs <- predict(gam9_cr, newdata=Data0, type='terms')
sel.column <- which(colnames(terms)=="s(toy)")
o <- order(Data0$toy)
plot(Data0$toy[o] , terms[o, sel.column], type='l', ylim=range(terms[o, sel.column], terms_cs[o, sel.column]))
lines(Data0$toy[o] , terms_cs[o, sel.column], col='red')




########################################################################################################
########shrinkage approach
########################################################################################################

equation <- Load~s(as.numeric(Date),k=3, bs='cs') + s(toy,k=30, bs='cs') + s(Temp,k=10, bs='cs') + s(Load.1, bs='cs')+ s(Load.7, bs='cs') +
  s(Temp_s99,k=10, bs='cs') + WeekDays +BH  + te(Temp_s95_max, Temp_s99_max, bs='ts') + Summer_break  + Christmas_break + te(Temp_s95_min, Temp_s99_min, bs='ts')
Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
Block_residuals <- Data0$Load-Block_forecast
rmse9_cs <- rmse(Data0$Load, Block_forecast)
rmse9_cs

gam9_cs<-gam(equation, data=Data0)
gam9_cs.forecast<-predict(gam9_cs,  newdata= Data1)

plot(gam9_cs)
summary(gam9)
summary(gam9_cs)

gam9_cs$gcv.ubre%>%sqrt


##toy
terms <- predict(gam9, newdata=Data0, type='terms')
terms_cs <- predict(gam9_cs, newdata=Data0, type='terms')
sel.column <- which(colnames(terms)=="s(toy)")
o <- order(Data0$toy)
plot(Data0$toy[o] , terms[o, sel.column], type='l', ylim=range(terms[o, sel.column], terms_cs[o, sel.column]))
lines(Data0$toy[o] , terms_cs[o, sel.column], col='red')


terms <- predict(gam9, newdata=Data0, type='terms')
terms_cs <- predict(gam9_cs, newdata=Data0, type='terms')
sel.column <- which(colnames(terms)=="s(Temp)")
o <- order(Data0$Temp)
plot(Data0$Temp[o] , terms[o, sel.column], type='l', ylim=range(terms[o, sel.column], terms_cs[o, sel.column]))
lines(Data0$Temp[o] , terms_cs[o, sel.column], col='red')


##temp_s99
terms <- predict(gam9, newdata=Data0, type='terms')
terms_cs <- predict(gam9_cs, newdata=Data0, type='terms')
sel.column <- which(colnames(terms)=="s(Temp_s99)")
o <- order(Data0$Temp_s99)
plot(Data0$Temp_s99[o] , terms[o, sel.column], type='l', ylim=range(terms[o, sel.column], terms_cs[o, sel.column]))
lines(Data0$Temp_s99[o] , terms_cs[o, sel.column], col='red')



########################################################################################################
########double penalty shrinkage
########################################################################################################

equation <- Load~s(as.numeric(Date),k=3) + s(toy,k=30) + s(Temp,k=10) + s(Load.1)+ s(Load.7) +
  s(Temp_s99,k=10) + WeekDays +BH  + te(Temp_s95_max, Temp_s99_max, bs='ts') + Summer_break  + Christmas_break + te(Temp_s95_min, Temp_s99_min)
Block_forecast<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
Block_residuals <- Data0$Load-Block_forecast
rmse9_dp <- rmse(Data0$Load, Block_forecast)
rmse9_dp

gam9_select<-gam(equation, data=Data0, select=TRUE)
summary(gam9_select)
gam9_select$gcv.ubre%>%sqrt

gam9_cs$gcv.ubre%>%sqrt
gam9_select$gcv.ubre%>%sqrt

summary(gam9_cs)
summary(gam9_select)


##############################################################################
#############lasso/group lasso  selection
##############################################################################

install.packages("plsmselect")
library("plsmselect")
#block coordinate descent algorithm.

equation <- Load~s(as.numeric(Date),k=3, bs='cs') + s(toy,k=30, bs='cs') + s(Temp,k=10, bs='cs') + s(Load.1, bs='cs')+ s(Load.7, bs='cs') +
  s(Temp_s99,k=10, bs='cs') + WeekDays +BH  + te(Temp_s95_max, Temp_s99_max, bs='ts') + Summer_break  + Christmas_break + te(Temp_s95_min, Temp_s99_min, bs='ts')

gfit = gamlasso(equation <- Load~s(as.numeric(Date),k=3, bs='cs') + s(toy,k=30, bs='cs') + s(Temp,k=10, bs='cs') + s(Load.1, bs='cs')+ s(Load.7, bs='cs') +
                  s(Temp_s99,k=10, bs='cs') + WeekDays +BH  + te(Temp_s95_max, Temp_s99_max, bs='ts') + Summer_break  + Christmas_break + te(Temp_s95_min, Temp_s99_min, bs='ts'),
                data = Data0,
                seed = 1)
summary(gfit)
gam9$gcv.ubre%>%sqrt
gam9_cr$gcv.ubre%>%sqrt
gam9_cs$gcv.ubre%>%sqrt
gfit$gam$gcv.ubre%>%sqrt


blockRMSE2<-function(equation, block)
{
  g<- gamlasso(as.formula(equation), data=Data0[-block,])
  forecast<-predict(g, newdata=Data0[block,])
  return(forecast)
} 

Block_forecast<-lapply(block_list, blockRMSE2, equation=equation)%>%unlist
Block_residuals <- Data0$Load-Block_forecast
rmse9_gamlasso <- rmse(Data0$Load, Block_forecast)


##################################################################
######bonne syntaxe mais trop long
##################################################################
Data0$X = model.matrix(~WeekDays + BH + Summer_break  + Christmas_break, data=Data0)[,-1]
gfit_l1 = gamlasso(equation <- Load~ X + s(as.numeric(Date),k=3, bs='cs') + s(toy,k=30, bs='cs') + s(Temp,k=10, bs='cs') + s(Load.1, bs='cs')+ s(Load.7, bs='cs') +
                  s(Temp_s99,k=10, bs='cs')  + te(Temp_s95_max, Temp_s99_max, bs='ts')  + te(Temp_s95_min, Temp_s99_min, bs='ts'),
                data = Data0,
                seed = 1,
                linear.penalty = "l1",
                smooth.penalty = "l1")

gfit$gam$gcv.ubre%>%sqrt
gfit_l1$gam$gcv.ubre%>%sqrt
summary(gfit_l1)



##################################################################
######online learning
##################################################################
Data <- read_delim("Data/train.csv", delim=",")
Data$Time <- as.numeric(Data$Date)

sel_a <- which(Data$Year<=2019)
sel_b <- which(Data$Year>2019)

Data0 <- Data[sel_a, ]
Data1 <- Data[sel_b, ]


equation <- "Load~ s(as.numeric(Date),k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +s(Temp_s99,k=10, bs='cr')+ WeekDays +BH  + 
te(Temp_s95_max, Temp_s99_max) + te(Temp_s95_min, Temp_s99_min)"

equation <- "Load~  s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr') + s(Load.1, bs='cr')+ s(Load.7, bs='cr') +s(Temp_s99,k=10, bs='cr')+ WeekDays"


gam9<-gam(equation%>%as.formula, data=Data0)
gam9.forecast <- predict(gam9, newdata=Data1)

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
gam9.kalman.static <- ssm$pred_mean%>%tail(nrow(Data1))

length(y%>%tail(nrow(Data1)))
rmse(y=Data1$Load, ychap=gam9.forecast)
rmse(y=Data1$Load, ychap=gam9.kalman.static)




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
ssm_em2 <- predict(ssm_em, X[sel_b,], type='model', compute_smooth = TRUE)
ssm_em <-readRDS("Results/ssm_em.RDS")

gam9.kalman.Dyn.em <- ssm_em$pred_mean%>%tail(nrow(Data1))



plot(ssm_em, pause=F, window_size = 14, date = Data$Date, sel = sel_b)
rmse(y=Data1$Load, ychap=gam9.kalman.Dyn.em)

plot(Data1$Date, Data1$Load, type='l')
lines(Data1$Date, gam9.forecast, col='red')
lines(Data1$Date, gam9.kalman.static, col='blue')
lines(Data1$Date, gam9.kalman.Dyn, col='green')
lines(Data1$Date, gam9.kalman.Dyn.em, col='purple')













