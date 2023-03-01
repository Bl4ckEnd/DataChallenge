rm(list=objects())
library(tidyverse)

load("Data/Data0.Rda")
load("Data/Data1.Rda")

sel_a <- which(Data0$Year<=2019)
sel_b <- which(Data0$Year>2019)

#### create linear model with all variables
formula <- "Load ~ Load.1 + Load.7 + Temp + toy + WeekDays + BH + Year + Month + DLS + Summer_break + Christmas_break"
formula2 <- "Load ~ Load.1 + Load.7 + Temp + toy + WeekDays + BH + Year + Month + DLS + Summer_break + Christmas_break + TauxPopMovement + Movement"

mod0 <- lm(formula , data=Data0[sel_a,])
summary(mod0)
mod0.forecast <- predict(mod0, newdata=Data0[sel_b,])

mod1 <- lm(formula2 , data=Data0[sel_a,])
summary(mod1)
mod1.forecast <- predict(mod0, newdata=Data0[sel_b,])

anova(mod0, mod1)
