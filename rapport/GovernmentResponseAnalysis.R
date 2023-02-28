rm(list=objects())
graphics.off()

############### Packages
library(tidyverse)
library(patchwork)

############### Data
load("Data/Data0.Rda")
load("Data/Data1.Rda")

############### Plot GovernmentResponseIndex
p_response <- Data1 %>% 
  select(Date, GovernmentResponseIndex, Load.1) %>% 
  ggplot() +
  geom_line(aes(Date, GovernmentResponseIndex))

p_load <- Data1 %>% 
  select(Date, GovernmentResponseIndex, Load.1) %>% 
  ggplot() +
  geom_line(aes(Date, Load.1))

p_response / p_load

hist(Data1$GovernmentResponseIndex)



