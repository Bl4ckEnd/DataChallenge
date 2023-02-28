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

p_response + p_load

hist(Data1$GovernmentResponseIndex)

# <50, 50 - 62, >62
new_Data1 <- Data1 %>% 
  mutate(GRI_factor=cut(GovernmentResponseIndex, breaks = c(-Inf, 50, 62, Inf), labels = c("none", "medium", "high")))

new_Data0 <- Data0 %>% 
  mutate(GRI_factor=cut(GovernmentResponseIndex, breaks = c(-Inf, 50, 62, Inf), labels = c("none", "medium", "high")))

save(new_Data0, file = "Data/Data0.Rda")
save(new_Data1, file = "Data/Data1.Rda")
