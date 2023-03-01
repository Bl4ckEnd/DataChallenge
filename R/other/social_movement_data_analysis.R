rm(list=objects())
library(tidyverse)
library(lubridate)

Data0 <- read_delim("Data/train.csv", delim=",")
Data1<- read_delim("Data/test.csv", delim=",")
social_movement <- read_delim("Data/mouvements-sociaux-depuis-2002.csv", delim=";")

Data0$Time <- as.numeric(Data0$Date)
Data1$Time <- as.numeric(Data1$Date)

head(social_movement)
summary(social_movement)

### First Day Train: [1] "2012-01-01"
### Last Day Train: [1] "2020-04-15"
### First Day Test: [1] "2020-04-16"
### Last Day Test: [1] "2021-01-15"

## Filter correct dates
train_social_movement <- social_movement %>% filter(between(date_de_debut, as.Date("2012-01-01"), as.Date("2020-04-15")))
test_social_movement <- social_movement %>% filter(between(date_de_debut, as.Date("2020-04-16"), as.Date("2021-01-15")))

train_social_movement <- train_social_movement[,c(1,8)]
test_social_movement <- test_social_movement[,c(1,8)]

colnames(train_social_movement) <- c("Date", "TauxPopMovement")
colnames(test_social_movement) <- c("Date", "TauxPopMovement")

new_Data0 <- merge(Data0, train_social_movement, by="Date", all.x = TRUE)
new_Data1 <- merge(Data1, test_social_movement, by="Date", all.x = TRUE)

new_Data0$TauxPopMovement[is.na(new_Data0$TauxPopMovement)] = 0
new_Data1$TauxPopMovement[is.na(new_Data1$TauxPopMovement)] = 0


for (i in 1:length(new_Data0[,1])) {
  if (new_Data0$TauxPopMovement[i] > 0) {
    new_Data0$Movement[i] = 1
  }
  else {
    new_Data0$Movement[i] = 0
  }
}
new_Data0$Movement = factor(new_Data0$Movement)


for (i in 1:length(new_Data1[,1])) {
  if (new_Data1$TauxPopMovement[i] > 0) {
    new_Data1$Movement[i] = 1
  }
  else {
    new_Data1$Movement[i] = 0
  }
}
new_Data1$Movement = factor(new_Data1$Movement)

### plot difference in energy consumption
plot(Load ~ Date, data = new_Data0, col=new_Data0$Movement)



### write new data to storage
write.table(new_Data0, file="Data/new_Data0.csv", sep=",", dec=".", row.names = F, quote=F)
write.table(new_Data1, file="Data/new_Data1.csv", sep=",", dec=".", row.names = F, quote=F)
