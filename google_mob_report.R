gmr2020 <- read_csv("Data/Region_Mobility_Report_CSVs/2020_FR_Region_Mobility_Report.csv")
gmr2021 <- read_csv("Data/Region_Mobility_Report_CSVs/2021_FR_Region_Mobility_Report.csv")
gmr2022 <- read_csv("Data/Region_Mobility_Report_CSVs/2022_FR_Region_Mobility_Report.csv")

gmr <- rbind(gmr2020, gmr2021, gmr2022)

range(gmr2020$date)
range(gmr2021$date)
range(gmr2022$date)
dim(gmr)


list_reg <- gmr$sub_region_1%>%unique
list_reg <- list_reg[-1]
sel_reg <- which(gmr$sub_region_1 == "Île-de-France"  )


gmr_region <- gmr %>% 
  group_by(sub_region_1, date) %>%                            
  summarise(retail_and_recreation = sum(retail_and_recreation_percent_change_from_baseline),
            grocery_and_pharmacy  = sum(grocery_and_pharmacy_percent_change_from_baseline),
            parks=sum(parks_percent_change_from_baseline),
            transit_stations=sum(transit_stations_percent_change_from_baseline),
            workplaces=sum(workplaces_percent_change_from_baseline),
            residential=sum(residential_percent_change_from_baseline))


#############plot idf mobility variables

sel_reg <- which(gmr_region$sub_region_1 == "Île-de-France")
#sel_reg <- which(gmr_region$sub_region_1 == "Auvergne-Rhône-Alpes")
idf <- gmr_region[sel_reg, ]
o <- order(idf$date)
idf  <- idf[o,]
par(mfrow=c(1,1))
plot(idf$date, idf$retail_and_recreation, type='l')
plot(idf$date, idf$grocery_and_pharmacy, type='l')
plot(idf$date, idf$residential, type='l')



#############aggregate all regions in one table

list_reg <- list_reg[-1]
for(j in c(1:length(list_reg)))
{
  sel_reg <- which(gmr_region$sub_region_1 == list_reg[j] )
  reg <- gmr_region[sel_reg, ]
  o <- order(reg$date)
  names(reg) <- paste0(substr(list_reg[j],1,6), "_", colnames(reg))
  
  if(j==1)
  {
    google <- reg[o,-1]
    names(google)[1] <- "date"
  }
  else
  {
    google <- data.frame(google, reg[o, -c(1,2)])
  }
  
  print(dim(reg))
}


names(google)


col <- yarrr::piratepal("info2")

retail.index <- grep(pattern = "retail_and_recreation", x=names(google))
plot(google$date, google[,retail.index[1]], type='l', col=col[1], ylim=google[,retail.index]%>%range(na.rm=T))
for(i in c(2:length(retail.index)))
{
  lines(google$date, google[,retail.index[i]], type='l', col=col[i])
}
legend("topleft", col=col, names(google)[retail.index], lty=1, bty='n', ncol=2, lwd=2)



retail.index <- grep(pattern = "residential", x=names(google))
plot(google$date, google[,retail.index[1]], type='l', col=col[1], ylim=google[,retail.index]%>%range(na.rm=T))
for(i in c(2:length(retail.index)))
{
  lines(google$date, google[,retail.index[i]], type='l', col=col[i])
}
legend("topleft", col=col, names(google)[retail.index], lty=1, bty='n', ncol=2, lwd=2)



saveRDS(google,  "Data/google.RDS")
Data <- left_join(Data, google, by=c('Date'='date'))





