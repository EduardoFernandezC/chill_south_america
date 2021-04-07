#this script reads the climate stations chill files and calculates the safe chill (0.1 quantile) for each year and station
#furthermore summarizes future chill to pessimistic, intermediate and optimistic chill predictions
#result is a table, each row for a climate station, each column for a year / future scenario chill value


library(stringr)


#read hitoric chill calculated on weather generator data
setwd('southamerica_chill/chill_south_america/data/projections/hist_sim_chill/')
temp = list.files(pattern="*.csv")
hist_sim_chill = lapply(temp, read.csv)

#calculate safe chill for each station and year
res <- lapply(hist_sim_chill, function(x){
  return(quantile(x$Chill_Portions,probs = 0.1)[[1]])
})

#extract information from the file names to set up data frame

#ectract station name
station_names <- sapply(temp, function(x){
  return(str_split(x, pattern = '_')[[1]][2])
})

#extract year
years <- sapply(temp, function(x){
  intermed <- str_split(x, pattern = '_')[[1]][6]
  return(str_split(intermed,pattern = '\\.')[[1]][1])
})

#bind the historic simulated chill to one data frame
hist_sim_chill <- data.frame('station_name' = station_names, 'year' = years, 'safe_chill' = unlist(res),row.names = NULL)

write.csv(hist_sim_chill, '../../historic_sim_safe_chill.csv',row.names = FALSE)


#######climate change projection files
#same procedure as above, read all files, calculate safe chill and bind it to a data frame

#read hitoric chill calculated on weather generator data
setwd('../../future_chill/')
temp = list.files(pattern="*.csv")
future_sim_chill = lapply(temp, read.csv)

#calculate safe chill for each station and year
res <- lapply(future_sim_chill, function(x){
  return(quantile(x$Chill_Portions,probs = 0.1)[[1]])
})

#extract information from the file names to set up data frame

#ectract station name
station_names <- sapply(temp, function(x){
  return(str_split(x, pattern = '_')[[1]][2])
})

#extract which climate change model was used
gcm <- sapply(temp, function(x){
  intermed <- str_split(x, pattern = '_')[[1]][6]
  return(str_split(intermed,pattern = '\\.')[[1]][1])
})

#extract year of simulation
years <- sapply(temp, function(x){
  return(str_split(x, pattern = '_')[[1]][4])
})

#extract pathway scenarion
rcp <- sapply(temp, function(x){
  return(str_split(x, pattern = '_')[[1]][3])
})

#bind the historic simulated chill to one data frame
future_sim_chill <- data.frame('station_name' = station_names, 'rcp' = rcp, 'gcm' = gcm, 'year' = years, 'safe_chill' = unlist(res),row.names = NULL)

#for each of the stations a pessimistic (min), intermediate min(abs(mean - median)) and optimisitc (max) projection is chosen

library(tidyverse)

perc_level <- 0.15

future_sim_chill <- future_sim_chill %>%
  group_by(station_name,rcp,year)%>%
  summarise('pessimistic' = quantile(safe_chill,perc_level),'intermediate' = median(safe_chill), 'optimistic' = quantile(safe_chill,1-perc_level))

library(reshape2)
stations <- read.csv('../../data/all_chill_projections.csv')
test_melt <- melt(test_df,id.vars = c('station_name','rcp','year'))

write.csv(future_sim_chill, '../future_safe_chill.csv',row.names = FALSE)



######## combine all chill projections to one big file

#read the station names, future and historic chill
stations <- read.csv('southamerica_chill/chill_south_america/data/weather_stations.csv')
future_chill <- read.csv('../future_safe_chill.csv')
historic_chill <- read.csv('southamerica_chill/chill_south_america/data/historic_sim_safe_chill.csv')

#I want for each scenario (historic ones, future projections) one column of values, but sofar the tables are melted
#--> 'de-melt' them using dcast
library(reshape2)
historic_chill <- dcast(historic_chill,station_name ~ year)
future_chill <- melt(future_chill,c('station_name', 'rcp', 'year'))
future_chill <- dcast(future_chill, station_name ~ rcp + year + variable)

#test <- stations[,1:2]
#test2 <- merge.data.frame(test, future_chill, by = 'Name')
#stations[,17:28] <- test2[,3:14]

#combine 'de-melted' tables to one big table and save it as csv
intermediate <- merge.data.frame(stations, historic_chill, by = 'station_name')
all_proejctions <- merge.data.frame(intermediate, future_chill, by = 'station_name')

write.csv(all_proejctions,'../all_chill_projections.csv', row.names = FALSE)
