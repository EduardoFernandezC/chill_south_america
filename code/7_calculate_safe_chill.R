#this script reads the climate stations chill files and calculates the safe chill (0.1 quantile) for each year and station
#furthermore summarizes future chill to pessimistic, intermediate and optimistic chill predictions
#result is a table, each row for a climate station, each column for a year / future scenario chill value

#this a change


library(stringr)


#read hitoric chill calculated on weather generator data
setwd('data/projections/hist_sim_chill/')
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

#summarize the future chill to optimisitic, intermediate and pessimisitic safe chill output per station, year and rcp

#set the perecentile for the pessimisitc output, optimisitc is 1-perc_level and intermediate is always the median (=0.5)
perc_level <- 0.15

future_sim_chill <- future_sim_chill %>%
  group_by(station_name,rcp,year)%>%
  summarise('pessimistic' = quantile(safe_chill,perc_level),'intermediate' = median(safe_chill), 'optimistic' = quantile(safe_chill,1-perc_level))


######## combine all chill projections to one big file

#read the station names
stations <- read.csv('../weather_stations.csv')

#in each columns should be the chill values of a specific year for all the stations --> I need to "de-melt" them
#use function dcast for this

library(reshape2)
hist_sim_chill <- dcast(hist_sim_chill,station_name ~ year)

#melt table so model output is specified in one column (optimisitic, pessimisitic...) we have
future_sim_chill <- melt(future_sim_chill,c('station_name', 'rcp', 'year'))
future_sim_chill <- dcast(future_sim_chill, station_name ~ rcp + year + variable)


#combine 'de-melted' tables to one big table and save it as csv
intermediate <- merge.data.frame(stations, hist_sim_chill, by = 'station_name')
all_proejctions <- merge.data.frame(intermediate, future_sim_chill, by = 'station_name')

write.csv(all_proejctions,'../all_chill_projections.csv', row.names = FALSE)
