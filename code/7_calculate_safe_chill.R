#this script reads the climate stations chill files and calculates the safe chill (0.1 quantile) for each year and station
#furthermore summarizes future chill to pessimistic, intermediate and optimistic chill predictions
#result is a table, each row for a climate station, each column for a year / future scenario chill value

library(tidyverse)
library(stringr)
library(reshape2)

#read historic chill calculated on weather generator data
#setwd('data/projections/hist_sim_chill/')
path <- 'data/projections/hist_sim_chill/'
temp <- list.files(path, pattern = "*.csv")
hist_sim_chill <- lapply(temp, function (x) read.csv(paste0(path, x)))

#calculate safe chill for each station and year
res <- lapply(hist_sim_chill, function(x){
  return(quantile(x$Chill_Portions, probs = 0.1)[[1]])
})

#extract information from the file names to set up data frame

#extract station name
station_names <- sapply(temp, function(x){
  return(str_split(x, pattern = '_')[[1]][2])
})

#extract year
years <- sapply(temp, function(x){
  intermed <- str_split(x, pattern = '_')[[1]][6]
  return(str_split(intermed, pattern = '\\.')[[1]][1])
})

#bind the historic simulated chill to one data frame of safe winter chill for historic scenarios
hist_sim_chill <- data.frame('station_name' = station_names, 'year' = years, 'safe_chill' = unlist(res),
                             row.names = NULL)

#######climate change projection files
#same procedure as above, read all files, calculate safe chill and bind it to a data frame

#read hitoric chill calculated on weather generator data
#setwd('../../future_chill/')
path <- 'data/future_chill/'
temp <- list.files(path, pattern = "*.csv")
future_sim_chill <- lapply(temp, function (x) read.csv(paste0(path, x)))

#calculate safe chill for each station and year
res <- lapply(future_sim_chill, function(x){
  return(quantile(x$Chill_Portions,probs = 0.1)[[1]])
})

#extract information from the file names to set up data frame

#exctract station name
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
future_sim_chill <- data.frame('station_name' = station_names, 'rcp' = rcp, 'gcm' = gcm,
                               'year' = years, 'safe_chill' = unlist(res), 
                               row.names = NULL)

# Pessimistic, moderate and optimistic scenarios
#for each of the stations a pessimistic (min), intermediate min(abs(mean - median)) and optimistic (max) 
#projection is chosen

#summarize the future chill to optimistic, intermediate and pessimistic safe chill output per station, year and rcp

#set the percentile for the pessimistic output, optimistic is 1-perc_level and intermediate is always the median (= 0.5)
perc_level <- 0.15

future_sim_chill <- future_sim_chill %>%
  group_by(station_name, rcp, year) %>%
  summarise('pessimistic' = quantile(safe_chill, perc_level),
            'intermediate' = median(safe_chill),
            'optimistic' = quantile(safe_chill, 1-perc_level))


# Add the actual observed safe winter chill to the table
#read historic chill calculated on weather station data

path <- 'data/projections/observed/'
temp <- list.files(path, pattern = "*.csv")
hist_obs_chill <- lapply(temp, function (x) read.csv(paste0(path, x)))

#calculate safe chill for each station and year
res <- lapply(hist_obs_chill, function(x){
  return(quantile(x$Chill_Portions, probs = 0.1)[[1]])
})

#extract information from the file names to set up data frame

#extract station name
station_names <- sapply(temp, function(x){
  return(str_split(x, pattern = '_')[[1]][2])
})

#bind the historic observed chill to one data frame of safe winter chill for historic scenarios
hist_obs_chill <- data.frame('station_name' = station_names, 'observed_SWC' = unlist(res),
                             row.names = NULL)

######## combine all chill projections to one big file

#read the station names
stations <- read.csv('data/weather_stations.csv')

# Warning here! The stations data frame still shows two 'Punta Arenas'. This will fix the issue by adding a '2' to the
# second one

stations[stations$station_name == 'Punta Arenas', "station_name"] <- c("Punta Arenas", "Punta Arenas 2")

#in each columns should be the chill values of a specific year for all the stations --> I need to "de-melt" them
#use function dcast from reshape2 for this

hist_sim_chill <- dcast(hist_sim_chill,station_name ~ year)

#melt table so model output is specified in one column (optimisitic, pessimisitic...) we have
future_sim_chill <- melt(future_sim_chill,c('station_name', 'rcp', 'year'))
future_sim_chill <- dcast(future_sim_chill, station_name ~ rcp + year + variable)


#combine 'de-melted' tables to one big table and save it as csv
intermediate <- merge.data.frame(stations, hist_obs_chill, by = 'station_name')
intermediate <- merge.data.frame(intermediate, hist_sim_chill, by = 'station_name')
all_projections <- merge.data.frame(intermediate, future_sim_chill, by = 'station_name')

#save the chill values to one table. this will be the key for all further scripts
write.csv(all_projections,'data/all_chill_projections.csv', row.names = FALSE)
