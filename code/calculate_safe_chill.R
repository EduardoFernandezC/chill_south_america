library(stringr)

# Install the "lhmetools" package from GitHub to unrar the files

devtools::install_github("lhmet/lhmetools")


#I dont understand which files are used in the supplementary materials of the tunesia paper
#also I dont understand the files in the... (?)


# Use lhmetools::unrar to unpack the .rar file and read the historic_sim_chill as well as the observed_chill.
# This takes a while after you see the files extracted in the folder. Give it some time :)

lhmetools::unrar("data/projections.rar")

#read hitoric chill calculated on weather generator data
#setwd('southamerica_chill/chill_south_america/data/projections/hist_sim_chill/')
temp <- list.files("data/projections/", pattern = "*.csv")

# Add the path to the folder containing the data
temp <- paste0("data/projections/", temp)

# Select only the files for historic observed data
files_observed_chill <- which(stringr::str_detect(temp, "observed"))

# Select only the files for historic observed data
files_simulated_chill <- which(stringr::str_detect(temp, "ref_year"))

# Read the files with simulated chill
hist_sim_chill <- lapply(temp[files_simulated_chill], read.csv)

# Read the files with observed chill
hist_observed_chill <- lapply(temp[files_observed_chill], read.csv)


#calculate safe chill for each station and simulated year
res_simulated <- lapply(hist_sim_chill, function(x) {
  return(quantile(x$Chill_Portions, probs = 0.1)[[1]])
})


#calculate safe chill for each station across all observed years
res_observed <- lapply(hist_observed_chill, function(x) {
  return(quantile(x$Chill_Portions, probs = 0.1)[[1]])
})

#extract information from the file names to set up data frame

#ectract station name
station_names <- sapply(temp[files_simulated_chill], function(x){
  return(str_split(x, pattern = '_')[[1]][2])
})

#extract year
years <- sapply(temp[files_simulated_chill], function(x){
  intermed <- str_split(x, pattern = '_')[[1]][6]
  return(str_split(intermed,pattern = '\\.')[[1]][1])
})

#bind the historic simulated chill to one data frame
hist_sim_chill_df <- data.frame('station_name' = station_names, 'year' = years, 'safe_chill' = unlist(res_simulated),
                                row.names = NULL)

#bind the historic observed chill to one data frame
hist_obs_chill_df <- data.frame('station_name' = unique(station_names), 'year' = "observed", 'safe_chill' = unlist(res_observed),
                                row.names = NULL)

dir.create("data/safe_winter_chill")

write.csv(hist_sim_chill_df, 'data/safe_winter_chill/historic_sim_safe_chill.csv', row.names = FALSE)
write.csv(hist_obs_chill_df, 'data/safe_winter_chill/historic_obs_safe_chill.csv',row.names = FALSE)


#######climate change projection files
#same procedure as above, read all files, calculate safe chill and bind it to a data frame

#read hitoric chill calculated on weather generator data
#setwd('../../future_chill/')
temp <- list.files("data/future_chill/", pattern = "*.csv")
temp <- paste0("data/future_chill/", temp)

# Read the files
future_sim_chill <- lapply(temp, read.csv)

#calculate safe chill for each station and year
res_future_chill <- lapply(future_sim_chill, function(x){
  return(quantile(x$Chill_Portions,probs = 0.1)[[1]])
})

#extract information from the file names to set up data frame

#ectract station name
station_names <- sapply(temp, function(x){
  return(str_split(x, pattern = '_')[[1]][3])
})

#extract which climate change model was used
gcm <- sapply(temp, function(x){
  intermed <- str_split(x, pattern = '_')[[1]][7]
  return(str_split(intermed,pattern = '\\.')[[1]][1])
})

#extract year of simulation
years <- sapply(temp, function(x){
  return(str_split(x, pattern = '_')[[1]][5])
})

#extract pathway scenarion
rcp <- sapply(temp, function(x){
  return(str_split(x, pattern = '_')[[1]][4])
})

#bind the historic simulated chill to one data frame
future_sim_chill_df <- data.frame('station_name' = station_names, 'rcp' = rcp, 'gcm' = gcm, 'year' = years,
                                  'safe_chill' = unlist(res_future_chill),row.names = NULL)

#for each of the stations a pessimistic (min), intermediate min(abs(mean - median)) and optimisitc (max) projection is chosen

library(tidyverse)

future_sim_chill_df <- future_sim_chill_df %>%
  group_by(station_name,rcp,year)%>%
  summarise('pessimistic' = min(safe_chill),'intermediate' = median(safe_chill), 'optimistic' = max(safe_chill))

write.csv(future_sim_chill_df, 'data/safe_winter_chill/future_safe_chill.csv', row.names = FALSE)

# Remove the projections folder to reduce the size of the repository... If you need this again,
# please run the unrar() function

unlink("data/projections", recursive = TRUE)


######## combine all chill projections to one big file

#read the station names, future and historic chill
stations <- read.csv('data/weather_info.csv')
future_chill <- read.csv('data/safe_winter_chill/future_safe_chill.csv')
historic_chill_simu <- read.csv('data/safe_winter_chill/historic_sim_safe_chill.csv')
historic_chill_obs <- read.csv('data/safe_winter_chill/historic_obs_safe_chill.csv')

#I want for each scenario (historic ones, future projections) one column of values, but sofar the tables are melted
#--> 'de-melt' them using dcast
library(reshape2)
historic_chill_simu <- dcast(historic_chill_simu, station_name ~ year)
historic_chill_obs <- dcast(historic_chill_obs, station_name ~ year)
future_chill <- melt(future_chill, c('station_name', 'rcp', 'year'))
future_chill <- dcast(future_chill, station_name ~ rcp + year + variable)

#combine 'de-melted' tables to one big table and save it as csv
intermediate <- left_join(stations[, c("Name", "CTRY", "Latitude", "Longitude", "Elevation")],
                                 historic_chill_obs, by = c("Name" = 'station_name'))

intermediate <- left_join(intermediate, historic_chill_simu, by = c("Name" = 'station_name'))

all_projections <- left_join(intermediate, future_chill, by = c("Name" = 'station_name'))

# Fill the missing values for country in the all_projections dataframe

all_projections[98 : 152, "CTRY"] <- "CL"

all_projections[153 : 157, "CTRY"] <- "AR"

write.csv(all_projections,'data/Lars/all_chill_projections.csv', row.names = FALSE)
