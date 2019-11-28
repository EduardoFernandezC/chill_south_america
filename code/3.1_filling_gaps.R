library(chillR)
library(alternativechillfunctions)
library(dplyr)

# This script was developed before realizing that "3_filling_gaps.R" did not work really well. All the weather
# stations that were used as auxiliary source data had no data at all. Such a procedure took a lot of time and 
# therefore I will keep that data as "all_data.RData".

# The results of the script writen and ran below were stored as "fixed_WS.RData". Important note: to recreate
# these files, you need to load "all_data.RData" environment


# Set colnames in the GSOD weather stations DF for further binding of the rows with the other databases

colnames(SA_GSOD_WS_90)[which(colnames(SA_GSOD_WS_90) %in% c("STATION.NAME", "Lat", "Long", "Elev"))] <-
  c("Name", "Latitude", "Longitude", "Elevation")


# Put toghether (in a DF) all the weather stations 

All_WS_90 <- suppressWarnings(bind_rows(SA_GSOD_WS_90, CL_WS_90, AR_WS_90))


# Join the list of weather data 

All_data_90 <- c(SA_GSOD_list_90, CL_list_90, AR_list_90)


# For loop to fill gaps by patching them with weather data from close weather stations

All_patched <- list()

for (i in 1 : length(All_WS_90$Name)){
  
  # Define the point of interest (latitude and longitude of WS "i")

  my_point <- All_WS_90[i, c("Longitude", "Latitude")]


  # Compute the distance between my point (the point of the i element of the list) and the other 161
  # weather stations in the dataframe containig all the WS used in this project

  All_WS_90["distance"] <- round(sp::spDistsN1(as.matrix(All_WS_90[, c("Longitude", "Latitude")]),
                                                       c(my_point[1, 1], my_point[1, 2]), longlat = TRUE), 2)


  # Order the main dataframe (161 WS) acording the the distance between my point and the WS in the database

  All_WS_90 <- All_WS_90[order(All_WS_90$distance), ]


  # Fill the gaps using data from the 30 closest weather stations

  All_patched[[i]] <- patch_daily_temperatures2(All_data_90[[i]], All_data_90[All_WS_90[(2 : 41), "Name"]], 
                                                max_mean_bias = 4, max_stdev_bias = 4)
  
  # Order the DF start ok again
  
  All_WS_90 <- suppressWarnings(bind_rows(SA_GSOD_WS_90, CL_WS_90, AR_WS_90))}


# Check how many weather stations still have a lot of missing days


for (i in 1 : 161)
  
  print(All_patched[[i]][["statistics"]][[length(All_patched[[i]][["statistics"]])]])


# Remove the weather station number 124 (La Laguna Embalse) and 113 (El Yeso Embalse) since it has ~368 gaps
# remain after patching with data from 40 auxiliary WS.

All_patched <- All_patched[-c(113, 124)]

All_WS_90 <- All_WS_90[-c(113, 124), ]


# Fill the remaining gaps through linear interpolation

All_patched_fixed <- list()

for (i in 1 : length(All_patched))
  
  All_patched_fixed[[i]] <- fix_weather(All_patched[[i]]$weather)


# Check for WS possibly having a lot of missing days

for (i in 1 : length(All_patched_fixed))
  
  cat(i, All_patched_fixed[[i]][["QC"]][["Incomplete_days"]], "\n", "\n")

rm(i, my_point)


# Remove the WS 59 (Rodriguez Gallon) and 148 (Codpa) since they have at least one complete year missing

All_patched_fixed <- All_patched_fixed[-c(59, 148)]

All_WS_90 <- All_WS_90[!(All_WS_90$Name %in% c("RODRIGUEZ BALLON", "Codpa")), ]



