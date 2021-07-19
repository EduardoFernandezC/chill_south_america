library(chillR)
library(dormancyR)
library(dplyr)

# This script was developed before realizing that "3_filling_gaps.R" did not work really well. All the weather
# stations that were used as auxiliary source data had no data at all. Such a procedure took a lot of time and 
# therefore I will keep that data as "all_data.RData".

# The results of the script writen and ran below were stored as "fixed_WS.RData". Important note: to recreate
# these files, you need to load "all_data.RData" environment


# Set colnames in the GSOD weather stations DF for further binding of the rows with the other databases

colnames(SA_GSOD_WS_90)[which(colnames(SA_GSOD_WS_90) %in% c("STATION.NAME", "Lat", "Long", "Elev"))] <-
  c("Name", "Latitude", "Longitude", "Elevation")


colnames(CL_WS_90)[which(colnames(CL_WS_90) %in% c("Altitude", "Distance"))] <- c("Elevation", "distance")


colnames(AR_WS_90)[which(colnames(AR_WS_90) %in% c("Station_name"))] <- "Name"


# Put toghether (in a DF) all the weather stations 

All_WS_90 <- suppressWarnings(bind_rows(SA_GSOD_WS_90, CL_WS_90, AR_WS_90))


# Join the list of weather data 

All_data_90 <- c(SA_GSOD_list_90, CL_list_90, AR_list_90)


# For loop to fill gaps by patching them with weather data from close weather stations

All_patched_month <- list()

for (i in 1 : length(All_WS_90$Name)){
  
  # Define the point of interest (latitude and longitude of WS "i")

  my_point <- All_WS_90[i, c("Longitude", "Latitude")]


  # Compute the distance between my point (the point of the i element of the list) and the other 161
  # weather stations in the dataframe containig all the WS used in this project

  All_WS_90["distance"] <- round(sp::spDistsN1(as.matrix(All_WS_90[, c("Longitude", "Latitude")]),
                                                       c(my_point[1, 1], my_point[1, 2]), longlat = TRUE), 2)


  # Order the main dataframe (161 WS) acording the the distance between my point and the WS in the database

  All_WS_90 <- All_WS_90[order(All_WS_90$distance), ]


  # Fill the gaps using data from the 40 closest weather stations

  All_patched_month[[i]] <- patch_daily_temps(All_data_90[[i]], All_data_90[All_WS_90[(2 : 41), "Name"]], 
                                              max_mean_bias = 4, max_stdev_bias = 4, time_interval = "month")
  
  
  # Order the df to start again
  
  All_WS_90 <- suppressWarnings(bind_rows(SA_GSOD_WS_90, CL_WS_90, AR_WS_90))
  
  
}



# Check how many weather stations still have a lot of missing days


for (i in 1 : 161){
  
  tmin <- sum(All_patched_month[[i]][["statistics"]][["Tmin"]][[length(All_patched_month[[i]][["statistics"]][["Tmin"]])]][5 : 8, "Gaps_remain"])
  
  tmax <- sum(All_patched_month[[i]][["statistics"]][["Tmax"]][[length(All_patched_month[[i]][["statistics"]][["Tmax"]])]][5 : 8, "Gaps_remain"])
  
  cat(i, ":", "Tmin", tmin, "Tmax", tmax, "\n")

}


# Some stations have a lot of missing days. I will retain those to be removed in the next step in case the fixing
# does not work for others


# Fill the remaining gaps through linear interpolation

All_patched_fixed <- list()

for (i in 1 : length(All_patched_month))
  
  All_patched_fixed[[i]] <- fix_weather(All_patched_month[[i]]$weather)


# Check for WS possibly having a lot of missing days

for (i in 1 : length(All_patched_fixed))
  
  cat(i, All_patched_fixed[[i]][["QC"]][["Incomplete_days"]], "\n", "\n")

rm(i, my_point)


# Remove the WS 134, 125, 124, 121, 116, 113 since all them show many continuous missing days within the dormant
# period

All_patched_fixed_final <- All_patched_fixed[-c(134, 125, 124, 121, 116, 113)]


All_WS_90_final <- All_WS_90[-c(134, 125, 124, 121, 116, 113), ]


names(All_patched_fixed_final) <- All_WS_90_final$Name


# Save both the final weather station data frame and the weather data

dir.create("fixed_temps")

write.csv(All_WS_90_final, "weather_stations_final.csv", row.names = FALSE)

save_temperature_scenarios(lapply(All_patched_fixed_final, function (x) x[["weather"]]),
                           path = "fixed_temps/",
                           prefix = "patched_fixed")








