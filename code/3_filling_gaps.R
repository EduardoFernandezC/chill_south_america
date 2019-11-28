library(chillR)
library(alternativechillfunctions)

# This script is to fill the gaps in the main weather stations

# What follows is a long for loop that creates a list containing weather data for the ~25 closest weather stations
# to "i"


# The output is a list of lists

SA_GSOD_WS_90_aux_data <- list()


for (i in  79 : length(SA_GSOD_WS_90$chillR_code)){
  
  # Extract the latitude and longitude of the i element of the list that contains weather data
  # for places with 90% of data complete between 1980 - 2017
  
  my_point <- SA_GSOD_Weat_Stat[which(SA_GSOD_Weat_Stat$chillR_code == SA_GSOD_WS_90[i, "chillR_code"]),
                                c("Long", "Lat")]
  
  # Compute the distance between my point (the point of the i element of the list) and the other 1500
  # weather stations in the GSOD database
  
  SA_GSOD_Weat_Stat["distance"] <- round(sp::spDistsN1(as.matrix(SA_GSOD_Weat_Stat[, c("Long", "Lat")]),
                                                       c(my_point[1, 1], my_point[1, 2]), longlat = TRUE), 2)
  
  # Order the main dataframe (1500 WS) acording the the distance between my point and the WS in the GSOD database
  
  SA_GSOD_Weat_Stat <- SA_GSOD_Weat_Stat[order(SA_GSOD_Weat_Stat$distance), ]
  
  # Change the class of the vector STATION.NAME to character for further subsetting
  
  SA_GSOD_Weat_Stat$STATION.NAME <- as.character(SA_GSOD_Weat_Stat$STATION.NAME)
  
  # Select again the stations starting and ending within the period of interest. Remove those stations located
  # in the sea
  
  SA_GSOD_WS_80s_17s <- SA_GSOD_Weat_Stat[SA_GSOD_Weat_Stat$BEGIN < 19800101 & SA_GSOD_Weat_Stat$END > 20171231 &
                                            SA_GSOD_Weat_Stat$Lat > -60 &
                                            SA_GSOD_Weat_Stat$Long > -85 & SA_GSOD_Weat_Stat$Long < -38, ]
  
  # Remove the station located at Juan Fernandez (CHILE) and Fox Bay (FALKLAND ISLANDS)
  
  SA_GSOD_WS_80s_17s <- SA_GSOD_WS_80s_17s[-which(SA_GSOD_WS_80s_17s$STATION.NAME == "JUAN FERNANDEZ" |
                                                    SA_GSOD_WS_80s_17s$STATION.NAME == "FOX BAY"), ]
  
  # Select the 25 closest weather stations as auxiliary source of data for the WS[[i]] avoiding the WS 1 for which
  # weather data has been already downloaded
  
  SA_GSOD_alt_WS <- SA_GSOD_Weat_Stat[c(2 : 26), ]
  
  # Subset WS ending and starting during the period of interest 1980 - 2017
  
  SA_GSOD_alt_WS  <- SA_GSOD_alt_WS[SA_GSOD_alt_WS$BEGIN < 20180101 & SA_GSOD_alt_WS$END > 19800101, ]
  
  
  # For loop to create a list of X weather stations with weather data to patch the gaps in the weather station
  # "i"
  
  aux_data_list <- list()
  
  for (j in 1 : length(SA_GSOD_alt_WS$chillR_code)){
    
    # Check is the weather data has been already downloaded in a previous script "1_weather_data.R". If so,
    # it takes that data an located within the auxiliary data list for weather station "i"
    
    if (SA_GSOD_alt_WS[j, "chillR_code"] %in% SA_GSOD_WS_80s_17s$chillR_code){
      
      aux_data_list[j] <- SA_GSOD_list[SA_GSOD_alt_WS[j, "STATION.NAME"]]} else {
        
        # If the data of the weather station has not been already downloaded. This starts a step to do it
        
        if (!(SA_GSOD_alt_WS[j, "chillR_code"] %in% SA_GSOD_WS_80s_17s$chillR_code)){
          
          # Set the start and the end of the period of interest if the weather data start after 1980...
          
          if (SA_GSOD_alt_WS[j, "BEGIN"] >= 19800101){
            
            start <- as.numeric(substr(SA_GSOD_alt_WS[j, "BEGIN"], 1, 4))} else {start <- 1980}
          
          # ... or if WS ends before 2017
          
          if (SA_GSOD_alt_WS[j, "END"] < 20171231){
            
            end <- as.numeric(substr(SA_GSOD_alt_WS[j, "END"], 1, 4))} else {end <- 2017}
          
          # Download the weather data
          
          aux_data <- handle_gsod("download_weather", location = as.character(SA_GSOD_alt_WS[j, "chillR_code"]),
                                  time_interval = c(start, end), station_list = SA_GSOD_alt_WS)
          
          # It creates a blank dataframe if the step above resulted in no weather data (NA)
          
          if (!is.list(aux_data) || is.na(aux_data[[1]]) || is.null(aux_data)){
            
            # Create the df
            
            aux_data <- make_all_day_table(data.frame(Year = c(start, end),
                                                      Month = c(1, 12),
                                                      Day = c(1, 31),
                                                      Tmin = as.numeric(NA),
                                                      Tmax = as.numeric(NA)), add.DATE = F)
            
            aux_data["Weather_Station"] <- as.character(SA_GSOD_alt_WS[j, "STATION.NAME"])
            
            aux_data_list[[j]] <- aux_data[c("Weather_Station", colnames(aux_data)[1 : 5])]} else {
              
            
              # If the downloading option resulted in some records, this only selects the important
              # columns
                
              aux_data <- subset(aux_data[[2]], select = c("Year", "Month", "Day", "MIN", "MAX"))
              
              colnames(aux_data)[which(colnames(aux_data) == "MIN")] <- "Tmin"
              
              colnames(aux_data)[which(colnames(aux_data) == "MAX")] <- "Tmax"
              
              aux_data_list[[j]] <- data.frame(Weather_Station = as.character(SA_GSOD_alt_WS[j, "STATION.NAME"]),
                                               aux_data)}}}}
  
  # The auxiliary weather data of the station "i" is the list with weather data for about 18 WS downloaded in the
  # step above
  
  SA_GSOD_WS_90_aux_data[[i]] <- aux_data_list}
  
rm(aux_data, aux_data_list, my_point, SA_GSOD_alt_WS, i, end, j, start)
 



# Patch missing days 
  
SA_GSOD_90_patched <- list()

for (i in 1 : length(SA_GSOD_list_90)){
  
  SA_GSOD_90_patched[[i]] <- patch_daily_temperatures2(SA_GSOD_list_90[[i]], SA_GSOD_WS_90_aux_data[[i]],
                                                      max_mean_bias = 5, max_stdev_bias = 5)}






for (i in 1 : 98)
  
  print(SA_GSOD_90_patched[[i]][["statistics"]][[length(SA_GSOD_90_patched[[i]][["statistics"]])]])


tempResponse_daily_list(CL_list_90[[2]], latitude = CL_WS_90[2, "Latitude"],
                        Start_JDay = 121, End_JDay = 243)



