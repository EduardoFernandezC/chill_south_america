library(chillR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(alternativechillfunctions)

# List 1500 stations from the GSOD database using Quillota as central point. This may change of course, but 
# temperate trees are normally grown at southern places.

SA_GSOD_stations <- handle_gsod("list_stations", location = c(-71.2092, -32.8958), stations_to_choose_from = 1500)

# Define the borders of the countries across SA

SA_countries <- borders("world", regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname",
                                              "Colombia", "Venezuela", "Bolivia", "Ecuador", "Chile", "Paraguay",
                                              "Peru", "Guyana"),
                     fill = "white", colour = "black",
                     size = 0.3)

# Plot the map and the 1500 weather stations

ggplot() + SA_countries + theme_bw() +
  geom_point(aes(Long, Lat), data = SA_GSOD_stations, color = "black", size = 1) +
  labs(x = "Longitude", y = "Latitude") +
  theme(panel.grid = element_blank()) +
  coord_equal()

ggsave("./figures/SA_1500_stations.png", dpi = 800)

# Include only those stations starting before 1st Jan 1980 and ending after 31st Dec 2017 (n = 346)

ggplot() + SA_countries + theme_bw() +
  geom_point(aes(Long, Lat),
             data = SA_GSOD_stations[SA_GSOD_stations$BEGIN < 19800101 & SA_GSOD_stations$END > 20171231, ],
             color = "black", size = 1) +
  labs(x = "Longitude", y = "Latitude") +
  theme(panel.grid = element_blank()) +
  coord_equal()

ggsave("./figures/SA_filtered_stations.png", dpi = 800)

# Try to download the data only from those station within the period of interest and located out of the sea

SA_GSOD_stat_filt <- SA_GSOD_stations[SA_GSOD_stations$BEGIN < 19800101 & SA_GSOD_stations$END > 20171231 &
                                        SA_GSOD_stations$Lat > -60 &
                                        SA_GSOD_stations$Long > -85 & SA_GSOD_stations$Long < -38, ]


# Remove the station located at Juan Fernandez (CHILE) and Fox Bay (FALKLAND ISLANDS)

SA_GSOD_stat_filt <- SA_GSOD_stat_filt[-which(SA_GSOD_stat_filt$STATION.NAME == "JUAN FERNANDEZ" |
                                                SA_GSOD_stat_filt$STATION.NAME == "FOX BAY"), ]


# Download the data from the GSOD database

list <- NULL

for (i in 1 : length(SA_GSOD_stat_filt$chillR_code)) {
  
  data <- handle_gsod("download_weather", location = as.character(SA_GSOD_stat_filt[i, "chillR_code"]),
                      time_interval = c(1980, 2019), station_list = SA_GSOD_stat_filt)
  
  # Check if the weather station "i" has data or not. If not, it creates a blank DF to avoid errors inside the
  # for loop
  
  if (!is.list(data) || is.na(data[[1]]) || is.null(data))
    
    # If the weather station has not data, it tells to the WS dataframe that "i" has not data
    
    {SA_GSOD_stat_filt[i, "DATA"] <- "NO_DATA"
     
     data <- make_all_day_table(data.frame(Year = c(1980, 2019),
                                           Month = c(1, 12),
                                           Day = c(1, 31), 
                                           Tmin = as.numeric(NA), 
                                           Tmax = as.numeric(NA)), add.DATE = F)
     
     data["Weather_Station"] <- as.character(SA_GSOD_stat_filt[i, "STATION.NAME"])
     
     data <- data[c("Weather_Station", colnames(data)[1 : 5])] } else {
       
       # If WS "i" has data, it tells to the data frame of WS that "i" effectively has data.
       # Then, select only Tmin and Tmax columns
       
       SA_GSOD_stat_filt[i, "DATA"] <- "YES_DATA"
       
       data <- subset(data[[2]], select = c("Year", "Month", "Day", "MIN", "MAX"))
     
       colnames(data)[which(colnames(data) == "MIN")] <- "Tmin"
     
       colnames(data)[which(colnames(data) == "MAX")] <- "Tmax"
     
       data <- data.frame(Weather_Station = as.character(SA_GSOD_stat_filt[i, "STATION.NAME"]), data)}
  
  list <- c(list, list(data))
}

rm(data, i)


# Check for completeness of the data in each WS

completeness <- lapply(list2, FUN = perc_complete)

# Add the completeness data to the dataframe of weather stations

for (i in 1 : length(completeness)){
  
  SA_GSOD_stat_filt[i, "Perc_Tmin"] <- completeness[[i]][which(completeness[[i]]$variable == "Tmin"),
                                                         "percentage"]

  SA_GSOD_stat_filt[i, "Perc_Tmax"] <- completeness[[i]][which(completeness[[i]]$variable == "Tmax"),
                                                         "percentage"]}


# Include more weather stations from the chilean database by using chile_weather() function

# Set the initial parameters

path_zip_tmin <- "C:/Users/Admin/Dropbox/Doctorado/Doctorado/Chile_weathers/cr2_tasminDaily_2018_ghcn.zip"

path_zip_tmax <- "C:/Users/Admin/Dropbox/Doctorado/Doctorado/Chile_weathers/cr2_tasmaxDaily_2018_ghcn.zip"


# Call the function

CL_weather_stations <- chile_weather("info_stations", Initial_Date = "1980-01-01", End_Date = "2017-12-31",
                                     latitude = -32.8958, longitude = -71.2092,
                                     path_zip_tmin = path_zip_tmin, path_zip_tmax = path_zip_tmax,
                                     Number_of_stations = 909)


# Plot the weather stations from both databases having more than 90% of data complete

ggplot() + SA_countries + theme_bw() +
  geom_jitter(aes(Long, Lat), data = SA_GSOD_stat_filt[SA_GSOD_stat_filt$Perc_Tmin > 90 &
                                                        SA_GSOD_stat_filt$Perc_Tmin > 90, ],
             color = "black", size = 1) +
  geom_jitter(aes(Longitude, Latitude), data = CL_weather_stations[CL_weather_stations$Perc_days_complete > 90 &
                                                                     CL_weather_stations$Longitude > -78 &
                                                                     CL_weather_stations$Longitude < -38 &
                                                                     CL_weather_stations$Latitude > -56, ],
             color = "red", size = 1) +
  labs(x = "Longitude", y = "Latitude") +
  theme(panel.grid = element_blank()) +
  coord_equal(xlim = c(-30, -85))

ggsave("./figures/SA_90P_complete_stats.png", dpi = 800)






















rm(daily_data, data, dfs, info, list2, path_tmax, path_tmin, primer_data, Sorted_Stations, Stations_Tmax,
   Stations_Tmin, Sumarized_stations, Tmax, Tmin, x, actual_WD, Cod_stations_Tmax, Cod_stations_Tmin,
   End_Date, Fecha_Final, Fecha_Inicial, i, Initial_Date, latitud, longitud, mypoint, variables, count_NAs)



my_weather<-chile_weather("my_data",Initial_Date = Fecha_Inicial,End_Date = Fecha_Final,latitude = latitud,
                          longitude = longitud,path_zip_tmin = path_zip_tmin,path_zip_tmax = path_zip_tmax)
list<-chile_weather("station_list",Initial_Date = Fecha_Inicial,End_Date = Fecha_Final,latitude = latitud,
                    longitude = longitud,path_zip_tmin = path_zip_tmin,path_zip_tmax = path_zip_tmax)






setwd("C:/Users/Admin/Dropbox/Doctorado/Doctorado/chill_south_america")

