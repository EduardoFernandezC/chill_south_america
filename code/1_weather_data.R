library(chillR)
library(alternativechillfunctions)

# Look for weather stations ====

# List 1500 stations from the GSOD database using Quillota as central point. This may change of course, but 
# temperate trees are normally grown at southern places.

SA_GSOD_Weat_Stat <- handle_gsod("list_stations", location = c(-71.2092, -32.8958, 130),
                                 stations_to_choose_from = 1500)


# Try to download the data only from those station within the period of interest and located out of the sea

SA_GSOD_WS_80s_17s <- SA_GSOD_Weat_Stat[SA_GSOD_Weat_Stat$BEGIN < 19800101 & SA_GSOD_Weat_Stat$END > 20171231 &
                                          SA_GSOD_Weat_Stat$Lat > -60 &
                                          SA_GSOD_Weat_Stat$Long > -85 & SA_GSOD_Weat_Stat$Long < -38, ]


# Remove the station located at Juan Fernandez (CHILE) and Fox Bay (FALKLAND ISLANDS)

SA_GSOD_WS_80s_17s <- SA_GSOD_WS_80s_17s[-which(SA_GSOD_WS_80s_17s$STATION.NAME == "JUAN FERNANDEZ" |
                                                  SA_GSOD_WS_80s_17s$STATION.NAME == "FOX BAY"), ]


# Check for duplicated station names. Normally, they have a different chillR_code and have different data.
# This is very important in case we want to patch data from weather stations located very close.
# Add a "2" for the duplicated name

SA_GSOD_WS_80s_17s$STATION.NAME <- as.character(SA_GSOD_WS_80s_17s$STATION.NAME)

SA_GSOD_WS_80s_17s[which(duplicated(SA_GSOD_WS_80s_17s$STATION.NAME)), "STATION.NAME"] <-
  paste(SA_GSOD_WS_80s_17s[which(duplicated(SA_GSOD_WS_80s_17s$STATION.NAME)), "STATION.NAME"], "2")


# Download the data from the GSOD database ====


#SA_GSOD_list <- NULL

#for (i in 1 : length(SA_GSOD_WS_80s_17s$chillR_code)) {
  
  #data <- handle_gsod("download_weather", location = as.character(SA_GSOD_WS_80s_17s[i, "chillR_code"]),
                      #time_interval = c(1980, 2017), station_list = SA_GSOD_WS_80s_17s)
  
  # Check if the weather station "i" has data or not. If not, it creates a blank DF to avoid errors inside the
  # for loop
  
  #if (!is.list(data) || is.na(data[[1]]) || is.null(data))
    
    # If the weather station has not data, it tells to the WS dataframe that "i" has not data
    
    #{SA_GSOD_WS_80s_17s[i, "DATA"] <- "NO_DATA"
     
     #data <- make_all_day_table(data.frame(Year = c(1980, 2017),
                                           #Month = c(1, 12),
                                           #Day = c(1, 31), 
                                           #Tmin = as.numeric(NA), 
                                           #Tmax = as.numeric(NA)), add.DATE = F)
     
     #data["Weather_Station"] <- as.character(SA_GSOD_WS_80s_17s[i, "STATION.NAME"])
     
     #data <- data[c("Weather_Station", colnames(data)[1 : 5])] } else {
       
       # If WS "i" has data, it tells to the data frame of WS that "i" effectively has data.
       # Then, select only Tmin and Tmax columns
       
       #SA_GSOD_WS_80s_17s[i, "DATA"] <- "YES_DATA"
       
       #data <- subset(data[[2]], select = c("Year", "Month", "Day", "MIN", "MAX"))
     
       #colnames(data)[which(colnames(data) == "MIN")] <- "Tmin"
     
       #colnames(data)[which(colnames(data) == "MAX")] <- "Tmax"
     
       #data <- data.frame(Weather_Station = as.character(SA_GSOD_WS_80s_17s[i, "STATION.NAME"]), data)}
  
  #SA_GSOD_list <- c(SA_GSOD_list, list(data))
#}

#rm(data, i)



# Runnig the loop above took about three days and there is not need to run it again since all the data
# downloaded is now in "SA_GSOD_list"...



# Check for completeness of the data in each WS ====


completeness <- lapply(SA_GSOD_list, FUN = perc_complete)

# Add the completeness data to the dataframe of weather stations

for (i in 1 : length(completeness)){
  
  SA_GSOD_WS_80s_17s[i, "Perc_Tmin"] <- completeness[[i]][which(completeness[[i]]$variable == "Tmin"),
                                                         "percentage"]

  SA_GSOD_WS_80s_17s[i, "Perc_Tmax"] <- completeness[[i]][which(completeness[[i]]$variable == "Tmax"),
                                                         "percentage"]}

rm(completeness, i)

# Select only WS having 90% or more data complete

SA_GSOD_WS_90 <- SA_GSOD_WS_80s_17s[SA_GSOD_WS_80s_17s$Perc_Tmin >= 90 &
                                      SA_GSOD_WS_80s_17s$Perc_Tmin >= 90, ]


# Change the name of the elements of the main data list for further subsetting

names(SA_GSOD_list) <- as.character(SA_GSOD_WS_80s_17s$STATION.NAME)


# Select only the dataframes having more than 90% of data complete from the GSOD database

SA_GSOD_list_90 <- SA_GSOD_list[which(names(SA_GSOD_list) %in% as.character(SA_GSOD_WS_90$STATION.NAME))]





# Check for WS in chilean database ====

# Include more weather stations from the chilean database by using chile_weather() function

# Set the initial parameters

path_zip_tmin <- "C:/Users/Admin/Dropbox/Doctorado/Doctorado/Chile_weathers/cr2_tasminDaily_2018_ghcn.zip"

path_zip_tmax <- "C:/Users/Admin/Dropbox/Doctorado/Doctorado/Chile_weathers/cr2_tasmaxDaily_2018_ghcn.zip"

# Call the function

CL_Weat_Stat <- chile_weather("info_stations", Initial_Date = "1980-01-01", End_Date = "2017-12-31",
                                     latitude = -32.8958, longitude = -71.2092,
                                     path_zip_tmin = path_zip_tmin, path_zip_tmax = path_zip_tmax,
                                     Number_of_stations = 909)


# Select only weather stations having more than 90% of data complete

CL_WS_90 <- CL_Weat_Stat[CL_Weat_Stat$Perc_days_complete >= 90 &
                           CL_Weat_Stat$Longitude > -78 &
                           CL_Weat_Stat$Longitude < -38 &
                           CL_Weat_Stat$Latitude > -56, ]


# Get the weather data from the chilean database only for those stations having more than 90% of data complete

CL_list_90 <- list(chile_weather("my_data", Initial_Date = "1980-01-01", End_Date = "2017-12-31",
                                 latitude = -32.8958, longitude = -71.2092,
                                 path_zip_tmin = path_zip_tmin, path_zip_tmax = path_zip_tmax))

CL_list_90 <- c(CL_list_90, chile_weather("station_list_data", Initial_Date = "1980-01-01",
                                          End_Date = "2017-12-31",
                                          latitude = -32.8958, longitude = -71.2092,
                                          path_zip_tmin = path_zip_tmin, path_zip_tmax = path_zip_tmax,
                                          stations_df = CL_WS_90))

rm(path_zip_tmax, path_zip_tmin)


# Check for duplicated weather station names

CL_WS_90$Name <- as.character(CL_WS_90$Name)

CL_WS_90[which(duplicated(CL_WS_90$Name)), "Name"] <- paste(CL_WS_90[which(duplicated(CL_WS_90$Name)),
                                                                     "Name"], "2")

# Change the name of the elements inside the list (Punta Arenas is repeated but I will keep since appearently
# they belog to different organization)

names(CL_list_90) <- as.character(CL_WS_90$Name)




# Get some data from Argentina ====

# This data were obtained from the patagonia project

AR_list <- list()

for (i in list.files("./data/Argentina/")){
  
  data <- data.frame(Weather_Station = substr(i, 1, nchar(i) - 4),
                     read.csv(paste("./data/Argentina/", i, sep = ""), sep = ";",
                              header = T)[, c("Year", "Month", "Day", "Tmin", "Tmax")])
  
  data <- data[data$Year %in% c(1980 : 2017), ]
  
  AR_list <- c(AR_list, list(data))}

rm(i, data)

# Change the names of the element of the list

names(AR_list) <- substr(list.files("./data/Argentina/"), 1, nchar(list.files("./data/Argentina/")) - 4)

# Make a DF of the weather stations from the patagonia project

AR_Weat_Stat <- data.frame(Name = substr(list.files("./data/Argentina/"), 1, nchar(list.files("./data/Argentina/")) - 4),
                    Latitude = c(-39.38, -41.97, -41.25, -38.95, -43.87, -39.02, -43.23, -40.85),
                    Longitude = c(-62.62, -71.50, -68.73, -67.97, -69.03, -64.08, -65.32, -63.02),
                    Elevation = c(22, 337, 887, 271, 590, 59, 10, 7),
                    Perc_Tmax = as.numeric(NA), 
                    Perc_Tmin = as.numeric(NA))


# Compute the completeness for the data

completeness_AR <- lapply(AR_list, perc_complete)


# Add the completeness data to the dataframe of weather stations

for (i in 1 : length(completeness_AR)){
  
  AR_Weat_Stat[i, "Perc_Tmin"] <- completeness_AR[[i]][which(completeness_AR[[i]]$variable == "Tmin"),
                                                          "percentage"]
  
  AR_Weat_Stat[i, "Perc_Tmax"] <- completeness_AR[[i]][which(completeness_AR[[i]]$variable == "Tmax"),
                                                          "percentage"]}

rm(completeness_AR, i)


# Keep only weather stations having more than 90% of records complete

AR_WS_90 <- AR_Weat_Stat[AR_Weat_Stat$Perc_Tmax >= 90 & AR_Weat_Stat$Perc_Tmin >= 90, ]


# Extract the data from the WS having more than 90% of records complete

AR_list_90 <- AR_list[which(unique(names(AR_list)) %in% as.character(AR_WS_90$Name))]


















