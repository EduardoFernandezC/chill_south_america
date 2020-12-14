library(chillR)


# Extract only the weather data from the list after patching and fixing ====

weather_data <- list()

for (i in 1 : length(All_patched_fixed)){
  
  weather_data[[i]] <- All_patched_fixed[[i]][["weather"]]
}


# Change the name of the elements of the list

names(weather_data) <- All_WS_90$Name

# Change the name of the dataframe containing information about the weather stations

weather_info <- All_WS_90

# Save the information on the weather stations

write.csv(weather_info, "data/weather_info.csv", row.names = FALSE)

# Clean the global environment

rm(All_data_90, All_patched, All_patched_fixed, All_WS_90, i)


# Compute the observed responses ====

Start_JDay <- 121
End_JDay <- 243

dir.create("./data/projections/observed")

chill_observed <- list()

for (i in 1 : length(weather_data)){
  
  
  chill_observed[[i]] <- tempResponse_daily_list(weather_data[[i]], latitude = weather_info[i, "Latitude"],
                                          Start_JDay = Start_JDay, End_JDay = End_JDay, misstolerance = 7)[[1]]
  
  write.csv(chill_observed[[i]], paste("./data/projections/observed", i, "_", weather_info[i, "Name"],
                                       "_observed_chill.csv", sep = ""), row.names = FALSE)
}



#make_chill_plot(chill_observed[[98]],"Chill_Portions",metriclabel="Annual chill (Chill Portions)")




# Simulate historic temperature scenarios from records ====


historic_weather_scenarios <- list()

for(i in 1 : length(weather_data)){
  
  historic_temperature_scenarios <- temperature_scenario_from_records(weather = weather_data[[i]],
                                                                      year = c(1981, 1985, 1989, 1993, 1997, 2001,
                                                                               2005, 2009, 2013, 2017))
  
  
  historic_weather_scenarios[[i]] <- temperature_generation(weather = weather_data[[i]],
                                                            year = c(1981, 1985, 1989, 1993, 1997, 2001,
                                                                     2005, 2009, 2013, 2017),
                                                            sim_years = c(2000, 2100),
                                                            temperature_scenario = historic_temperature_scenarios)
  
  save_temperature_scenarios(historic_weather_scenarios[[i]], "./data/projections/historic_simulated_temps", 
                             paste(i, weather_info[i, "Name"], sep = "_"))
}


rm(historic_temperature_scenarios)

# Estimate the responses for the historic simulated scenarios ====

hist_sim_chill <- list()

for (i in 3 : length(historic_weather_scenarios)){
  
  hist_sim_chill[[i]] <- tempResponse_daily_list(historic_weather_scenarios[[i]],
                                            latitude = weather_info[i, "Latitude"],
                                            Start_JDay = Start_JDay, End_JDay = End_JDay)
  
  save_temperature_scenarios(hist_sim_chill[[i]], "./data/projections/hist_sim_chill",
                             paste(i, weather_info[i, "Name"], "ref_year", sep = "_"))
}


# Compress the folder projections (/hist_sim_chill, /historic_simulated_temps, /observed) ====

zip(zipfile = "./data/projections", files = "./data/projections", flags = " a -idq -ep1",
    zip = "C:/Program Files/WinRAR/WinRAR")


# Remove the folder

unlink("./data/projections", recursive = T)


# Get the climate wizard data and simulate future observations based on RCP and year scenarios ====

RCPs <- c("rcp45", "rcp85")
Times <- c(2050, 2085)

for (i in 1 : length(weather_info$Name)){
  for (RCP in RCPs){
    for (Time in Times){
      
      start_year <- Time - 15
      end_year <- Time + 15
      
      clim_scen <- getClimateWizardData(c(longitude = weather_info[i, "Longitude"],
                                          latitude = weather_info[i, "Latitude"]),
                                        RCP, start_year,
                                        end_year, temperature_generation_scenarios = TRUE,
                                        baseline = c(1980, 2017))
      
      temps <- temperature_generation(weather = weather_data[[i]], years = c(1980, 2017), 
                                      sim_years = c(2000, 2100),
                                      temperature_scenario = clim_scen)
      
      save_temperature_scenarios(temps, "./data/future_temps",
                                 paste(i, "_", weather_info[i, "Name"], "_", RCP, "_", Time, sep = ""))
      
    }
  }
}


# Compute climate-related metrics for future scenarios

for (i in 1 : 157){
  for(RCP in RCPs){
    for(Time in Times){
      
      temps <- chillR::load_temperature_scenarios("data/future_temps/",
                                                  paste(i, "_", weather_info[i, "Name"], "_",
                                                        RCP, "_", Time, sep = ""))
      
      chill <- chillR::tempResponse_daily_list(temps, latitude = weather_info[i, "Latitude"],
                                               Start_JDay = Start_JDay, End_JDay = End_JDay,
                                               misstolerance = 5)
      
      chillR::save_temperature_scenarios(chill, "./data/future_chill",
                                         paste(i, "_", weather_info[i, "Name"], "_", RCP,
                                               "_", Time, sep = ""))
    }
  }
}
  
  

  