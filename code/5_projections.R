library(chillR)


# Extract only the weather data from the list after patching and fixing ====

weather_data <- list()

for (i in 1 : length(All_patched_fixed)){
  
  weather_data[[i]] <- All_patched_fixed[[i]][["weather"]]}


# Change the name of the elements of the list

names(weather_data) <- All_WS_90$Name

# Change the name of the dataframe containing information about the weather stations

weather_info <- All_WS_90

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
                                       "_observed_chill.csv", sep = ""), row.names = FALSE)}


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
                             paste(i, weather_info[i, "Name"], sep = "_"))}



# Estimate the responses for the historic simulated scenarios ====

hist_sim_chill <- list()

for (i in 3 : length(historic_weather_scenarios)){
  
  hist_sim_chill[[i]] <- tempResponse_daily_list(historic_weather_scenarios[[i]],
                                            latitude = weather_info[i, "Latitude"],
                                            Start_JDay = Start_JDay, End_JDay = End_JDay)
  
  save_temperature_scenarios(hist_sim_chill[[i]], "./data/projections/hist_sim_chill",
                             paste(i, weather_info[i, "Name"], "ref_year", sep = "_"))}



# Compress the folder projections (/hist_sim_chill, /historic_simulated_temps, /observed) ====

zip(zipfile = "./data/projections", files = "./data/projections", flags = " a -idq -ep1",
    zip = "C:/Program Files/WinRAR/WinRAR")


# Remove the folder

unlink("./data/projections", recursive = T)




# Set the baseline scenarios ====

weather_baseline_scen <- temperature_scenario_from_records(weather = final_weather$weather, year = c(1995))
climate_wizard_baseline_scen <- temperature_scenario_from_records(weather = final_weather$weather,
                                                                  year = c(1995))

baseline_adjustment <- temperature_scenario_baseline_adjustment(weather_baseline_scen,
                                                                climate_wizard_baseline_scen)

####################### Por cada RCP y año empiezo a descargar las posibles temperaturas de futuro ##########

RCPs <- c("rcp45", "rcp85")
Times <- c(2050, 2085)

for(RCP in RCPs)
  for(Time in Times){
    
    start_year <- Time - 15
    end_year <- Time + 15
    
    clim_scen <- getClimateWizardData(c(weather_info[1, "Longitude"], weather_info[1, "Latitude"]),
                                      RCP, start_year,
                                      end_year, temperature_generation_scenarios = TRUE,
                                      baseline = c(1980, 2017))
    
    clim_scen_adj <- temperature_scenario_baseline_adjustment(baseline_adjustment,
                                                              clim_scen)
    
    temps <- temperature_generation(weather = weather_data[[1]], years = c(1980, 2017), 
                                    sim_years = c(2000, 2100),
                                    temperature_scenario = clim_scen_adj)
    
    save_temperature_scenarios(temps,
                               paste(path, "/temperatures", sep=""),
                               paste(RCP, "_", Time, sep = ""))
  }

## Continuación (01/04/2019)
chills_hours <- make_climate_scenario(simu_hist_resp_hours, caption = "Historic",
                                      historic_data = chill_observed_hours) 

chills_days <- make_climate_scenario(simu_hist_resp_days, caption = "Historic",
                                     historic_data = chill_observed_days)


############## Aquí calculo las métricas para cada escenario futuro ########

for(RCP in RCPs)
  for(Time in Times)
  { temps <- load_temperature_scenarios(paste(path, "/temperatures", sep = ""),
                                        paste(RCP, "_", Time, sep = ""))
  chill_hour <- tempResponse_daily_list(temps, latitude, Start_JDay = Start_JDay, End_JDay = End_JDay,
                                        misstolerance = 5, 
                                        models = list(Chill_Hours = Chilling_Hours,
                                                      Chill_Units = Utah_Model,
                                                      Chill_Portions = Dynamic_Model,
                                                      North_Caro_Units = north_carolina_model,
                                                      Posit_Chill_Units = positive_utah_model,
                                                      Modif_Chill_Units = modified_utah_model,
                                                      Low_Chill_Units = low_chill_model,
                                                      Chill_Units_Harri = chilling_units_Harrington))
  save_temperature_scenarios(chill_hour, paste(path, "/future_chill_hours", sep = ""),
                             paste(RCP, "_", Time, sep = ""))
  
  chill_day <- tempResponse_list_daily(temps, Start_JDay = Start_JDay, End_JDay = End_JDay)
  
  save_temperature_scenarios(chill_day, paste(path, "/future_chill_days", sep = ""),
                             paste(RCP, "_", Time, sep = ""))
  
  if(RCP == "rcp45") RCPcaption <- "RCP4.5"
  if(RCP == "rcp85") RCPcaption <- "RCP8.5"   
  chills_hours <- make_climate_scenario(chill_hour, caption = c(RCPcaption, Time), add_to = chills_hours)
  chills_days <- make_climate_scenario(chill_day, caption = c(RCPcaption, Time), add_to = chills_days)  }
