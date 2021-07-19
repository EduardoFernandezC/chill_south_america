# Get the climate wizard data and simulate future observations based on RCP and year scenarios ====

RCPs <- c("rcp45", "rcp85")
Times <- c(2050, 2085)

start <- Sys.Date()

for (i in 1 : 2){
  
  # Set a baseline scenario
  weather_baseline_scen <- temperature_scenario_from_records(weather = weather_data[[i]],
                                                             year = median(c(1980, 2017)))
  
  climate_wizard_baseline_scen <- temperature_scenario_from_records(weather = weather_data[[i]],
                                                                    year = median(c(1980, 2005)))
  
  baseline_adjustment <- temperature_scenario_baseline_adjustment(weather_baseline_scen,
                                                                  climate_wizard_baseline_scen)
  
  
  # Implement the for loop to generate future temps
  for (RCP in RCPs){
    for (Time in Times){
      
      start_year <- Time - 15
      end_year <- Time + 15
      
      clim_scen <- getClimateWizardData(c(longitude = weather_info[i, "Longitude"],
                                          latitude = weather_info[i, "Latitude"]),
                                        RCP, start_year,
                                        end_year, temperature_generation_scenarios = TRUE,
                                        baseline = c(1980, 2005))
      
      clim_scen_adj <- temperature_scenario_baseline_adjustment(baseline_adjustment,
                                                                clim_scen,
                                                                temperature_check_args = list(scenario_check_thresholds = c(-7, 15)))
      
      
      temps <- temperature_generation(weather = weather_data[[i]],
                                      years = c(1980, 2017), 
                                      sim_years = c(2000, 2099),
                                      temperature_scenario = clim_scen_adj)
      
      save_temperature_scenarios(temps, "./data/future_temps_2",
                                 paste(i, "_", weather_info[i, "Name"], "_", RCP, "_", Time, sep = ""))
      
    }
  }
}

end <- Sys.Date()

time <- end - start

time
