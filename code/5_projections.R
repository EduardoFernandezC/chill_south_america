library(chillR)
library(tidyverse)

# Load the weather data from folder

weather_data <- load_temperature_scenarios("data/re_analysis/fixed_temps/",
                                           prefix = "patched_fixed")

# Remove the weird DATE.1 column

weather_data <- lapply(weather_data, function (x) select(x, -DATE.1))


# Load the information for the weather stations

weather_info <- read.csv("data/re_analysis/weather_stations_final.csv")

weather_info[duplicated(weather_info$Name), "Name"] <- paste0(weather_info[duplicated(weather_info$Name), "Name"], "2")

# Rename the list of weather data

names(weather_data) <- weather_info$Name

# Compute the observed responses ====

Start_JDay <- 121
End_JDay <- 243

dir.create("data/re_analysis/observed_chill")

chill_observed <- list()

for (i in 1 : length(weather_data)){
  
  chill_observed[[i]] <- tempResponse_daily_list(weather_data[[i]], latitude = weather_info[i, "Latitude"],
                                          Start_JDay = Start_JDay, End_JDay = End_JDay, misstolerance = 7)[[1]]
  
  write.csv(chill_observed[[i]], paste("data/re_analysis/observed_chill/", i, "_", weather_info[i, "Name"],
                                       "_observed_chill.csv", sep = ""), row.names = FALSE)
}

#make_chill_plot(chill_observed[[98]],"Chill_Portions",metriclabel="Annual chill (Chill Portions)")


# Simulate historic temperature scenarios from records ====

dir.create("data/re_analysis/historic_simulated_temps")

historic_weather_scenarios <- list()

for(i in 1 : length(weather_data)){
  
  historic_temperature_scenarios <- temperature_scenario_from_records(weather = weather_data[[i]],
                                                                      year = c(1981, 1985, 1989, 1993, 1997, 2001,
                                                                               2005, 2009, 2013, 2017))
  
  
  historic_weather_scenarios[[i]] <- temperature_generation(weather = weather_data[[i]],
                                                            year = c(1981, 1985, 1989, 1993, 1997, 2001,
                                                                     2005, 2009, 2013, 2017),
                                                            sim_years = c(2000, 2099),
                                                            temperature_scenario = historic_temperature_scenarios)
  
  save_temperature_scenarios(historic_weather_scenarios[[i]], "data/re_analysis/historic_simulated_temps/", 
                             paste(i, weather_info[i, "Name"], sep = "_"))
}


rm(historic_temperature_scenarios)

# Read the data from folder

# Generate an empty list to save the outputs
historic_weather_scenarios <- list()

# For loop to read the scenarios
for (i in 1 : length(weather_data)){
  
  historic_weather_scenarios[[i]] <- load_temperature_scenarios("data/re_analysis/historic_simulated_temps/",
                                                                prefix = paste0(i, "_", weather_info[i, "Name"]))
  
}

# Name the elements of the list according to the names in the weather data list
names(historic_weather_scenarios) <- names(weather_data)

# Estimate the responses for the historic simulated scenarios ====

dir.create("data/re_analysis/simulated_chill")

hist_sim_chill <- list()

for (i in 1 : length(historic_weather_scenarios)){
  
  hist_sim_chill[[i]] <- tempResponse_daily_list(historic_weather_scenarios[[i]],
                                            latitude = weather_info[i, "Latitude"],
                                            Start_JDay = Start_JDay, End_JDay = End_JDay)
  
  save_temperature_scenarios(hist_sim_chill[[i]], "data/re_analysis/simulated_chill/",
                             paste(i, weather_info[i, "Name"], "ref_year", sep = "_"))
}


# Compress the folder projections (/hist_sim_chill, /historic_simulated_temps, /observed) ====

zip(zipfile = "./data/projections", files = "./data/projections", flags = " a -idq -ep1",
    zip = "C:/Program Files/WinRAR/WinRAR")


# Remove the folder

unlink("./data/projections", recursive = T)


# Get the climate wizard data and simulate future observations based on RCP and year scenarios ====

# Load a helper function to check the downloaded scenarios
source("code/utilities/check_scenarios.R")

RCPs <- c("rcp45", "rcp85")
Times <- c(2050, 2085)

for (i in 1 : length(weather_data)){
  
  # Set a baseline scenario to compare the data from ClimateWizard to the weather data from the station
  weather_baseline_scen <- temperature_scenario_from_records(weather = weather_data[[i]],
                                                             year = median(c(1980, 2017)))
  
  climate_wizard_baseline_scen <- temperature_scenario_from_records(weather = weather_data[[i]],
                                                                    year = median(c(1980, 2005)))
  
  baseline_adjustment <- temperature_scenario_baseline_adjustment(weather_baseline_scen,
                                                                  climate_wizard_baseline_scen)
  
  
  for (RCP in RCPs){
    for (Time in Times){
      
      start_year <- Time - 15
      end_year <- Time + 15
      
      clim_scen <- getClimateWizardData(c(longitude = weather_info[i, "Longitude"],
                                          latitude = weather_info[i, "Latitude"]),
                                        RCP, start_year,
                                        end_year, temperature_generation_scenarios = TRUE,
                                        baseline = c(1980, 2005))
      
      clim_scen_checked <- lapply(clim_scen, check_scenarios)
      
      clim_scen_adj <- temperature_scenario_baseline_adjustment(baseline_adjustment,
                                                                clim_scen_checked,
                                                                temperature_check_args = list(scenario_check_thresholds = c(-7, 15)))
      
      
      temps <- temperature_generation(weather = weather_data[[i]],
                                      years = c(1980, 2017), 
                                      sim_years = c(2000, 2099),
                                      temperature_scenario = clim_scen_adj,
                                      temperature_check_args = list(scenario_check_thresholds = c(-7, 15)))
      
      save_temperature_scenarios(temps, "data/re_analysis/future_temps",
                                 paste(i, "_", weather_info[i, "Name"], "_", RCP, "_", Time, sep = ""))
      
    }
  }
}


# Compute climate-related metrics for future scenarios

dir.create("data/re_analysis/future_chill")

for (i in 1 : 30){
  for(RCP in RCPs){
    for(Time in Times){
      
      temps <- chillR::load_temperature_scenarios("data/re_analysis/future_temps/",
                                                  paste(i, "_", weather_info[i, "Name"], "_",
                                                        RCP, "_", Time, sep = ""))
      
      chill <- chillR::tempResponse_daily_list(temps, latitude = weather_info[i, "Latitude"],
                                               Start_JDay = Start_JDay, End_JDay = End_JDay,
                                               misstolerance = 5)
      
      chillR::save_temperature_scenarios(chill, "data/re_analysis/future_chill/",
                                         paste(i, "_", weather_info[i, "Name"], "_", RCP,
                                               "_", Time, sep = ""))
    }
  }
}
  
  

  