library(tmap)

# This script is to obtain specific safe winter chill values for different zones of South America. It relies on loading
# the RData file for the figures

load("figures.RData")

#### Section 3.1 in the manuscript

### Historic observed panel

# Define the quadrant for southern Brazil
brazil <- data.frame(x = seq(-57, -49, 0.05),
                     y = seq(-33, -25, 0.05))

# Estimate the chill values
median(raster::extract(chill_list[["observed_SWC"]], brazil))
quantile(raster::extract(chill_list[["observed_SWC"]], brazil), 0.05)
quantile(raster::extract(chill_list[["observed_SWC"]], brazil), 0.95)


# Define the quadrant for northern Patagonia, Argentina
patagonia <- data.frame(x = seq(-70, -61, 0.05),
                        y = seq(-46, -37, 0.05))

# Estimate the chill values
median(raster::extract(chill_list[["observed_SWC"]], patagonia), na.rm = TRUE)
quantile(raster::extract(chill_list[["observed_SWC"]], patagonia), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["observed_SWC"]], patagonia), 0.95, na.rm = TRUE)



# Define the quadrant for the Central Valley of Chile
central_valley <- data.frame(x = seq(-77, -70, 0.05),
                             y = seq(-37, -30, 0.05))

# Estimate the chill values
median(raster::extract(chill_list[["observed_SWC"]], central_valley), na.rm = TRUE)
quantile(raster::extract(chill_list[["observed_SWC"]], central_valley), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["observed_SWC"]], central_valley), 0.95, na.rm = TRUE)



### Chill change 1981 - 2017 panel

# Find the extreme values
chill_list[["scen_2017"]] - chill_list[["scen_1981"]]


# Define the quadrant for Colombia
colombia <- data.frame(x = seq(-77.5, -72.5, 0.05),
                       y = seq(2.5, 7.5, 0.05))

# Estimate the chill values
mean(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], colombia), na.rm = TRUE)
quantile(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], colombia), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], colombia), 0.95, na.rm = TRUE)


# Change values for the Central Valley of Chile
mean(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], central_valley), na.rm = TRUE)
quantile(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], central_valley), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], central_valley), 0.95, na.rm = TRUE)


# Change values for Patagonia, Argentina
mean(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], patagonia), na.rm = TRUE)
quantile(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], patagonia), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], patagonia), 0.95, na.rm = TRUE)


# Change values for Brazil
mean(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], brazil), na.rm = TRUE)
quantile(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], brazil), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], brazil), 0.95, na.rm = TRUE)


#### Section 3.2 in the manuscript

# Extract the maximum chill values in a continental scale...
chill_list[["rcp45_2050_pessimistic"]]
chill_list[["rcp45_2050_optimistic"]]

chill_list[["rcp85_2085_pessimistic"]]
chill_list[["rcp85_2085_optimistic"]]


# Focus on specific locations like southern Brazil, northern Patagonia, and the Central Valley of Chile

# Southern Brazil
# RCP4.5 - 2050
median(raster::extract(chill_list[["rcp45_2050_pessimistic"]], brazil), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_pessimistic"]], brazil), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_pessimistic"]], brazil), 0.95, na.rm = TRUE)

median(raster::extract(chill_list[["rcp45_2050_intermediate"]], brazil), na.rm = TRUE)

median(raster::extract(chill_list[["rcp45_2050_optimistic"]], brazil), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_optimistic"]], brazil), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_optimistic"]], brazil), 0.95, na.rm = TRUE)


# RCP8.5 - 2085
median(raster::extract(chill_list[["rcp85_2085_pessimistic"]], brazil), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_pessimistic"]], brazil), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_pessimistic"]], brazil), 0.95, na.rm = TRUE)

median(raster::extract(chill_list[["rcp85_2085_optimistic"]], brazil), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_optimistic"]], brazil), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_optimistic"]], brazil), 0.95, na.rm = TRUE)


# Northern Patagonia
# RCP4.5 - 2050
median(raster::extract(chill_list[["rcp45_2050_pessimistic"]], patagonia), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_pessimistic"]], patagonia), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_pessimistic"]], patagonia), 0.95, na.rm = TRUE)

median(raster::extract(chill_list[["rcp45_2050_optimistic"]], patagonia), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_optimistic"]], patagonia), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_optimistic"]], patagonia), 0.95, na.rm = TRUE)


# RCP8.5 - 2085
median(raster::extract(chill_list[["rcp85_2085_pessimistic"]], patagonia), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_pessimistic"]], patagonia), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_pessimistic"]], patagonia), 0.95, na.rm = TRUE)

median(raster::extract(chill_list[["rcp85_2085_optimistic"]], patagonia), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_optimistic"]], patagonia), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_optimistic"]], patagonia), 0.95, na.rm = TRUE)



# Central valley
# RCP4.5 - 2050
median(raster::extract(chill_list[["rcp45_2050_pessimistic"]], central_valley), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_pessimistic"]], central_valley), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_pessimistic"]], central_valley), 0.95, na.rm = TRUE)

median(raster::extract(chill_list[["rcp45_2050_intermediate"]], central_valley), na.rm = TRUE)

median(raster::extract(chill_list[["rcp45_2050_optimistic"]], central_valley), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_optimistic"]], central_valley), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_optimistic"]], central_valley), 0.95, na.rm = TRUE)


# RCP8.5 - 2085
median(raster::extract(chill_list[["rcp85_2085_pessimistic"]], central_valley), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_pessimistic"]], central_valley), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_pessimistic"]], central_valley), 0.95, na.rm = TRUE)

median(raster::extract(chill_list[["rcp85_2085_optimistic"]], central_valley), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_optimistic"]], central_valley), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_optimistic"]], central_valley), 0.95, na.rm = TRUE)


#### Section 3.3 in the manuscript

# General overview (min and max) from raster elements
chill_list[["rcp45_2050_pessimistic"]] - median_raster_scen
chill_list[["rcp45_2050_optimistic"]] - median_raster_scen


chill_list[["rcp85_2085_pessimistic"]] - median_raster_scen
chill_list[["rcp85_2085_optimistic"]] - median_raster_scen


# Central valley
# RCP4.5 - 2050
median(raster::extract(chill_list[["rcp45_2050_pessimistic"]] - median_raster_scen, central_valley), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_pessimistic"]] - median_raster_scen, central_valley), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_pessimistic"]] - median_raster_scen, central_valley), 0.95, na.rm = TRUE)

median(raster::extract(chill_list[["rcp45_2050_optimistic"]] - median_raster_scen, central_valley), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_optimistic"]] - median_raster_scen, central_valley), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_optimistic"]] - median_raster_scen, central_valley), 0.95, na.rm = TRUE)


# RCP8.5 - 2085
median(raster::extract(chill_list[["rcp85_2085_pessimistic"]] - median_raster_scen, central_valley), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_pessimistic"]] - median_raster_scen, central_valley), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_pessimistic"]] - median_raster_scen, central_valley), 0.95, na.rm = TRUE)

median(raster::extract(chill_list[["rcp85_2085_optimistic"]] - median_raster_scen, central_valley), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_optimistic"]] - median_raster_scen, central_valley), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_optimistic"]] - median_raster_scen, central_valley), 0.95, na.rm = TRUE)


# Northern Patagonia, Argentina
# RCP4.5 - 2050
median(raster::extract(chill_list[["rcp45_2050_pessimistic"]] - median_raster_scen, patagonia), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_pessimistic"]] - median_raster_scen, patagonia), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_pessimistic"]] - median_raster_scen, patagonia), 0.95, na.rm = TRUE)

median(raster::extract(chill_list[["rcp45_2050_optimistic"]] - median_raster_scen, patagonia), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_optimistic"]] - median_raster_scen, patagonia), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_optimistic"]] - median_raster_scen, patagonia), 0.95, na.rm = TRUE)


# RCP8.5 - 2085
median(raster::extract(chill_list[["rcp85_2085_pessimistic"]] - median_raster_scen, patagonia), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_pessimistic"]] - median_raster_scen, patagonia), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_pessimistic"]] - median_raster_scen, patagonia), 0.95, na.rm = TRUE)

median(raster::extract(chill_list[["rcp85_2085_optimistic"]] - median_raster_scen, patagonia), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_optimistic"]] - median_raster_scen, patagonia), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_optimistic"]] - median_raster_scen, patagonia), 0.95, na.rm = TRUE)



# Southern Brazil
# RCP4.5 - 2050
median(raster::extract(chill_list[["rcp45_2050_pessimistic"]] - median_raster_scen, brazil), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_pessimistic"]] - median_raster_scen, brazil), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_pessimistic"]] - median_raster_scen, brazil), 0.95, na.rm = TRUE)

median(raster::extract(chill_list[["rcp45_2050_optimistic"]] - median_raster_scen, brazil), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_optimistic"]] - median_raster_scen, brazil), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp45_2050_optimistic"]] - median_raster_scen, brazil), 0.95, na.rm = TRUE)


# RCP8.5 - 2085
median(raster::extract(chill_list[["rcp85_2085_pessimistic"]] - median_raster_scen, brazil), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_pessimistic"]] - median_raster_scen, brazil), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_pessimistic"]] - median_raster_scen, brazil), 0.95, na.rm = TRUE)

median(raster::extract(chill_list[["rcp85_2085_optimistic"]] - median_raster_scen, brazil), na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_optimistic"]] - median_raster_scen, brazil), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["rcp85_2085_optimistic"]] - median_raster_scen, brazil), 0.95, na.rm = TRUE)








