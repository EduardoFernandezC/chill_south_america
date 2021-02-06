library(tidyverse)

# This script is to obtain some quality values regarding our patching and filling procedure

# It is important to load the Rdata object generated in the script 3.1_filling_gaps.R

load("fixed_WS.RData")

# First, we compute the percentage of data complete to get an idea of the mean percentage of missing values.
# This is done to the All_data_90 object minus 4 stations that were not used at the end of the analysis because the
# patching procedure did not work (too many missing still)

perc_complete_list <- lapply(All_data_90[-c(113, 124, 59, 148)], dormancyR::perc_complete)

# Transform the list into a dataframe

perc_complete_df <- dplyr::bind_rows(perc_complete_list, .id = "Weather_Station")

# Now, we compute the minimum percentage of data complete

range_perc_Tmin <- range(perc_complete_df[perc_complete_df$Variable == "Tmin", "Percentage"])
mean_perc_Tmin <- mean(perc_complete_df[perc_complete_df$Variable == "Tmin", "Percentage"])
median_perc_Tmin <- median(perc_complete_df[perc_complete_df$Variable == "Tmin", "Percentage"])

range_perc_Tmax <- range(perc_complete_df[perc_complete_df$Variable == "Tmax", "Percentage"])
mean_perc_Tmax <- mean(perc_complete_df[perc_complete_df$Variable == "Tmax", "Percentage"])
median_perc_Tmax <- median(perc_complete_df[perc_complete_df$Variable == "Tmax", "Percentage"])

# Determine the number of missing days

perc_complete_df[perc_complete_df$Percentage == min_perc_Tmin, ]
perc_complete_df[perc_complete_df$Percentage == min_perc_Tmax, ]


# Metrics related to the remaining gaps after the patching procedure

# Generate a list of dataframes containing only the rows in which the linear interpolation
# procedure was implemented...

interpolated <- list()

for (i in 1 : length(All_patched_fixed)){
  
  temp <- All_patched_fixed[[i]][["weather"]]
  
  temp <- temp[which(temp$no_Tmin == TRUE | temp$no_Tmax == TRUE), ]
  
  interpolated[[i]]<- temp
}

# Create a vector containing the number of days interpolated in the whole period

interpolated_vector <- unlist(lapply(interpolated, function(x) nrow(x)))


# Determine how many stations were subjected to linear interpolation

sum(interpolated_vector != 0)

# Determine the mean number of days interpolated

mean(interpolated_vector[which(interpolated_vector != 0)])
range(interpolated_vector[which(interpolated_vector != 0)])

# Determine in how many stations the linear interpolation coincided with the chilling period

sum(unlist(lapply(interpolated, function(x) nrow(x[x$Month %in% c(5 : 8), ]))) != 0)





