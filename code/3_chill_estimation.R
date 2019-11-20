library(chillR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(alternativechillfunctions)




for (i in 1 : length(SA_GSOD_list_90))
  
  tempResponse_daily_list(SA_GSOD_list_90[["RIO GALLEGOS"]], latitude = -51.609,
                           Start_JDay = 121, End_JDay = 243)


