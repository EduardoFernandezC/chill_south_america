library(chillR)
library(alternativechillfunctions)





for (i in 1 : length(SA_GSOD_list_90))
  
  tempResponse_daily_list(SA_GSOD_list_90[["RIO GALLEGOS"]], latitude = -51.609,
                           Start_JDay = 121, End_JDay = 243)








my_point <- SA_GSOD_Weat_Stat[which(SA_GSOD_Weat_Stat$chillR_code == SA_GSOD_WS_90[
              SA_GSOD_WS_90$STATION.NAME == "SAUCE VIEJO", "chillR_code"]), c("Long", "Lat")]


SA_GSOD_Weat_Stat["distance"] <- round(sp::spDistsN1(as.matrix(SA_GSOD_Weat_Stat[, c("Long", "Lat")]),
                                                      c(my_point[1, 1], my_point[1, 2]), longlat = TRUE), 2)


SA_GSOD_Weat_Stat <- SA_GSOD_Weat_Stat[order(SA_GSOD_Weat_Stat$distance), ]

SA_GSOD_Weat_Stat$STATION.NAME <- as.character(SA_GSOD_Weat_Stat$STATION.NAME)

SA_GSOD_WS_80s_17s <- SA_GSOD_Weat_Stat[SA_GSOD_Weat_Stat$BEGIN < 19800101 & SA_GSOD_Weat_Stat$END > 20171231 &
                                          SA_GSOD_Weat_Stat$Lat > -60 &
                                          SA_GSOD_Weat_Stat$Long > -85 & SA_GSOD_Weat_Stat$Long < -38, ]


# Remove the station located at Juan Fernandez (CHILE) and Fox Bay (FALKLAND ISLANDS)

SA_GSOD_WS_80s_17s <- SA_GSOD_WS_80s_17s[-which(SA_GSOD_WS_80s_17s$STATION.NAME == "JUAN FERNANDEZ" |
                                                  SA_GSOD_WS_80s_17s$STATION.NAME == "FOX BAY"), ]




SA_GSOD_alt_WS <- SA_GSOD_Weat_Stat[c(2 : 26), ]


SA_GSOD_alt_WS  <- SA_GSOD_alt_WS[SA_GSOD_alt_WS$BEGIN < 20180101 & SA_GSOD_alt_WS$END > 19800101, ]






SA_GSOD_alt_list <- SA_GSOD_list[SA_GSOD_WS_80s_17s[SA_GSOD_WS_80s_17s$chillR_code %in% SA_GSOD_alt_WS$chillR_code,
                                                   "STATION.NAME"]]




patch_daily_temperatures2(SA_GSOD_list_90[["SAUCE VIEJO"]], SA_GSOD_alt_list,
                         max_mean_bias = 4, max_stdev_bias = 4)




patch_daily_temperatures(SA_GSOD_list_90[["SAUCE VIEJO"]], SA_GSOD_alt_list,
                          max_mean_bias = NA, max_stdev_bias = NA)




