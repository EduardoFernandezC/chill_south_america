library(chillR)





for (i in 1 : length(SA_GSOD_list_90))
  
  tempResponse_daily_list(SA_GSOD_list_90[["RIO GALLEGOS"]], latitude = -51.609,
                           Start_JDay = 121, End_JDay = 243)








my_point <- SA_GSOD_Weat_Stat[which(SA_GSOD_Weat_Stat$chillR_code == SA_GSOD_WS_90[
              SA_GSOD_WS_90$STATION.NAME == "RIO GALLEGOS", "chillR_code"]), c("Long", "Lat")]


SA_GSOD_Weat_Stat["distance2"] <- round(sp::spDistsN1(as.matrix(SA_GSOD_Weat_Stat[, c("Long", "Lat")]),
                                                      c(my_point[1, 1], my_point[1, 2]), longlat = TRUE), 2)


SA_GSOD_Weat_Stat <- SA_GSOD_Weat_Stat[order(SA_GSOD_Weat_Stat$distance2), ]


SA_GSOD_alt_WS <- SA_GSOD_Weat_Stat[c(2 : 26), ]


SA_GSOD_alt_WS  <- SA_GSOD_alt_WS[SA_GSOD_alt_WS$BEGIN < 20180101 & SA_GSOD_alt_WS$END > 19800101, 1]




a <- SA_GSOD_list[SA_GSOD_WS_80s_17s[SA_GSOD_WS_80s_17s$chillR_code %in% SA_GSOD_alt_WS$chillR_code,
                                     "STATION.NAME"]]






patch_daily_temperatures(SA_GSOD_list_90[["RIO GALLEGOS"]], a, max_mean_bias = 4, max_stdev_bias = 4)










