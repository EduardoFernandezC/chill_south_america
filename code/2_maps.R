library(ggplot2)

# Plot the first map only with GSOD weather stations ====

# Define the borders of the countries across SA

SA_countries <- borders("world", regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname",
                                             "Colombia", "Venezuela", "Bolivia", "Ecuador", "Chile", "Paraguay",
                                             "Peru", "Guyana"),
                        fill = "white", colour = "black",
                        size = 0.3)

# Plot the map and the 1500 weather stations

ggplot() + SA_countries + theme_bw() +
  geom_point(aes(Long, Lat), data = SA_GSOD_Weat_Stat, color = "black", size = 1) +
  labs(x = "Longitude", y = "Latitude") +
  theme(panel.grid = element_blank()) +
  coord_equal()

ggsave("./figures/SA_1500_stations.png", dpi = 800)

# Include only those stations starting before 1st Jan 1980 and ending after 31st Dec 2017 (n = 346)

ggplot() + SA_countries + theme_bw() +
  geom_point(aes(Long, Lat),
             data = SA_GSOD_WS_80s_17s,
             color = "black", size = 1) +
  labs(x = "Longitude", y = "Latitude") +
  theme(panel.grid = element_blank()) +
  coord_equal()

ggsave("./figures/SA_filtered_stations.png", dpi = 800)


# Chilean, Argentinean and GSOD weather stations plot ====

# Include weather stations having 90% of data complete (after downloading the data)in chilean and GSOD database

ggplot() + SA_countries + theme_bw() +
  geom_point(aes(Long, Lat), data = SA_GSOD_WS_90, color = "black", size = 1) +
  geom_point(aes(Longitude, Latitude), data = CL_WS_90, color = "red", size = 1) +
  geom_point(aes(Longitude, Latitude), data = AR_WS_90, color = "blue", size = 1) +
  labs(x = "Longitude", y = "Latitude") +
  theme(panel.grid = element_blank()) +
  coord_equal(xlim = c(-30, -85))

ggsave("./figures/WS_90_complete_GSOD_CR_AR.png", dpi = 800)




