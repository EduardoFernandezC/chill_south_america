library(ggplot2)
library(tmap)

# Plot the first map only with GSOD weather stations ====

# Define the borders of the countries across SA

SA_countries <- borders("world", regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname",
                                             "Colombia", "Venezuela", "Bolivia", "Ecuador", "Chile", "Paraguay",
                                             "Peru", "Guyana"),
                        fill = "goldenrod", colour = "grey40",
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
  coord_equal()

ggsave("./figures/WS_90_complete_GSOD_CR_AR.png", dpi = 800)




# Map for all the weather stations remaining after the patching and fixing process (28/11/19) ====

# load the data 

load("fixed_WS.RData")

# Classify the database to get the legend according to the source

All_WS_90[!is.na(All_WS_90$chillR_code), "Database"] <- "GSOD"
All_WS_90[is.na(All_WS_90$chillR_code) & !is.na(All_WS_90$Cod_Station), "Database"] <- "CR2"
All_WS_90[is.na(All_WS_90$chillR_code) & !is.na(All_WS_90$Perc_Tmin), "Database"] <- "SMN - INTA"

# Generate the plot

ggplot() + SA_countries +
  geom_point(aes(Longitude, Latitude, shape = Database), data = All_WS_90, size = 1.5) +
  scale_shape_manual(values = c(1, 2, 3), breaks = c("CR2", "GSOD", "SMN - INTA"),
                     labels = c(expression("[CR]"^2), "GSOD", "SMN - INTA")) +
  scale_x_continuous(limits = c(-85, -33), labels = function(x) paste0(x, "°"), expand = expansion(mult = 0.015)) +
  scale_y_continuous(limits = c(-59, 12.5), labels = function(x) paste0(x, "°"), expand = expansion(mult = 0.015)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        legend.text.align = 0,
        legend.position = c(0.82, 0.13)) +
  coord_equal() + 
  ggsn::scalebar(location = "bottomleft", dist = 500, dist_unit = "km", transform = TRUE,
                 model = "WGS84", x.min = -85, x.max = -33, y.min = -57.5, y.max = -12.5,
                 st.bottom = TRUE, st.size = 2.5, st.dist = 0.03, border.size = 0.3) 

ggsave("figures/figure_1.png", height = 5.05, width = 6.5, dpi = 600)


# Remake figure 1 using tmap

stations <- read.csv("data/weather_info.csv")

# Add the data base column

stations[!is.na(stations$chillR_code), "Database"] <- "GSOD"
stations[is.na(stations$chillR_code) & !is.na(stations$Cod_Station), "Database"] <- "CR2"
stations[is.na(stations$chillR_code) & !is.na(stations$Perc_Tmin), "Database"] <- "SMN - INTA"

# Transform the data frame into a spatial data frame

stations_sp <-  SpatialPointsDataFrame(stations[, c("Longitude", "Latitude")],
                                            proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                            data = stations[, -(which(colnames(stations) %in% 
                                                                                     c("Longitude", "Latitude")))])


# Read the shape file from SA
SA <- readOGR('data/sa_outline/SA_outline.shp')

# Replace point boundary extent with that of South America to make sure the interpolation is done for the whole extend of south america
stations_sp@bbox <- SA@bbox

# Get the elevation model of south america
elevation <- elevatr::get_elev_raster(SA, z = 6, neg_to_na = TRUE)

# Create the map
stations_map <- tm_shape(elevation, bbox = extent(-81.75, -34.25, -56, 12.75)) +
  tm_raster(breaks = seq(0, 4500, 1500), style = "cont", title = "Elevation (a.s.l.)",
            legend.format = list(suffix = " m", text.align = "center")) +
  tm_shape(SA) +
  tm_borders(col = 'grey40') +
  tm_graticules(lines = FALSE, labels.size = 0.6, labels.col = "black") +
  tm_shape(stations_sp[stations_sp$Database == "GSOD", ]) +
  tm_symbols(size = 0.1, shape = 2, col = 'steelblue4') +
  tm_shape(stations_sp[stations_sp$Database == "CR2", ]) +
  tm_symbols(size = 0.1, shape = 1, col = 'steelblue4') +
  tm_shape(stations_sp[stations_sp$Database == "SMN - INTA", ]) +
  tm_symbols(size = 0.1, shape = 3, col = 'steelblue4') +
  tm_compass(position = c(0.69, 0.9), text.size = 0.5) +
  tm_scale_bar(position = c(0.57, 0.95), bg.color = 'transparent', text.size = 0.5, color.dark = "grey20") +
  tm_add_legend(type = "symbol", labels = c("GSOD", expression("[CR]"^2), "SMN - INTA"),
                col = "steelblue4", shape = c(2, 1, 3), size = 0.35, title = "Database") +
  tm_layout(legend.outside = FALSE,
            legend.position = c(0.6, 0),
            outer.margins = c(0.01, 0.01, 0.01, 0.01),
            legend.title.size = 0.9,
            legend.text.size = 0.6,
            bg.color = "black", 
            attr.color = "white")

stations_map

tmap_save(stations_map, 'figures/final_figures/figure_1_b.png',
          height = 12, width = 11, units = 'cm')