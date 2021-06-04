library(tmap)

# This script is to obtain specific safe winter chill values for different zones of South America. It relies on loading
# the RData file for the figures

load("figures.RData")


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

# Change values for Patagonia, Argentina
mean(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], patagonia), na.rm = TRUE)
quantile(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], patagonia), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], patagonia), 0.95, na.rm = TRUE)


# Change values for the Central Valley of Chile
mean(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], central_valley), na.rm = TRUE)
quantile(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], central_valley), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], central_valley), 0.95, na.rm = TRUE)


# Change values for Brazil
mean(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], brazil), na.rm = TRUE)
quantile(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], brazil), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], brazil), 0.95, na.rm = TRUE)


# Define the quadrant for Colombia
colombia <- data.frame(x = seq(-77.5, -72.5, 0.05),
                       y = seq(2.5, 7.5, 0.05))

# Estimate the chill values
mean(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], colombia), na.rm = TRUE)
quantile(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], colombia), 0.05, na.rm = TRUE)
quantile(raster::extract(chill_list[["scen_2017"]] - chill_list[["scen_1981"]], colombia), 0.95, na.rm = TRUE)




b <- bbox(Porig)
b[1, ] <- c(-77, -70)
b[2, ] <- c(-37, -30)
b <- bbox(t(b))

chile <- tm_shape(SA) +
  tm_fill(col = 'grey10') +
  tm_shape(SA_test) +
  tm_lines(col = 'grey35') +
  tm_shape(r.m) +
  tm_raster(palette = get_brewer_pal('RdYlBu', n = 20),
            midpoint = 30,
            title = 'Safe Winter Chill',
            style = 'cont', legend.reverse = TRUE, breaks = seq(0, 100, by = 20),
            legend.format = list(suffix = " CP", text.align = "center")) +
  tm_shape(Porig) +
  tm_symbols(size = 0.075, shape = 4, col = 'firebrick', alpha = 0.8) + 
  tm_shape(SA) +
  tm_borders(col = 'grey40') +
  tm_graticules(lines = F, labels.size = 0.6, labels.col = "black") +
  tm_compass(position = c(0.66, 0.85), text.size = 0.6) +
  tm_scale_bar(position = c(0.57, 0.925), bg.color = 'transparent', text.size = 0.6, color.dark = "grey20") +
  tm_add_legend(type = "line", labels = "Excluded", col = "grey35", lwd = 3) +
  tm_add_legend(type = "symbol", labels = "  Weather station", shape = 4, size = 0.5, col = "firebrick") +
  tm_layout(main.title = paste0("      ", scenarios_fixed[[scen]]),
            main.title.position = "center",
            main.title.size = 1.4,
            main.title.color = "black",
            legend.outside = F,
            legend.title.size = 0.85,
            legend.text.size = 0.65,
            legend.position = c(0.665, 0.005),
            outer.margins = c(0.001, 0.001, 0.001, 0.001),
            bg.color = "black",
            attr.color = "white",
            outer.bg.color = "white")

chile


figure_3 <- tmap_arrange(chile, chile, chile, chile, ncol = 2)

figure_3

# Save figure 3 with tmap save
tmap_save(figure_3, "figures/final_figures/test_color.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)




plot_list[["observed_SWC"]]




