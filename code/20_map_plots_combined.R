# In this script, I will produce the figure combining all maps generated in 16_interpolation_3d_model.R. May be necessary
# to run the for loop for generating the figures. Make sure the save calls are commented to avoid saving the plots to the
# folder again

# Load the data for the figures
load("figures.RData")

# Figure 2 (map for the figure)
figure_2 <- tm_shape(SA) +
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
  tm_layout(legend.outside = F,
            legend.title.size = 0.85,
            legend.text.size = 0.65,
            legend.position = c(0.665, 0.005),
            outer.margins = c(0.001, 0.001, 0.001, 0.001),
            bg.color = "black",
            attr.color = "white",
            outer.bg.color = "white")

figure_2

tmap_save(figure_2, filename = "figures/final_figures/figure_2_map.png",
          height = height, width = width, units = 'cm')


# Figure 3 will show the historic observed, historic simulated 1981 and 2017 as well as the change between 1981 and 2017
# (simulated)

figure_3 <- tmap_arrange(plot_list[["observed_SWC"]], plot_list[["scen_1981"]], plot_list[["scen_2017"]],
                         change_map)

figure_3

# Save figure 3 with tmap save
tmap_save(figure_3, "figures/final_figures/figure_3_b.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)


# Figure 4 will show the absolute chill for future scenarios. Will only use one combination of RCP and year for both the
# pessimistic and optimistic scenario

figure_4 <- tmap_arrange(plot_list[["rcp45_2050_pessimistic"]], plot_list[["rcp45_2050_optimistic"]],
                         plot_list[["rcp85_2085_pessimistic"]], plot_list[["rcp85_2085_optimistic"]])

figure_4

# Save figure 4 with tmap save
tmap_save(figure_4, "figures/final_figures/figure_4_b.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)


# Figure 5 future chill levels in southern-South America

figure_5 <- tmap_arrange(chile_list[["rcp45_2050_pessimistic"]], chile_list[["rcp45_2050_optimistic"]], 
                         chile_list[["rcp85_2085_pessimistic"]], chile_list[["rcp85_2085_optimistic"]],
                         ncol = 2) 

figure_5

# Save figure 5 with tmap save
tmap_save(figure_5, "figures/final_figures/figure_5_b.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)


# Figure 6 will show the expected change in chill for future scenarios relative to the median historic chill from
# simulated historic scenarios. Will only use one combination of RCP and year with
# pessimistic and optimistic scenarios

figure_6 <- tmap_arrange(change_maps[["rcp45_2050_pessimistic"]], change_maps[["rcp45_2050_optimistic"]], 
                         change_maps[["rcp85_2085_pessimistic"]], change_maps[["rcp85_2085_optimistic"]]) 

figure_6

# Save figure 6 with tmap save
tmap_save(figure_6, "figures/final_figures/figure_6_b.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)

