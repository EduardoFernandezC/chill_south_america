# In this script, I will produce the figure combining all maps generated in 16_interpolation_3d_model.R. May be necessary
# to run the for loop for generating the figures. Make sure the save calls are commented to avoid saving the plots to the
# folder again


# Figure 3 will show the historic observed, historic simulated 1981 and 2017 as well as the change between 1981 and 2017
# (simulated)

figure_3 <- tmap_arrange(plot_list[["observed_SWC"]], plot_list[["scen_1981"]], plot_list[["scen_2017"]],
                         change_map)

figure_3

# Save figure 3 with tmap save
tmap_save(figure_3, "figures/final_figures/figure_3.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)


# Figure 4 will show the absolute chill for future scenarios. Will only use one combination of RCP and year for both the
# pessimistic and optimistic scenario

figure_4 <- tmap_arrange(plot_list[["rcp45_2050_pessimistic"]], plot_list[["rcp45_2050_optimistic"]],
                         plot_list[["rcp85_2085_pessimistic"]], plot_list[["rcp85_2085_optimistic"]])

figure_4

# Save figure 4 with tmap save
tmap_save(figure_4, "figures/final_figures/figure_4.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)


# Figure 5 will show the expected change in chill for future scenarios relative to the median historic chill from
# simulated historic scenarios. Will only use one combination of RCP and year with
# pessimistic and optimistic scenarios

figure_5 <- tmap_arrange(change_maps[["rcp45_2050_pessimistic"]], change_maps[["rcp45_2050_optimistic"]], 
                         change_maps[["rcp85_2085_pessimistic"]], change_maps[["rcp85_2085_optimistic"]]) 

figure_5

# Save figure 5 with tmap save
tmap_save(figure_5, "figures/final_figures/figure_5.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)


# Figure 7 future chill levels in southern-South America

figure_7 <- tmap_arrange(chile_list[["rcp45_2050_pessimistic"]], chile_list[["rcp45_2050_optimistic"]], 
                         chile_list[["rcp85_2085_pessimistic"]], chile_list[["rcp85_2085_optimistic"]]) 

figure_7

# Save figure 5 with tmap save
tmap_save(figure_7, "figures/final_figures/figure_7.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)
