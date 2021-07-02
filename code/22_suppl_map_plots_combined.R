# In this script I will produce supplementary figures for the manuscript. Mainly, the future scenarios not included in the
# manuscript

# Load the data for the figures
load("figures.RData")

# Figure S3 will show the RCP4.5 by 2085 and RCP by 2050 in the pessimistic and optimistic version

figure_S3 <- tmap_arrange(plot_list[["rcp45_2085_pessimistic"]], plot_list[["rcp45_2085_optimistic"]],
                          plot_list[["rcp85_2050_pessimistic"]], plot_list[["rcp85_2050_optimistic"]])

figure_S3

# Save figure S3 with tmap save
tmap_save(figure_S3, "figures/final_figures/figure_S3.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)


# Figure S4 will show all combinations of RCP and time horizon using only the intermediate climate model scenario

figure_S4 <- tmap_arrange(plot_list[["rcp45_2050_intermediate"]], plot_list[["rcp45_2085_intermediate"]],
                          plot_list[["rcp85_2050_intermediate"]], plot_list[["rcp85_2085_intermediate"]])

figure_S4

# Save figure S4 with tmap save
tmap_save(figure_S4, "figures/final_figures/figure_S4.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)


# Figure S5 will show the RCP4.5 by 2085 and RCP by 2050 in the pessimistic and optimistic version for 
# southern SA

figure_S5 <- tmap_arrange(chile_list[["rcp45_2085_pessimistic"]], chile_list[["rcp45_2085_optimistic"]],
                          chile_list[["rcp85_2050_pessimistic"]], chile_list[["rcp85_2050_optimistic"]],
                          ncol = 2)

figure_S5

# Save figure S5 with tmap save
tmap_save(figure_S5, "figures/final_figures/figure_S5.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)


# Figure S6 will show all combinations of RCP and time horizon using only the intermediate climate model scenario
# for southern SA

figure_S6 <- tmap_arrange(chile_list[["rcp45_2050_intermediate"]], chile_list[["rcp45_2085_intermediate"]],
                          chile_list[["rcp85_2050_intermediate"]], chile_list[["rcp85_2085_intermediate"]],
                          ncol = 2)

figure_S6

# Save figure S6 with tmap save
tmap_save(figure_S6, "figures/final_figures/figure_S6.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)



# Figure S7 will show the RCP4.5 by 2085 and RCP by 2050 in the pessimistic and optimistic version (change plot)

figure_S7 <- tmap_arrange(change_maps[["rcp45_2085_pessimistic"]], change_maps[["rcp45_2085_optimistic"]],
                          change_maps[["rcp85_2050_pessimistic"]], change_maps[["rcp85_2050_optimistic"]])

figure_S7

# Save figure S7 with tmap save
tmap_save(figure_S7, "figures/final_figures/figure_S7.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)


# Figure S8 will show all combinations of RCP and time horizon using only the intermediate climate model scenario
# (change plot)

figure_S8 <- tmap_arrange(change_maps[["rcp45_2050_intermediate"]], change_maps[["rcp45_2085_intermediate"]],
                          change_maps[["rcp85_2050_intermediate"]], change_maps[["rcp85_2085_intermediate"]])

figure_S8

# Save figure S9 with tmap save
tmap_save(figure_S8, "figures/final_figures/figure_S8.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)

