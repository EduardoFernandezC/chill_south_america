#### combine repeated n-folded cross validation and interpolation method for chill
library(rgdal)
library(tmap)
library(spatstat) # Used for the dirichlet tessellation function
library(maptools) # Used for conversion from SPDF to ppp
library(raster) # Used to clip out thiessen polygons
library(gstat) # Use gstat's idw routine
library(sp) # Used for the spsample function
require(tmaptools)
library(gridExtra)
library(reshape2)#to melt a data frame
#stuff needed for the set up of the tmin, tmax plane
library(fields)
library(metR)
library(colorRamps)
library(sf)
library(tidyverse)
library(cartography) #needed to include structures like stripes to maps
library(data.table)#rbindlist function


# general settings   ---------------------------------
#set height and width (cm) of maps when maps are saved

#number of repeats
repititions <- 5
#number of splits of data set (usually between 5 and 10)
k <- 7

#set seed for reproducability
set.seed(123)

height <- 13
width <- 12

#set extent to outline of south america
bb <- extent(-109.46044, -26.23419, -59.48714, 12.62908)

#function which gets the chill correction for a tmin and tmax entry, work only for individual values, needs to be used in a loop / apply function
get_chill_correction <-  function(tmin, tmax, lookup = pred){
  if(is.na(tmin) == T){
    return(NA)
  } else if(is.na(tmax) == T){
    return(NA)
  } else{
    tmin_index <- which.min(abs(lookup$x - tmin))
    tmax_index <- which.min(abs(lookup$y - tmax))
    return(lookup$z[tmin_index,tmax_index])
  }
}

#####read data and prepare grid

#read station coordinates with the projected chill (future and historic)
stations <- read.csv('data/re_analysis/all_chill_projections.csv')

#save scenario names to vector
scenarions <- colnames(stations)[6:28]

#trnasform to spatial dataframe
Porig<-SpatialPointsDataFrame(stations[,c("Longitude","Latitude")],
                              proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                              data = dplyr::select(stations, -Latitude, -Longitude))

#outline of south america
SA <- readOGR('data/sa_outline/SA_outline_2.shp')

# Replace point boundary extent with that of South America to make sure the interpolation is done for the whole extend of south america
Porig@bbox <- SA@bbox

# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(SA, "regular", n=50000))
names(grd) <- c("Longitude", "Latitude")
coordinates(grd) <- c("Longitude", "Latitude")
gridded(grd) <- TRUE # Create SpatialPixel object
fullgrid(grd) <- TRUE # Create SpatialGrid object
proj4string(grd) <- proj4string(Porig)

#load tmin and tmax map for july
min_temp_jul <- raster('data/world_clim/wc2-2/wc2.1_30s_tmin_07.tif')
max_temp_jul <- raster('data/world_clim/wc2-3/wc2.1_30s_tmax_07.tif')

#extract south america from world wide map
min_temp_jul <- crop(min_temp_jul, bb)
max_temp_jul <- crop(max_temp_jul, bb)

#adjust resolution of temperature map to match the grid of our project
temp_min.res<-resample(min_temp_jul,raster(grd))
temp_max.res<-resample(max_temp_jul,raster(grd))


# Remove the weather stations excluded in the interpolation procedure...
stations_clean <- as.data.frame(subset(stations, !(outlier_tmin_jul | outlier_tmax_jul)))


# data splitting   ---------------------------------

#loop for repitions of repeatead k-fold cross-validation
for(rep in 1 : repititions){

  #split data set in k even groups
  split_df <- split(stations_clean, sample(1 : k, nrow(stations_clean), replace = T))
  
  #create empty list in which the values are stored
  eval_list <- vector(mode = "list", length = k)
  
  #loop for crossvalidation, i is marking the data frame which is held back to be used as validation
  for(i in 1 : k){
    #combine all splits except the one for the training
    train_df <-as.data.frame(rbindlist(split_df[-i]))
    train_df <- SpatialPointsDataFrame(train_df[,c("Longitude","Latitude")],
                                       proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                       data = dplyr::select(train_df, -Latitude, -Longitude))
    
    eval_df_original <- split_df[[i]]
    eval_df_original <- SpatialPointsDataFrame(eval_df_original[,c("Longitude","Latitude")],
                                      proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                      data = dplyr::select(eval_df_original, -Latitude, -Longitude))
    
    
    
    # prepare reference interpolation map  ---------------------------------
    
    ## produce interpolated layer from both temperature maps of all station locations
    f.temp_min<-as.formula(min_temp_jul ~ Longitude + Latitude)
    f.temp_max<-as.formula(max_temp_jul ~ Longitude + Latitude)
    
    #old approach to fit variogram
    # #set up variogram
    # var.smpl.temp_min <- variogram(f.temp_min, train_df)
    # var.smpl.temp_max <- variogram(f.temp_max, train_df)
    # 
    # dat.fit.temp_min <- fit.variogram(var.smpl.temp_min, fit.ranges = FALSE,
    #                                   fit.sills = FALSE,
    #                                   vgm(model="Sph", range = 2600, psil = 12, nugget = 3))
    # dat.fit.temp_max <- fit.variogram(var.smpl.temp_max, fit.ranges = FALSE,
    #                                   fit.sills = FALSE,
    #                                   vgm(model="Sph", range = 2600, psil = 5, nugget = 1))
    # #do the krigging
    # dat.krg.temp_min <- krige( f.temp_min, train_df, grd, dat.fit.temp_min)
    # dat.krg.temp_max <- krige( f.temp_max, train_df, grd, dat.fit.temp_max)
    
    # At this point I will try to use a different approach by using a function that fits automatically the
    # variogram based on the data. This may be helpful since there will be no need to set the nugget, psill, and
    # range manually
    #fix.values = c(nugget, range: sill); NA = not fixed, decided to ignore values of distance > 1500, that is why range = 240
    var_smpl_min_temp_jul <- automap::autofitVariogram(f.temp_min, train_df)
    #plot(var_smpl_min_temp_jul)
    
    var_smpl_max_temp_jul <- automap::autofitVariogram(f.temp_max, train_df,fix.values = c(NA,240,NA))
    #plot(var_smpl_max_temp_jul)
    
    #do the krigging
    dat.krg.temp_min <- krige(f.temp_min, train_df, grd, var_smpl_min_temp_jul$var_model)
    dat.krg.temp_max <- krige(f.temp_max, train_df, grd, var_smpl_max_temp_jul$var_model)
    

    
    #transform the kriged surface to a raster
    r_krig_min<-raster(dat.krg.temp_min)
    #only use the rasters within the boundaries of south america
    r.m_min <- mask(r_krig_min, SA)
    r_krig_max<-raster(dat.krg.temp_max)
    r.m_max <- mask(r_krig_max, SA)
    
    
    # Set up correction model  ---------------------------------
  
    #loop for scenarios
    for (scen in scenarions){
      
      #krig the tmin tmax data on a plane
      model_krig <- Krig(x = as.matrix(as.data.frame(train_df)[, c("min_temp_jul", "max_temp_jul")]),
                         Y = as.data.frame(train_df)[scen])
      pred <- predictSurface(model_krig)
      
      #adjust row and column name of object
      colnames(pred$z)<-pred$y
      rownames(pred$z)<-pred$x
      
      #save number of rows and cols
      no_row <- nrow(r.m_min)
      no_col <- ncol(r.m_min)
      
      #transform kriged tmin and tmax to matrix
      mat_krig_tmin <- matrix(r.m_min, nrow = no_row, ncol = no_col,byrow = T)
      mat_krig_tmax <- matrix(r.m_max, nrow = no_row, ncol = no_col,byrow = T)
      mat_real_tmin <- matrix(temp_min.res, nrow = no_row, ncol = no_col,byrow = T)
      mat_real_tmax <- matrix(temp_max.res, nrow = no_row, ncol = no_col,byrow = T)
      
      #transform matrix to vector and bind tmin and tmax
      t_both <- cbind(as.vector(mat_krig_tmin),as.vector(mat_krig_tmax))
      t_both_real <- cbind(as.vector(mat_real_tmin),as.vector(mat_real_tmax))
      
      #extract the model chill for real and kriged tmin and tmax
      model_krigged_temp <- sapply(1:nrow(t_both), function(i) get_chill_correction(t_both[i,1], t_both[i,2]))
      model_real_temp <- sapply(1:nrow(t_both_real), function(i) get_chill_correction(t_both_real[i,1], t_both_real[i,2]))
      
      #calculate the adjustment (so the chill, which so far was not capured by krigging)
      #problem: model_real_temp contains many NA
      model_adjust <- model_real_temp - model_krigged_temp
      
      model_adjust <- matrix(model_adjust,nrow = no_row, ncol = no_col)
      
      #transform the matrix back to a grid
      raster_model_adjust <- raster(model_adjust)
      raster_model_adjust <- setExtent(raster_model_adjust,bb)
      crs(raster_model_adjust) <- crs(r.m_min)
      
      
      # Interpolation of chill  ---------------------------------
      
      #do interpolation of chill
      # Define the krigging model for the chill
      f.1 <- as.formula(paste(scen, "~ Longitude + Latitude"))
      
      #old approach to fit semivariogram
      # # Compute the sample variogram; note that the f.1 trend model is one of the
      # # parameters passed to variogram(). This tells the function to create the
      # # variogram on the de-trended data.
      # var.smpl <- variogram(f.1, train_df, cloud = FALSE)
      # 
      # # Compute the variogram model by passing the nugget, sill and range values
      # # to fit.variogram() via the vgm() function.
      # dat.fit <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
      #                          vgm(psill = 275, model="Sph", nugget = 20, range = 1500))
      
      #automatical fitting of variogram, so it works also in a loop where the coefficient cannot be adjusted
      var.smpl <- automap::autofitVariogram(f.1, train_df)
      #plot(var.smpl)
      
      # Perform the krige interpolation (note the use of the variogram model
      # created in the earlier step)
      dat.krg.chil <- krige(f.1, train_df, grd, var.smpl$var_model)
      
      #assign krigged data to the raster
      r_krig<-raster(dat.krg.chil)
      r.m.chill <- mask(r_krig, SA)
      r.m.chill<-max(r.m.chill,0)
      
      raster_model_adjust <- setExtent(raster_model_adjust, extent(r.m.chill)) 
      
      #adjust the chill portions, prevent that chill portions become lower than zero
      r<-max(r.m.chill+raster_model_adjust,0)
      r.m <- mask(r, SA)
      
      # evaluation of interpolation result   ---------------------------------
      
      # The step to follow generates NA values estimated by the model even for locations not excluded by
      # the rule of 2 C when comparing Tmin and Tmax from WorldClim and on-site data. The following lines
      # will attempt to address this issue (TBH: I am not sure if we should keep it as is or fix it).
      # I found a solution in stackoverflow (https://stackoverflow.com/questions/27562076/if-raster-value-na-search-and-extract-the-nearest-non-na-pixel)
      # that looks for the nearest value estimated by the model...
      
      # Generates a data frame only having the coordinates
      coords_eval_df <- data.frame(x = as.data.frame(eval_df_original)$Longitude,
                                   y = as.data.frame(eval_df_original)$Latitude)
      
      #extract values from chill map
      model_estimates <- raster::extract(r.m, coords_eval_df, method = "bilinear")
      
      # Copy the original eval_df
      eval_df <- eval_df_original
      
      # Add the model estimates eval_df to compare them latter
      eval_df$model_value <- model_estimates
      
      #transform to data frame, only use columns of interest (name, country, chil1981, modelvalue)
      eval_df <- as.data.frame(eval_df[, c("station_name", "CTRY", scen, "model_value")])
      
      # Pivot longer for further compatibility
      eval_df <- pivot_longer(eval_df, all_of(scen), names_to = "scenario", values_to = "observed_value")
      
      # Generate a big dataframe with multiple scenarios
      if (scen == scenarions[1]) eval_df_all <- eval_df else eval_df_all <- bind_rows(eval_df_all, eval_df)
    
    }# end loop according to scenarios
    
    #safe df to list, where all evaluation values are gathered
    eval_list[[i]] <- eval_df_all
    
  }#end loop for k-splits of cross validation
  
  #combine all observations to one data frame
  eval_df_all <- as.data.frame(rbindlist(eval_list))
  
  #calculate the resiudal (observation - prediction)
  eval_df_all$residual <- eval_df_all$observed_value - eval_df_all$model_value
  
  # Add a column for the repetition
  eval_df_all$repetition <- rep
  
  #if first repetition, then create new object to store the residuals, otherwise just add columns to it
  if(rep == 1) eval_df_final <- eval_df_all else eval_df_final <- bind_rows(eval_df_final, eval_df_all)
}

# Save the final data frame after running the loop with 3 repetitions for all the scenarios
write.csv(eval_df_final, "data/re_analysis/cross_validation_raw.csv", row.names = FALSE)

# Load the file from folder

eval_df_final <- read.csv("data/re_analysis/cross_validation_raw.csv")

#summarise residuals, select only columns with residual values
# eval_df_final <- transform(eval_df_final, mean_residual=apply(eval_df_final[,4:(4+repititions-1)],1, mean, na.rm = TRUE))
# eval_df_final <- transform(eval_df_final, sd_residual=apply(eval_df_final[,4:(4+repititions-1)],1, sd, na.rm = TRUE))

# Use tidyverse to summarize the residuals by weather station and scenario (mean, median, and sd)
eval_df_final_summ <- eval_df_final %>% group_by(station_name, CTRY, Longitude, Latitude, scenario) %>% 
  
  summarize(mean_res = mean(residual, na.rm = TRUE),
            median_res = median(residual, na.rm = TRUE),
            sd = sd(residual, na.rm = TRUE))


# The same as above but using only weather stations
eval_df_final_summ_WS <- eval_df_final %>% group_by(station_name, CTRY, Longitude, Latitude) %>% 
  
  summarize(mean_res = mean(residual, na.rm = TRUE),
            median_res = median(residual, na.rm = TRUE),
            sd = sd(residual, na.rm = TRUE))

#get general stats of residuals
summary(eval_df_final_summ_WS)

# Make a hist to see extreme values
hist(eval_df_final_summ_WS$median_res)

# Check the location of extreme values
eval_df_final_summ_WS[order(eval_df_final_summ_WS$median_res), ]
eval_df_final_summ_WS[order(eval_df_final_summ_WS$median_res, decreasing = TRUE), ]


# eval_melt <- melt(as.data.table(eval_df_final), id.vars = c('station_name','CTRY','X1981','Latitude','Longitude'))
# eval_melt %>%
#   filter(!(variable %in% c('mean_resiudal','sd_residual')))%>%
#   ggplot(aes(y=value,x=CTRY))+
#   geom_boxplot()+
#   theme_bw()+
#   ylab('Residual (Observation - Prediction)')+
#   xlab('Country')
# 
# eval_melt %>%
#   filter((variable == 'mean_residual'))%>%
#   ggplot(aes(y=value,x=CTRY))+
#   geom_boxplot()+
#   theme_bw()+
#   ylab('Mean Residual (Observation - Prediction)')+
#   xlab('Country')
# 
# #save also absolute resiudal value for size of bubbles in plot
# eval_df_final$abs_residual <- abs(eval_df_final$mean_residual)


# Plot the residuals by weather station
eval_df_final_sp <-  SpatialPointsDataFrame(eval_df_final_summ_WS[, c("Longitude", "Latitude")],
                                            proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                            data = eval_df_final_summ_WS[, -(which(colnames(eval_df_final_summ_WS) %in% 
                                                                            c("Longitude", "Latitude")))])
# Identify the NAs from the cross validation procedure
NAs_cross_validation <- as.data.frame(eval_df_final_sp[which(is.na(eval_df_final_sp$median_res)), ])[["station_name"]]

# Increase the coverage of the plot to fit the legends inside
b <- bbox(Porig)
b[1, ] <- c(-80.5, -15)
b[2, ] <- c(-55, 12)
b <- bbox(t(b))

chill_residual <- tm_shape(SA, bbox = b) +
  tm_fill(col = 'grey10') +
  tm_shape(SA, bbox = b) +
  tm_borders(col = 'grey40') +
  tm_graticules(lines = FALSE, labels.size = 0.6, labels.col = "black") +
  tm_shape(eval_df_final_sp) +
  tm_symbols(col = "median_res", size = 0.0001, legend.col.show = FALSE,
             legend.hist = TRUE, palette = get_brewer_pal("RdYlBu", n = 30), midpoint = 0,
             breaks = seq(-40, 10, 5), legend.hist.title = "Histogram of residuals", legend.hist.z = 4) +
  tm_shape(eval_df_final_sp) +
  tm_bubbles(col = 'median_res', size = 'sd', palette = get_brewer_pal("RdYlBu", n = 30),
             midpoint = 0, style = "cont", breaks = seq(-40, 10, 5), legend.col.reverse = TRUE,
             title.size = "SD residual", title.col = "Median residual",
             legend.format = list(suffix = " CP", text.align = "center"), legend.col.z = 2,
             legend.size.z = 1, border.col = "grey10") + 
  tm_shape(Porig[which(Porig$station_name %in% NAs_cross_validation), ]) +
  tm_symbols(size = 0.2, shape = 4, col = 'firebrick') +
  tm_add_legend(type = "symbol", labels = "Missing", col = "firebrick", shape = 4, size = 0.5, z = 3) +
  tm_compass(position = c(0.92, 0.92), text.size = 0.5) +
  tm_scale_bar(position = c(0.57, 0.935), bg.color = 'transparent', text.size = 0.5, color.dark = "grey20") +
  tm_layout(legend.outside = F,
            outer.margins = c(0.01, 0.01, 0.01, 0.01),
            legend.title.size = 0.7,
            legend.text.size = 0.5,
            legend.hist.width = 0.3,
            legend.hist.size = 0.5,
            legend.position = c(0.7, 0.01),
            bg.color = "black",
            attr.color = "white")

chill_residual

tmap_save(chill_residual, 'figures/final_figures/figure_7_b.png',
          height = 11, width = 11, units = 'cm')
