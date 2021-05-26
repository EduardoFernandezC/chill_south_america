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
stations <- read.csv('data/all_chill_projections.csv')

#save scenario names to vector
scenarions <- colnames(stations)[6:28]

#trnasform to spatial dataframe
Porig<-SpatialPointsDataFrame(stations[,c("Longitude","Latitude")],
                              proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                              data=stations[,c(1,5:34,48,50)])

#outline of south america
SA <- readOGR('data/sa_outline/SA_outline.shp')

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
min_temp_jul <- crop(min_temp_jul,bb)
max_temp_jul <- crop(max_temp_jul,bb)

#adjust resolution of temperature map to match the grid of our project
temp_min.res<-resample(min_temp_jul,raster(grd))
temp_max.res<-resample(max_temp_jul,raster(grd))


# data splitting   ---------------------------------

#loop for repitions of repeatead k-fold cross-validation
for(rep in 1:repititions){
  #split data set in k even groups
  split_df <- split(stations, sample(1:k, nrow(stations), replace=T))
  
  #create empty list in which the values are stored
  eval_list <- vector(mode = "list", length = k)
  
  #loop for crossvalidation, i is marking the data frame which is held back to be used as validation
  for(i in 1:k){
    #combine all splits except the one for the training
    train_df <-as.data.frame(rbindlist(split_df[-i]))
    train_df <- SpatialPointsDataFrame(train_df[,c("Longitude","Latitude")],
                                       proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                       data=train_df[,c(1,5:34,48,50)])
    
    eval_df <- split_df[[i]]
    eval_df <- SpatialPointsDataFrame(eval_df[,c("Longitude","Latitude")],
                                      proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                      data=eval_df[,c(1,5:34,48,50)])
    
    
    
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
    
    #check for outliers in the tmean and remove them
    stations_clean <- as.data.frame(subset(train_df, !(outlier_tmin_jul | outlier_tmax_jul)))
    
    #loop for scenarions, right know only for 1981
    scen <- scenarions[2]
    
    #krig the tmin tmax data on a plane
    model_krig <- Krig(x=as.matrix(stations_clean[,c("min_temp_jul","max_temp_jul")]),
                       Y=stations_clean[scen])
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
    
    #extract values from chill map
    eval_df$model_value <- raster::extract(r.m, eval_df)

    #transform to data frame, only use columns of interest (name, country, chil1981, modelvalue)
    eval_df <- as.data.frame(eval_df[,c(1,2,4,34)])
    
    #safe df to list, where all evaluation values are gathered
    eval_list[[i]] <- eval_df
    
  }#end loop for k-splits of cross validation
  
  #combine all observations to one data frame
  eval_df <- as.data.frame(rbindlist(eval_list))
  
  #calculate the resiudal (observation - prediction)
  eval_df$residual <- eval_df$X1981 - eval_df$model_value
  
  #if first repetition, then create new object to store the residuals, otherwise just add columns to it
  if(rep == 1){
    #create new df where the results of the repetitions are saved, discard longitude, latitude, model value
    eval_df_final <- eval_df[,-c(4:6)]
  } else {
    eval_df_final <- cbind(eval_df_final,eval_df$residual)
  }
}

#summarise residuals, select only columns with residual values
eval_df_final <- transform(eval_df_final, mean_residual=apply(eval_df_final[,4:(4+repititions-1)],1, mean, na.rm = TRUE))
eval_df_final <- transform(eval_df_final, sd_residual=apply(eval_df_final[,4:(4+repititions-1)],1, sd, na.rm = TRUE))


#add longitude and latitude to df
eval_df_final <- merge.data.frame(eval_df_final,stations[,c(1,3,4,7)],by=c("station_name","X1981"),all.x = TRUE)

#save results
write.csv(eval_df_final,"data/cross_validation.csv",row.names = F)

#get general stats of residuals
summary(eval_df_final)

eval_melt <- melt(as.data.table(eval_df_final),id.vars=c('station_name','CTRY','X1981','Latitude','Longitude'))
eval_melt %>%
  filter(!(variable %in% c('mean_resiudal','sd_residual')))%>%
  ggplot(aes(y=value,x=CTRY))+
  geom_boxplot()+
  theme_bw()+
  ylab('Residual (Observation - Prediction)')+
  xlab('Country')

eval_melt %>%
  filter((variable == 'mean_residual'))%>%
  ggplot(aes(y=value,x=CTRY))+
  geom_boxplot()+
  theme_bw()+
  ylab('Mean Residual (Observation - Prediction)')+
  xlab('Country')

#save also absolute resiudal value for size of bubbles in plot
eval_df_final$abs_residual <- abs(eval_df_final$mean_residual)


#
eval_df_final <-  SpatialPointsDataFrame(eval_df_final[,c("Longitude","Latitude")],
                       proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                       data=eval_df_final[,c(1:3,9,13)])

chill_residual <- tm_shape(SA)+
  tm_borders(col = 'black')+
  tm_shape(eval_df_final) + 
  tm_bubbles(size = 'abs_residual',col = 'mean_residual', palette=get_brewer_pal("RdBu"),
             midpoint = 0, breaks=seq(-20,10,by=5))+
  tm_grid()+ 
  tm_layout(legend.outside=T,outer.margins = c(0.001,0.001,0,0.001))
chill_residual
tmap_save(chill_residual, filename = 'figures/cross-validation/residual_corrected-krig.jpg',height = 13,width=12,units = 'cm')
