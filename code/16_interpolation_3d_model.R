#most important script, spatial interpolation using the "3D model"

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



#can I replace the Worig with SA?
SA <- readOGR('data/sa_outline/SA_outline.shp')


#read station coordinates with the projected chill (future and historic)
stations <- read.csv('data/all_chill_projections.csv')

Porig<-SpatialPointsDataFrame(stations[,c("Longitude","Latitude")],
                              proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                              data=stations[,c(2,5:43)])

# Replace point boundary extent with that of South America to make sure the interpolation is done for the whole extend of south america
Porig@bbox <- SA@bbox


#create a polygon of South America which is filled 
data(World)#for map of south america

countries <- c('FRA','ARG', 'BOL', 'BRA', 'CHL', 'COL', 'ECU',  'GUY', 'PER', 'PRY', 'SUR', 'URY', 'VEN')
SA_countries <- World[World$iso_a3 %in% countries,]

#this function fills the south america outline with dashes
SA_sfc <- hatchedLayer(x= SA_countries,mode = "sfc", pattern = 'right2left', density = 5)
#set bounding box for SA
SA_region = extent(c(-8000000,-3000000,-6700000,1700000))
SA_sfc <- as_Spatial(SA_sfc) #convert to S4 object
SA_sfc <- crop(SA_sfc,SA_region) #crop france out of the selection, because it was needed for french guyana
SA_test <- spTransform(SA_sfc,CRSobj = crs(Porig))

#test if the dashed area worked
tm_shape(SA_test)+
  tm_lines(col = "grey50")+
  tm_shape(SA) + 
  tm_borders(col = 'black')+
  tm_shape(Porig) + 
  tm_dots(size = 0.2)+
  tm_grid()


# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(SA, "regular", n=50000))
names(grd) <- c("Longitude", "Latitude")
coordinates(grd) <- c("Longitude", "Latitude")
gridded(grd) <- TRUE # Create SpatialPixel object
fullgrid(grd) <- TRUE # Create SpatialGrid object
proj4string(grd) <- proj4string(Porig)

#save scenario names to vector
scenarions <- colnames(stations)[6:27]

#load tmin and tmax map for july
min_temp_jul <- raster('D:/chil/wc2.1_30s_tmin/wc2.1_30s_tmin_07.tif')
max_temp_jul <- raster('D:/chil/wc2.1_30s_tmax/wc2.1_30s_tmax_07.tif')

#set extent to outline of south america
bb <- extent(-109.46044, -26.23419, -59.48714, 12.62908)

#extract south america from world wide map
min_temp_jul <- crop(min_temp_jul,bb)
max_temp_jul <- crop(max_temp_jul,bb)


#adjust resolution of temperature map to match the grid of our project
temp_min.res<-resample(min_temp_jul,raster(grd))
temp_max.res<-resample(max_temp_jul,raster(grd))

## produce interpolated layer from both temperature maps of all station locations
f.temp_min<-as.formula(min_temp_jul ~ Longitude + Latitude)
f.temp_max<-as.formula(max_temp_jul ~ Longitude + Latitude)

#set up variogram
var.smpl.temp_min <- variogram(f.temp_min, Porig)
var.smpl.temp_max <- variogram(f.temp_max, Porig)

dat.fit.temp_min <- fit.variogram(var.smpl.temp_min, fit.ranges = FALSE,
                              fit.sills = FALSE,
                              vgm(model="Sph", range = 2600, psil = 12, nugget = 3))
dat.fit.temp_max <- fit.variogram(var.smpl.temp_max, fit.ranges = FALSE,
                                  fit.sills = FALSE,
                                  vgm(model="Sph", range = 2600, psil = 5, nugget = 1))

#check the variogram
#plot(var.smpl.temp_min, dat.fit.temp_min)
#plot(var.smpl.temp_max, dat.fit.temp_max)


#do the krigging
dat.krg.temp_min <- krige( f.temp_min, Porig, grd, dat.fit.temp_min)
dat.krg.temp_max <- krige( f.temp_max, Porig, grd, dat.fit.temp_max)

#transform the kriged surface to a raster
r_krig_min<-raster(dat.krg.temp_min)
#only use the rasters within the boundaries of south america
r.m_min <- mask(r_krig_min, SA)
r_krig_max<-raster(dat.krg.temp_max)
r.m_max <- mask(r_krig_max, SA)

#calculate a quality map, where you can see the percent difference of krigged temperature to original temperature
temp_diff_min <- r.m_min - temp_min.res
temp_diff_max <- r.m_max - temp_max.res


#check for outliers in the tmean and remove them
is_outlier <- abs(stations$avg_temp_jul - stations$obs_avg_temp_jul) > 2
stations$outlier <- is_outlier
stations_clean <- stations[!stations$outlier,]

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

#create empty list which is used to store chill values
chill_list <- list()

#set height and width (cm) of maps when maps are saved
height <- 13
width <- 12

for(scen in scenarions){
  
  #krig the tmin tmax data on a plane
  k <- Krig(x=as.matrix(stations_clean[,c("min_temp_jul","max_temp_jul")]),
          Y=stations_clean[scen])
  pred <- predictSurface(k)
  #error <- predictSurfaceSE(k)
  
  #adjust row and column name of object
  colnames(pred$z)<-pred$y
  rownames(pred$z)<-pred$x
  #colnames(error$z)<-error$y
  #rownames(error$z)<-error$x
  
  #melt df
  melted<-melt(pred$z)
  #melted_error <- melt(error$z)
  
  colnames(melted)<-c("min_temp_jul","max_temp_jul","value")
  #colnames(melted_error)<-c("min_temp_jul","max_temp_jul","value")
  
  #plot the grid
  correction_plane <- ggplot(melted,
         aes(x=min_temp_jul,y=max_temp_jul,z=value)) +
    geom_contour_fill(bins=100) +
    scale_fill_gradientn(colours=alpha(matlab.like(15)),
                         name=paste("\nSafe Chill Portions\n[CP]",sep=''), trans = 'reverse') +
    geom_contour(col="black")  +
    geom_point(data=stations,
               aes(x=min_temp_jul,y=max_temp_jul,z=NULL),
               size=0.7) +
    geom_text_contour(stroke = 0.2,size = 2) +
   labs(title = scen)+
    ylab('Maximum Temperature, July [°C]')+
    xlab('Minimum Temperature, July [°C]')+
    theme_bw(base_size=12)
  
  ggsave(plot = correction_plane,filename = paste('figures/interpolation/correction_plane_',scen,'.jpg',sep=''),
         height = 10,width = 15, units = 'cm')
  
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
  
  #to see how many pixels are outside of the correction range
  #test <- t_both_real[!is.na(t_both_real[,1]),]
  #out_test <- sapply(1:nrow(test), function(i) get_chill_correction(test[i,1], test[i,2]))
  #sum(is.na(out_test)) / length(out_test)
  
  
  #extract the model chill for real and kriged tmin and tmax
  model_krigged_temp <- sapply(1:nrow(t_both), function(i) get_chill_correction(t_both[i,1], t_both[i,2]))
  model_real_temp <- sapply(1:nrow(t_both_real), function(i) get_chill_correction(t_both_real[i,1], t_both_real[i,2]))
  
  #test_corr_df <- data.frame('min_temp_jul' = as.vector(mat_real_tmin),'max_temp_jul' = as.vector(mat_real_tmax))
  
  #see where the datapoints are in the correction plane
  #ggplot(melted,
  #       aes(x=min_temp_jul,y=max_temp_jul,z=value)) +
  #  geom_contour_fill(bins=100) +
  #  scale_fill_gradientn(colours=alpha(matlab.like(15)),
  #                       name="Safe Chill Units", trans = 'reverse') +
  #  geom_contour(col="black")  +
  #  geom_text_contour(stroke = 0.2) +
  #  geom_point(data=test_corr_df,
  #             aes(x=min_temp_jul,y=max_temp_jul,z=NULL),
  #             size=0.7, alpha = 0.2) +
  #  theme_bw(base_size=15)
  #--> many points outside the range of the correction plane
  
  #calculate the adjustment (so the chill, which so far was not capured by krigging)
  #problem: model_real_temp contains many NA
  model_adjust <- model_real_temp - model_krigged_temp
  
  model_adjust <- matrix(model_adjust,nrow = no_row, ncol = no_col)
  #model_orig <- matrix(model_real_temp,nrow = no_row, ncol = no_col)
  #model_krig <- matrix(model_krigged_temp,nrow = no_row, ncol = no_col)
  #exlcude adjustments which would decrease the chill
  
  #how can I transform the matrix back to a grid?
  raster_model_adjust <- raster(model_adjust)
  raster_model_adjust <- setExtent(raster_model_adjust,bb)
  crs(raster_model_adjust) <- crs(r.m_min)
  
  #raster_model_orig <- raster(model_orig)
  #raster_model_orig <- setExtent(raster_model_orig,bb)
  #crs(raster_model_orig) <- crs(r.m_min)
  
  #raster_model_krig <- raster(model_krig)
  #raster_model_krig <- setExtent(raster_model_krig,bb)
  #crs(raster_model_krig) <- crs(r.m_min)


  #do interpolation of chill
  # Define the krigging model for the chill
  f.1 <- as.formula(paste(scen, "~ Longitude + Latitude"))
  
  # Compute the sample variogram; note that the f.1 trend model is one of the
  # parameters passed to variogram(). This tells the function to create the
  # variogram on the de-trended data.
  var.smpl <- variogram(f.1, Porig, cloud = FALSE)
  
  # Compute the variogram model by passing the nugget, sill and range values
  # to fit.variogram() via the vgm() function.
  dat.fit <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                           vgm(psill = 275, model="Sph", nugget = 20, range = 1500))
  
  #check the variogram  
  #plot(var.smpl,dat.fit)
  
  # Perform the krige interpolation (note the use of the variogram model
  # created in the earlier step)
  dat.krg.chil <- krige(f.1, Porig, grd, dat.fit)
  
  #assign krigged data to the raster
  r_krig<-raster(dat.krg.chil)
  r.m.chill <- mask(r_krig, SA)
  r.m.chill<-max(r.m.chill,0)
  
  raster_model_adjust <- setExtent(raster_model_adjust, extent(r.m.chill)) 
  #raster_model_krig <- setExtent(raster_model_krig, extent(r.m.chill))
  #raster_model_orig <-  setExtent(raster_model_orig, extent(r.m.chill))
  
  
  #adjust the chill portions, prevent that chill portions become lower than zero
  r<-max(r.m.chill+raster_model_adjust,0)
  r.m <- mask(r, SA)
  
  chill_list <- append(chill_list,r.m)
  

  f_name <- paste('figures/interpolation/adjusted_chill_',scen,'.jpg',sep = '')
  
  chill_map <- tm_shape(SA_test)+
    tm_lines(col='grey')+
  tm_shape(r.m)+
    tm_raster(palette=get_brewer_pal("RdBu", contrast = c(0, 0.75)),
              midpoint = 30,title=paste(scen,"\nSafe Winter Chill \n(Chill Portions)\nCorrected for Tmin, Tmax",sep = ''),
              breaks=seq(0,100,by=10))+
    tm_shape(Porig) + tm_symbols(size=0.2,shape = 4,col = 'black')+
    tm_shape(SA)+
    tm_borders(col='black')+
    tm_graticules(lines =F)+
    tm_compass(position = c(0.64,0.1))+
    tm_scale_bar(position = c(0.58,0.01),bg.color = 'white')+
    tm_layout(legend.outside=T,outer.margins = c(0.001,0.001,0,0.001))
  chill_map
  tmap_save(chill_map, filename = f_name,height = height,width=width,units = 'cm')  
  
  new_seq <- seq(-50,90,by=10)
  f_name <- paste('figures/interpolation/chill_correction_',scen,'.jpg',sep = '')
  chill_correction <- tm_shape(SA_test) +
    tm_lines(col = "grey50") +
    tm_shape(raster_model_adjust)+
    tm_raster(palette=get_brewer_pal("RdBu", contrast = c(0, 0.75)),
              midpoint = 0,title=paste(scen,"\nCorrection of\nwinter chill (Chill Portions)",sep = ''),
              breaks = new_seq)+
    tm_shape(Porig) + tm_symbols(size=0.2,shape = 4,col = 'black')+
    tm_shape(SA)+
    tm_borders(col='black')+
    tm_graticules(lines =F)+
    tm_compass(position = c(0.64,0.1))+
    tm_scale_bar(position = c(0.58,0.01),bg.color = 'white')+
    tm_layout(legend.outside=T,outer.margins = c(0.001,0.001,0,0.001))
  chill_correction
  tmap_save(chill_correction, filename = f_name,height = height,width=width,units = 'cm')  
} #end of loop to create interpolation maps

#########################
###compute change chill map
#########################

#change names in list to scneario names
names(chill_list) <- scenarions


#loop for change 2017 to future scenarios
for(scen in scenarions[11:22]){
  #create file name
  f_name <- paste('figures/interpolation/change_2017_',scen,'.jpg',sep = '')
  
  #split scenario name because so long
  x <- strsplit(scen,split = '_')
  
  change_map <- tm_shape(SA_test)+
    tm_lines(col='grey')+
    tm_shape(chill_list[[scen]]-chill_list[['X2017']])+
    tm_raster(palette=get_brewer_pal("RdBu", contrast = c(0, 0.75)),
              midpoint = 0,title=paste('2017 to ',x[[1]][2],'\n',x[[1]][1],' ',x[[1]][3],'\nChange in chill portions',sep=''),
              breaks = seq(-60,10,by=10))+
    tm_shape(Porig) + tm_symbols(size=0.2,shape = 4,col = 'black')+
    tm_shape(SA)+
    tm_borders(col='black')+
    tm_graticules(lines =F)+
    tm_compass(position = c(0.64,0.1))+
    tm_scale_bar(position = c(0.58,0.01),bg.color = 'white')+
    tm_layout(legend.outside=T,outer.margins = c(0.001,0.001,0,0.001))
  chill_correction
  tmap_save(change_map, filename = f_name,height = height,width=width,units = 'cm')  
  
}

#calculate change 1981 to 2017
scen <- scenarions[1]
f_name <- paste('figures/interpolation/change_2017_',scen,'.jpg',sep = '')

#split scenario name because so long
x <- strsplit(scen,split = '_')

change_map <- tm_shape(SA_test)+
  tm_lines(col='grey')+
  tm_shape(chill_list[['X2017']]-chill_list[[scen]])+
  tm_raster(palette=get_brewer_pal("RdBu", contrast = c(0, 0.75)),
            midpoint = 0,title='1981 to 2017\nChange in chill portions',
            breaks = seq(-60,10,by=10))+
  tm_shape(Porig) + tm_symbols(size=0.2,shape = 4,col = 'black')+
  tm_shape(SA)+
  tm_borders(col='black')+
  tm_graticules(lines =F)+
  tm_compass(position = c(0.64,0.1))+
  tm_scale_bar(position = c(0.58,0.01),bg.color = 'white')+
  tm_layout(legend.outside=T,outer.margins = c(0.001,0.001,0,0.001))
tmap_save(change_map, filename = f_name,height = height,width=width,units = 'cm')  



############
#create table with absolute and change in chill
############

#create data frame with frequencies of absolute CP
#create data frame with frequencies of change in chill

#empty data frame
absolute_chill_df <- data.frame(NULL)

#loop other every entry of the list
for(i in 1:length(chill_list)){
  #subset data frame and remove NAs
  sub_df <- na.omit(as.data.frame(chill_list[[i]])) 
  #create bins of chill
  out <- table(cut(sub_df$layer,breaks = seq(0,120,by=15),include.lowest = T))
  #save binned chill to data frame
  absolute_chill_df <- rbind(absolute_chill_df,out)
  
}
#add names to the dataframe 
absolute_chill_df <- cbind(names(chill_list),absolute_chill_df)
names(absolute_chill_df) <- c('year',names(out))      
#save the table
write.csv(absolute_chill_df,'data/absolute_chill_binned.csv',
          row.names = F)   

#create the same for change in chill
change_chill_list <- list()
for(i in 11:length(chill_list)){
  change <- chill_list[[i]]-chill_list[['X2017']]
  change_chill_list <- append(change_chill_list,change)
}

change <- chill_list[['X2017']]-chill_list[['X1981']]
change_chill_list <- append(change_chill_list,change)
  
names(change_chill_list) <- c(names(chill_list[11:length(chill_list)]),'X2017')

change_chill_df <- data.frame(NULL)

for(i in 1:length(change_chill_list)){
  sub_df <- na.omit(as.data.frame(change_chill_list[[i]])) 
  out <- table(cut(sub_df$layer,breaks = seq(-60,20,by=10)))
  change_chill_df <- rbind(change_chill_df,out)
  
}
change_chill_df <- cbind(names(change_chill_list),change_chill_df)
names(change_chill_df) <- c('year',names(out))      

write.csv(change_chill_df,'data/change_chill_binned.csv',
          row.names = F)