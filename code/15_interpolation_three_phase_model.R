#does spatial interpolation with "three-phases model"
#maps were not used in the final report and map appearances were not updated

library(rgdal)
library(tmap)
library(spatstat) # Used for the dirichlet tessellation function
library(maptools) # Used for conversion from SPDF to ppp
library(raster) # Used to clip out thiessen polygons
library(gstat) # Use gstat's idw routine
library(sp) # Used for the spsample function
require(tmaptools)
library(ggplot2)
library(gridExtra)

#read south america outline
SA <- readOGR('data/sa_outline/SA_outline.shp')

#read station coordinates with the projected chill (future and historic)
stations <- read.csv('data/all_chill_projections.csv')

Porig<-SpatialPointsDataFrame(stations[,c("Longitude","Latitude")],
                              proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                              data=stations[,c(2, 5 : ncol(stations))])


#read shape file of south america with original projection
#Worig<-readOGR('southamerica_chill/chill_south_america/sa_shapefile/SouthAmerica.shp')

# Replace point boundary extent with that of South America to make sure the interpolation is done for the whole extend of south america
Porig@bbox <- SA@bbox

# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(SA, "regular", n=50000))
names(grd) <- c("Longitude", "Latitude")
coordinates(grd) <- c("Longitude", "Latitude")
gridded(grd) <- TRUE # Create SpatialPixel object
fullgrid(grd) <- TRUE # Create SpatialGrid object
proj4string(grd) <- proj4string(Porig)

#save scenario names to vector
scenarions <- colnames(stations)[6:28]

#chose which data is used for the interpolation (this is where later on the loop is set)
scen <- scenarions[1]


###### ordinary krigging with mean temp in august 

#read original mean temp in august file
#same file like in script 10
avg_temp_jul <- raster('data/world_clim/wc2/wc2.1_30s_tavg_07.tif')

#set extent to outline of south america
bb <- extent(-109.46044, -26.23419, -59.48714, 12.62908)

#extract south america from world wide map
avg_temp_jul <- crop(avg_temp_jul, bb)

#adjust resolution of temperature map to match the grid of our project
temp.res<-resample(avg_temp_jul,raster(grd))


## produce interpolated layer from avg temp august of all station locations
f.temp<-as.formula(avg_temp_jul ~ Longitude + Latitude)

###here is a big problem: how should we fit the elevation data? 
var.smpl.temp <- variogram(f.temp, Porig)
dat.fit.temp <- fit.variogram(var.smpl.temp, fit.ranges = FALSE,
                                   fit.sills = FALSE,
                                   vgm(model="Sph", range = 2600, psil = 5, nugget = 2))

#check the variogram
#plot(var.smpl.temp, dat.fit.temp)

#do the krigging
dat.krg.temp <- krige( f.temp, Porig, grd, dat.fit.temp)
#tm_shape(dat.krg.temp)+
#  tm_raster()


r_krig<-raster(dat.krg.temp)
r.m <- mask(r_krig, SA)

#calculate a quality map, where you can see the percent difference of krigged temperature to original temperature
temp_diff <- r.m - temp.res
#temp_qual[temp_qual > 100] <- NA
#temp_qual[temp_qual < -100] <- -NA

#check distribution of quality map
#hist(temp_diff) 
#summary(temp_diff)

data(World)#for map of south america

#size of figures which are saved
height <- 13
width <- 12

#plot original temperature
#jpeg(file='original_temp_august.jpg', width = 700, height = 500)
 
org_temp <- tm_shape(SA) +
  tm_fill()+
  tm_shape(temp.res)+
  tm_raster(n = 7, midpoint = 10, palette = get_brewer_pal("-RdBu", n = 9, contrast = c(0, 0.75)),
            style = "cont", breaks = seq(-15, 35, by = 5),
            title = "Original Mean Temperature\n in July (°C)") +
  tm_shape(Porig) + tm_symbols(size=0.2,shape = 4,col = 'black')+
  tm_shape(SA)+
  tm_borders(col='black')+
  tm_graticules(lines =F)+
  tm_compass(position = c(0.64,0.1))+
  tm_scale_bar(position = c(0.58,0.01),bg.color = 'white')+
  tm_layout(legend.outside=T,outer.margins = c(0.001,0.001,0,0.001))
org_temp
tmap_save(org_temp, filename = 'figures/interpolation/original_tavg_jul.jpg',
          height = height, width = width, units = 'cm')  


#plot krigged temperature
krig_temp <- tm_shape(SA) +
  tm_fill()+  
  tm_shape(r.m)+
  tm_raster(midpoint = 10,palette=get_brewer_pal("-RdBu", n = 9, contrast = c(0, 0.75)),
            breaks = seq(-15,35,by=5),title="Krigged Mean Temperature\nin July (°C)",
            style = "cont")+
  tm_shape(Porig) + tm_symbols(size=0.2,shape = 4,col = 'black')+
  tm_shape(SA)+
  tm_borders(col='black')+
  tm_graticules(lines =F)+
  tm_compass(position = c(0.64,0.1))+
  tm_scale_bar(position = c(0.58,0.01),bg.color = 'white')+
  tm_layout(legend.outside=T,outer.margins = c(0.001,0.001,0,0.001))
krig_temp
tmap_save(krig_temp, filename = 'figures/interpolation/krig_tavg_jul.jpg',
          height = height, width = width, units = 'cm')  

#tmaptools::palette_explorer()

#plot percent difference of krigged to original temperature
#jpeg(file='error_krigging_temp_august.jpg', width = 700, height = 500)
temp_dif <-tm_shape(SA) +
  tm_fill()+
  tm_shape(temp_diff)+
  tm_raster(palette=get_brewer_pal("-RdBu", n = 9, contrast = c(0, 0.75)),
            midpoint = 0,title="Error\nkrig.-orig.(°C)",
            breaks = c(seq(-10,14,by=4),20), style = "cont")+
  tm_shape(Porig) + tm_symbols(size=0.2,shape = 4,col = 'black')+
  tm_shape(SA)+
  tm_borders(col='black')+
  tm_graticules(lines =F)+
  tm_compass(position = c(0.64,0.1))+
  tm_scale_bar(position = c(0.58,0.01),bg.color = 'white')+
  tm_layout(legend.outside=T,outer.margins = c(0.001,0.001,0,0.001))
temp_dif
tmap_save(temp_dif, filename = 'figures/interpolation/difference_tavg_jul.jpg',height = height,width=width,units = 'cm')  



#plot chill against the mean temperature in july
ggplot(stations,aes(x = avg_temp_jul, y = X1981)) +
  ylab('Chill Portion')+
  coord_cartesian(ylim = c(0,100))+
  geom_point()+
  geom_vline(xintercept=c(8,17))+
  theme_bw()

#hand-defined breaks for three phase model
upper_break = 20
lower_break = 8

#subset of original station data according to breakes
low <- subset(stations, stations$avg_temp_jul <= lower_break)
mid <- subset(stations, stations$avg_temp_jul > lower_break & stations$avg_temp_aug <upper_break)
up <- subset(stations, stations$avg_temp_jul >= upper_break)

#create model for the subset of chill explained by mean temperature in august
model_low <- lm(data = low, X1981~avg_temp_jul)
model_mid <- lm(data = mid, X1981~avg_temp_jul)
model_up <- lm(data = up, X1981~avg_temp_jul)


#set up function to automatically calculate the modeled chill by mean temperature in august
#function is defined for individual cell value, so it needs to be used in loop or in apply function
#input: set breaks, the models and the temperature map as a grid
#output: modelled chill as a matrix!

calc_model_val <- function(x, upper_break, lower_break, model_low, model_mid){
  #at first, check if the raster value is nan or na, if so return the same
  if(is.na(x) == T){
    return(NA)
  } else if (is.nan(x) == T){
    return(NaN)
  }
  #now check to which of the three phases the value belongs to and calculate modelled chill
  if(x <= lower_break){
      return(x * model_low$coefficients[2] + model_low$coefficients[1])
  } else if(x >= upper_break){
    return(0)
  } else {
    out <- (x * model_mid$coefficients[2] + model_mid$coefficients[1])
    if(out < 0){
      return(0)
    } else {
      return(out)
    }
  }
}

#test function for single value
calc_model_val(x=NaN,upper_break = upper_break, lower_break = lower_break, 
               model_low = model_low, model_mid = model_mid)

#test function on a matrix
my.matrx <- matrix(c(1:5, 6:10, 11:15, 16:20), nrow = 5, ncol = 4)
my_res <- apply(my.matrx, 1 : 2, calc_model_val, upper_break, lower_break, model_low, model_mid)

#save number of rows and columns, so that the gridded data is presented correctly as a matrix
no_row <- nrow(r.m)
no_col <- ncol(r.m)

#test function on real matrix
#krigged temp
model_krigged_temp <- apply(matrix(r.m, nrow = no_row, ncol = no_col,byrow = T),1:2,calc_model_val, upper_break, lower_break, model_low, model_mid)
#real temp
model_real_temp <- apply(matrix(temp.res,nrow = no_row, ncol = no_col, byrow = T),1:2, calc_model_val, upper_break, lower_break, model_low, model_mid)

#calculate the adjustment (so the chill, which so far was not capured by krigging)
model_adjust <- model_real_temp - model_krigged_temp
#exlcude adjustments which would decrease the chill

#how can I transform the matrix back to a grid?
raster_model_adjust <- raster(model_adjust)
raster_model_adjust <- setExtent(raster_model_adjust,bb)
crs(raster_model_adjust) <- crs(r.m)

tm_shape(raster_model_adjust)+
  tm_raster()+
  tm_shape(SA)+
  tm_borders()

#something is wrong here. the adjustment layer looks completely distorted
chill_adjust <- tm_shape(SA)+
  tm_fill()+
  tm_shape(raster_model_adjust)+
  tm_raster(n=10, palette=get_brewer_pal("RdBu", contrast = c(0, 0.75)),
            midpoint = 0,title="Correction of\n winter chill (Chill Portions)",
            style="cont")+
  tm_shape(Porig) + tm_dots(size=0.2)+
  tm_legend(legend.outside=TRUE)
chill_adjust
#tmap_save(temp_dif, filename = 'figures/interpolation/chill_adjustment.jpg',height = height,width=width,units = 'cm')  



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
plot(var.smpl,dat.fit)

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg.chil <- krige(f.1, Porig, grd, dat.fit)

#assign krigged data to the raster
r_krig<-raster(dat.krg.chil)
r.m.chill <- mask(r_krig, SA)
r.m.chill<-max(r.m.chill,0)

raster_model_adjust <- setExtent(raster_model_adjust, extent(r.m.chill)) 

#adjust the chill portions, prevent that chill portions become lower than zero
r<-max(r.m.chill+raster_model_adjust,0)
r.m <- mask(r, SA)


final_chill <- tm_shape(r.m)+
  tm_raster(n=10, palette=get_brewer_pal("RdBu", contrast = c(0, 0.75)),
            midpoint = 0,title="Safe Winter Chill \n(Chill Portions) \nCorrected for Temp.",
            style="cont")+
  tm_shape(Porig) + tm_dots(size=0.2)+
  tm_graticules(lines =F)+
  tm_compass(position = c(0.64,0.1))+
  tm_scale_bar(position = c(0.58,0.01),bg.color = 'white')+
  tm_layout(legend.outside=T,outer.margins = c(0.001,0.001,0,0.001))

tmap_save(final_chill, filename = paste0('figures/interpolation/adjusted_chill_three_phases_', scen, '.jpg'),
          height = height,width=width,units = 'cm')  


#have a look at the region with high density of stations
b <- bbox(avg_temp_jul)
b[1,] <- c(-75,-65)
b[2,] <- c(-40,-25)
b <- bbox(t(b))

tm_shape(r.m,bbox = b)+
  tm_raster(n=10, palette=get_brewer_pal("RdBu", contrast = c(0, 0.75)),
            midpoint = 50,title="Safe Winter Chill \n(Chill Portions) \nCorrected for Temp.",
            style="cont")+
  tm_shape(Porig) + tm_dots(size=0.2)+
  tm_grid()+
  tm_legend(legend.outside=TRUE)



orig_krig_chill <- tm_shape(r.m.chill)+
  tm_raster(palette=get_brewer_pal("RdBu", contrast = c(0, 0.75)),
            midpoint = 30,title=paste(scen,"\nSafe Winter Chill \n(Chill Portions)\nUncorrected",sep = ''),
            breaks=seq(0,100,by=10), style = "cont")+
  tm_shape(Porig) + tm_dots(size=0.2,shape = 4,col = 'black')+
  tm_shape(SA)+
  tm_borders()+
  tm_graticules(lines =F)+
  tm_compass(position = c(0.64,0.1))+
  tm_scale_bar(position = c(0.58,0.01),bg.color = 'white')+
  tm_layout(legend.outside=T,outer.margins = c(0.001,0.001,0,0.001))
orig_krig_chill

tmap_save(orig_krig_chill,
          filename = paste0('figures/interpolation/chill_kriged_uncorrected_', scen, '.jpg'),
          height = height,width=width,units = 'cm')  


