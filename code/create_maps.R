library(rgdal)
library(tmap)
library(spatstat) # Used for the dirichlet tessellation function
library(maptools) # Used for conversion from SPDF to ppp
library(raster) # Used to clip out thiessen polygons
library(gstat) # Use gstat's idw routine
library(sp) # Used for the spsample function
require(elevatr)
require(tmaptools)
library(ggplot2)

#read station coordinates with the projected chill (future and historic)
stations <- read.csv('southamerica_chill/chill_south_america/data/all_chill_projections.csv')

colnames(stations)[2] <- 'Elevation'

#bring weather station coordinates to the right projection format
P<-SpatialPointsDataFrame(stations[,c("Latitude","Longitude")],
                          proj4string=CRS("+proj=lcc +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"),
                          data=stations)

Worig<-readOGR('southamerica_chill/chill_south_america/sa_shapefile/SouthAmerica.shp')

#change the projection format, we need a planar projection mode: South America 1969 Lambert Conformal Conic
W<-spTransform(Worig, CRS("+proj=lcc +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"))


## Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(W, "regular", n=50000))
names(grd) <- c("Longitude", "Latitude")
coordinates(grd) <- c("Longitude", "Latitude")
gridded(grd) <- TRUE # Create SpatialPixel object
fullgrid(grd) <- TRUE # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(grd) <- proj4string(P)

## access elevation dataset for SA extent (takes a while)
dem <- get_elev_raster(locations = W, prj = sp::proj4string(P), z=5)
# resample elevation model to resolution of sampling grid
dem.res<-resample(dem,raster(grd))


## produce interpolated layer from elevations of all station locations
f.elevation<-as.formula(Elevation ~ Longitude + Latitude)

var.smpl.elevation <- variogram(f.elevation, P, cloud = FALSE)
dat.fit.elevation <- fit.variogram(var.smpl.elevation, fit.ranges = FALSE,
                                   fit.sills = FALSE,
                                   vgm(model="Sph", range = 20, psill = 500000))

plot(var.smpl.elevation, dat.fit.elevation)

dat.krg.elevation <- krige( f.elevation, P, grd, dat.fit.elevation)
r.elevation<-raster(dat.krg.elevation)

###plot the maps######


#extract the column names of the projected chill
scenarios <- colnames(stations)[6:27]

scen <- scenarios[1]

#loop over all projections
#for(scen in scenarios)
#{

  # Define the trend model
  f.1 <- as.formula(paste(scen, "~ Longitude + Latitude"))
  
  # Compute the sample variogram; note that the f.1 trend model is one of the
  # parameters passed to variogram(). This tells the function to create the
  # variogram on the de-trended data.
  var.smpl <- variogram(f.1, P, cloud = FALSE)
  
  # Compute the variogram model by passing the nugget, sill and range values
  # to fit.variogram() via the vgm() function.
  dat.fit <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                           vgm(model="Sph",nugget =25, range = 20, psill = 300))
  
  plot(var.smpl,dat.fit)
  
  # Perform the krige interpolation (note the use of the variogram model
  # created in the earlier step)
  dat.krg <- krige( f.1, P, grd, dat.fit)
  r_krig<-raster(dat.krg)
  r.m <- mask(r_krig, W)
  pred.model<-lm(stations[,scen]~stations$Elevation)
  dem.typical.chill<-dem.res*pred.model$coefficients[2]+pred.model$coefficients[1]
  dem.int.typical.chill<-r.elevation*pred.model$coefficients[2]+pred.model$coefficients[1]
  r<-max(r_krig+dem.typical.chill-dem.int.typical.chill,0)
  r.m <- mask(r, W)
  # produce a raster showing the elevation-related chill adjustments
  r_elev_correct<-r-r_krig
  r.m.elev_correct <- mask(r_elev_correct, W)
  # Plot the map
#  if(scen %in% c("Year_1980","Int_rcp45_2085"))#,"Opt_rcp45_2055","Int_rcp45_2055"))
#  {
  tm_shape(r.m)+
    tm_raster(n=10, palette=get_brewer_pal("RdYlBu", n = 9, contrast = c(0, 0.75)),
              midpoint = FALSE,title="Safe Winter Chill \n(Chill Portions)",
              style="cont",breaks=c(0:9*10))+
    tm_shape(P) + tm_dots(size=0.2)
  

  tm_shape(r.m) +
    tm_raster(n=10, palette=get_brewer_pal("RdYlBu", n = 9, contrast = c(0, 0.75)),
              midpoint = FALSE,title="Safe Winter Chill \n(Chill Portions)",
              style="cont",breaks=c(0:9*10))+
    tm_shape(P) + tm_dots(size=0.2)+
    tm_legend(legend.outside=TRUE)+ 
    #tm_text("Station",size=1.8,ymod=-0.7,auto.placement = FALSE)+
    #tm_text(scen,size=1.7,ymod=-1.8,legend.format=list(digits=1)) 
    tm_scale_bar(breaks=c(0,1000,2000),text.size=1,position="left")+
    tm_compass(text.size=1)  

  
  
  
######## Try out some stuff 

#define the model
  scen <- scenarios[1]
  f.1 <- as.formula(paste(scen, "~ Longitude + Latitude"))
  
#set up variogram
  var.smpl <- variogram(f.1, P, cloud = FALSE)
  dat.fit <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                           vgm(model="Sph",nugget =25, range = 20, psill = 300))
  
  plot(var.smpl,dat.fit)
  
  lzn.kriged <- krige(f.1, P, grd, model=dat.fit)

  lzn.kriged %>% as.data.frame %>%
    ggplot(aes(x=Longitude, y=Latitude)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
    scale_fill_gradient(low = "red", high="blue") +
    theme_bw()
#--> very weird form, looks like only one data point is taken into account, also the krigging results in rediculus values
  #original values were in range 0 - 90 but now if got values > 40,000, this doesn't make sense
  #were is the error?
