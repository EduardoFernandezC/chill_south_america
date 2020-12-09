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

####interpolation methods were taken from https://mgimond.github.io/Spatial/interpolation-in-r.html
####also from Luedeling and Benmoussa 2020 but their code looks suspiciously the same as the websites (with fitting coefficients and comments for completely different dataset)


#read station coordinates with the projected chill (future and historic)
stations <- read.csv('data/all_chill_projections.csv')

#change column name from 'Altitude' to 'Elevation', so that it is coherent
colnames(stations)[2] <- 'Elevation'

#set the planar projection mode as a proj4 string code
#projection method: ESRI:102033: South America Albers Equal Area Conic taken from: https://spatialreference.org/ref/esri/?search=south+america&srtext=Search
projection_string <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"

#change stations to spatial format with original projection
Porig<-SpatialPointsDataFrame(stations[,c("Longitude","Latitude")],
                              proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                              data=stations)

#change projection to planar mode
P <- spTransform(Porig, CRS(projection_string))

#read shape file of south america with original projection
Worig<-readOGR('data/Lars/shape/SouthAmerica.shp')

#change the projection format, we need a planar projection mode
W<-spTransform(Worig, CRS(projection_string))

# Replace point boundary extent with that of South America to make sure the interpolation is done for the whole extend of south america
P@bbox <- W@bbox

#test if projections of south america (SA) outline and spatial points match
tm_shape(W) + 
  tm_polygons() + 
  tm_shape(P) + tm_dots(size = 0.2)

#######nearest neighbors polygons after voronoi ######
library(dismo)
library(rgeos)

#create polygon
nn_voronoi <- voronoi(P)

#plot polygons
tm_shape(nn_voronoi)+
  tm_polygons(col = 'X1981', palette=get_brewer_pal("RdYlBu", n = 9, contrast = c(0, 0.75)),
              midpoint = FALSE,title="Safe Winter Chill \n(Chill Portions)",
              style="cont",breaks=c(0:10*10))+
  tm_legend(legend.outside=TRUE)

#I wanted to clip the polygons to the outline of south america, but my computer crashes all the time when doing so



###preparations for more advanced interpolation methods######

# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(W, "regular", n=50000))
names(grd) <- c("Longitude", "Latitude")
coordinates(grd) <- c("Longitude", "Latitude")
gridded(grd) <- TRUE # Create SpatialPixel object
fullgrid(grd) <- TRUE # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(grd) <- proj4string(P)

#save scenario names to vector
scenarions <- colnames(stations)[6:26]

#chose which data is used for the interpolation (this is where later on the loop is set)
scen <- scenarions[1]

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(X1981 ~ 1, P, newdata=grd, idp=2.0)

# Convert to raster object then clip to South America
r       <- raster(P.idw)
r.m     <- mask(r, W)

# Plot inverse distance weighing
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", midpoint = FALSE,
            title="Safe Winter Chill \n(Chill Portions)") + 
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)


#### ordinary krigging without any elevation information #### 

# Define the 1st order polynomial equation
f.1 <- as.formula(paste(scen, "~ Longitude + Latitude"))

# Compute the sample variogram; note that the f.1 trend model is one of the
# parameters passed to variogram(). This tells the function to create the 
# variogram on the de-trended data.
var.smpl <- variogram(f.1, P)

# Compute the variogram model by passing the nugget, sill and range values
# to fit.variogram() via the vgm() function.
dat.fit  <- fit.variogram(var.smpl, fit.ranges = TRUE, fit.sills = FALSE,
                          vgm(psill=300, model="Sph", range=150000, nugget=25))
#range decides at which point on the x-axis the function plateaus, psill does the same for the y-axis
#'Sph' decides on the shape of the function

# The following plot allows us to have a look on the goodness of fit, if not good, adjust parameters in previous step
plot(var.smpl, dat.fit)

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige( f.1, P, grd, dat.fit)

#have a look at the krigged data
head(dat.krg)
#there are values below zero, is it because the model is shit, or because in that area there are too few observations?
#--> compare predictions with variance

#set every prediction below zero to zero
dat.krg$var1.pred[dat.krg$var1.pred < 0] <- NA

# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
r.m <- mask(r, W)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", midpoint = FALSE, 
            title="Safe Winter Chill \n(Chill Portions)") +
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

#the whole predictions of krigging are completely out of range, there must be a fundamental problem in the previous steps


###### ordinary krigging with the elevation data 

## access elevation dataset for SA extent (takes a while)
dem <- get_elev_raster(locations = W, prj = sp::proj4string(P), z=5)
# resample elevation model to resolution of sampling grid
dem.res<-resample(dem,raster(grd))


####this part here is just to make a map of the elevation data to have a look at it #####

#chance values below zero to na, so that sea is not plotted
dem.res[dem.res < 0] <- NA

#use colours for more beautiful plotting, taken from https://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
col_frame <- c('#d73027','#f46d43','#fdae61','#fee08b','#ffffbf','#d9ef8b','#a6d96a','#66bd63','#1a9850')

#plot the elevation map
tm_shape(dem.res) + 
  tm_raster(n = length(col_frame), palette = rev(col_frame), midpoint = 100,
            title="Elevation \n(m above sea level)",
            breaks=c(0:9*100)) + 
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)


### at first there is a krigging for the elevation

## produce interpolated layer from elevations of all station locations
f.elevation<-as.formula(Elevation ~ Longitude + Latitude)

var.smpl.elevation <- variogram(f.elevation, P, cloud = FALSE)
dat.fit.elevation <- fit.variogram(var.smpl.elevation, fit.ranges = FALSE,
                                   fit.sills = FALSE,
                                   vgm(model="Sph", range = 2000000, psill = 500000))

#check the variogram
plot(var.smpl.elevation, dat.fit.elevation)

#do the krigging
dat.krg.elevation <- krige( f.elevation, P, grd, dat.fit.elevation)

#create raster out of the interpolation data
r.elevation<-raster(dat.krg.elevation)


# Define the krigging model for the chill
f.1 <- as.formula(paste(scen, "~ Longitude + Latitude"))

# Compute the sample variogram; note that the f.1 trend model is one of the
# parameters passed to variogram(). This tells the function to create the
# variogram on the de-trended data.
var.smpl <- variogram(f.1, P, cloud = FALSE)

# Compute the variogram model by passing the nugget, sill and range values
# to fit.variogram() via the vgm() function.
dat.fit <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                         vgm(model="Sph",range = 2000000))

#check the variogram  
plot(var.smpl,dat.fit)

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige( f.1, P, grd, dat.fit)

#assign krigged data to the raster
r_krig<-raster(dat.krg)
r.m <- mask(r_krig, W)

#create model of elevation and chill
pred.model<-lm(stations[,scen]~stations$Elevation)

#check chill dependent on elevation 
plot(stations[,scen] ~ stations$Elevation)
abline(pred.model, col = 'blue')
#--> appears that the relationship of chill and elevation is weak

summary(pred.model)
#coefficient for elevation is not signif. different from zero
#F-statistic the same, so model is not better than just using the mean
#--> nontheless, use it to adjust the kriggin values

#calculate chill based on the linear model with elevation: elevation (from elevation map) * coef elevation + error term
dem.typical.chill<-dem.res*pred.model$coefficients[2]+pred.model$coefficients[1]

#calculate chill with elevation data based on krigging elevation (from krigging of elevation) * coef elevation + error term
dem.int.typical.chill<-r.elevation*pred.model$coefficients[2]+pred.model$coefficients[1]

#correct chill anticipated by output of linear model of actual elevation and by krigged elevation 
r<-max(r_krig+dem.typical.chill-dem.int.typical.chill,0)
r.m <- mask(r, W)

# produce a raster showing the elevation-related chill adjustments
r_elev_correct<-r-r_krig
r.m.elev_correct <- mask(r_elev_correct, W)

#produce map of the corrected chill interpolation r.m
tm_shape(r.m)+
  tm_raster(n=10, palette=get_brewer_pal("RdYlBu", n = 9, contrast = c(0, 0.75)),
            midpoint = FALSE,title="Safe Winter Chill \n(Chill Portions)",
            style="cont",breaks=c(0:9*10))+
  tm_shape(P) + tm_dots(size=0.2)+
  tm_legend(legend.outside=TRUE)
