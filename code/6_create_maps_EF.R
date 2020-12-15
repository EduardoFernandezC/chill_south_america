####interpolation methods were taken from https://mgimond.github.io/Spatial/interpolation-in-r.html
####also from Luedeling and Benmoussa 2020 


#read station coordinates with the projected chill (future and historic). Is this safe winter chill?
stations <- read.csv('data/Lars/all_chill_projections.csv')


#set the planar projection mode as a proj4 string code
#projection method: ESRI:102033: South America Albers Equal Area Conic taken from: https://spatialreference.org/ref/esri/?search=south+america&srtext=Search
#projection_string <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"

#change stations to spatial format with original projection
Porig <- SpatialPointsDataFrame(stations[, c("Longitude","Latitude")],
                                proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                data = stations[, c(5 : ncol(stations))])

# #change projection to planar mode
# P <- spTransform(Porig, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#read shape file of south america with original projection
Worig <- readOGR('data/Lars/shape/SouthAmerica.shp')

#change the projection format, we need a planar projection mode
#W<-spTransform(Worig, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Replace point boundary extent with that of South America to make sure the interpolation is done for the whole extend of south america
#Porig@bbox <- Worig@bbox

#test if projections of south america (SA) outline and spatial points match
tm_shape(Worig) + 
  tm_polygons() + 
  tm_shape(Porig) + tm_dots(size = 0.2)



# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(Worig, "regular", n=50000))

grd_2 <- grd

names(grd_2) <- c("Longitude", "Latitude")
coordinates(grd_2) <- c("Longitude", "Latitude")

gridded(grd_2) <- TRUE # Create SpatialPixel object
fullgrid(grd_2) <- TRUE # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(grd_2) <- proj4string(Porig)

#save scenario names to vector
scenarios <- colnames(stations)[6 : ncol(stations)]

#chose which data is used for the interpolation (this is where later on the loop is set)
scen <- scenarios[1]

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(observed ~ 1, Porig, newdata = grd_2, idp=2.0)

# Convert to raster object then clip to South America
r       <- raster(P.idw)
r.m     <- mask(r, Worig)

# Plot inverse distance weighing
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", midpoint = FALSE,
            title="Safe Winter Chill \n(Chill Portions)") + 
  tm_shape(Porig) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)


#### ordinary krigging without any elevation information #### 

# Define the 1st order polynomial equation
f.1 <- as.formula(paste(scen, "~ Longitude + Latitude"))

# Compute the sample variogram; note that the f.1 trend model is one of the
# parameters passed to variogram(). This tells the function to create the 
# variogram on the de-trended data.
var.smpl <- variogram(f.1, Porig)

# Compute the variogram model by passing the nugget, sill and range values
# to fit.variogram() via the vgm() function.
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill = 275, model="Sph", nugget = 20, range = 1500))

#range decides at which point on the x-axis the function plateaus, psill does the same for the y-axis
#'Sph' decides on the shape of the function

# The following plot allows us to have a look on the goodness of fit, if not good, adjust parameters in previous step
plot(var.smpl, dat.fit)

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige( f.1, Porig, grd_2, dat.fit)

#have a look at the krigged data
head(dat.krg)
#there are values below zero, is it because the model is bad, or because in that area there are too few observations?
#--> compare predictions with variance

#set every prediction below zero to zero
dat.krg$var1.pred[dat.krg$var1.pred < 0] <- 0

# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
r.m <- mask(r, Worig)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdYlBu", midpoint = 20, 
            title="Safe Winter Chill \n(Chill Portions)",
            style = "cont") +
  tm_shape(Porig) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)
