#opens the climate zone map, maps the climate stations onto it and extracts the group to the stations table
#also produces map of climate zones


library(rgdal)
library(tmap)
library(spatstat) # Used for the dirichlet tessellation function
library(maptools) # Used for conversion from SPDF to ppp
library(raster) # Used to clip out thiessen polygons
library(gstat) # Use gstat's idw routine
library(sp) # Used for the spsample function
library(rgeos)
library(colorRamps)
library(tmaptools) #get_brewer_pal function

#read climate map, I stored the file on an external hardrive because it is a bit bigger
#http://www.fao.org/geonetwork/srv/en/metadata.show?id=12246&currTab=simple
Worig<-readOGR('D:/chil/agroecological_zones_sa/wrl_mcli.shp')

#change climate group encoding to factor
Worig$CLIMATE <- as.factor(Worig$CLIMATE)
levels(Worig$CLIMATE)
#create second column with full climate names
Worig$CLIMATE_name <- Worig$CLIMATE
levels(Worig$CLIMATE_name) <- c('Warm Tropics',
                                'Cool Tropics',
                                'Cold Tropics',
                                'Warm Sub-Tropics\n(Summer Rainfall (SR))',
                                'Cool Sub-Tropics (SR)', 
                                'Cold Sub-Tropics (SR)',
                                'Cool Sub-Tropics\n(Winter Rainfall (WR))',
                                'Cold Sub-Tropics(SR)', 
                                'Cool Temperate', 
                                'Cold Temperate',
                                'Transitional Moderately\nCool Sub-Tropics (SR)', 
                                'Moderately Cool Tropics',
                                'Moderately Cool Sub-Tropics', 
                                'Warm Moderately Cool\nSub-Tropics (SR)')

#read station file
stations <- read.csv('data/all_chill_projections.csv')

#change stations to spatial format with original projection
Porig<-SpatialPointsDataFrame(stations[,c("Longitude","Latitude")],
                              proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                              data=stations[,c(2,6:27)])

#change bounding box to only see south america
b <- bbox(Worig)
b[1,] <- c(-85,-35)
b[2,] <- c(-55,15)
b <- bbox(t(b))

#check if stations and climate map projection fit

clim_map <- tm_shape(Worig, bbox = b) + 
  tm_polygons('CLIMATE',palette=get_brewer_pal('Accent', n=14)) + 
  tm_shape(Porig) + tm_dots(size = 0.2)+
  tm_compass(position = c(0.64,0.1))+
  tm_scale_bar(position = c(0.58,0.01),bg.color = 'white')+
  tm_legend(legend.outside=TRUE)
clim_map
tmap_save(clim_map, filename = 'figures/climate_zones.jpg',height = 13,width=12,units = 'cm') 


#extract climate information for the points
P_climate <- extract(Worig,Porig)

#assign extracted information to climate stations df
stations$CLIMATE <- P_climate$CLIMATE
stations$CLIMATE_name <- P_climate$CLIMATE_name

#visuallise points were climate assignment did not work and decide by hand
tm_shape(Worig, bbox = b) + 
  tm_polygons('CLIMATE') + 
  tm_shape(Porig) + tm_dots(size = 0.2)+
  tm_shape(Porig[c(35,72,81),]) + tm_dots(size = 0.2, col = 'red')+
  tm_legend(legend.outside=TRUE)

stations[35,c('CLIMATE','CLIMATE_name')] <- c(as.factor(9), 'Cool Sub-Tropics (SR)')
stations[81,c('CLIMATE','CLIMATE_name')] <- c(as.factor(1), 'Warm Tropics')
stations[72,c('CLIMATE','CLIMATE_name')] <- c(as.factor(13), 'Cool Temperate')
library(ggplot2)

ggplot(stations, aes(x = CLIMATE)) +
  geom_bar()
#most climate stations are in climate 11


write.csv(stations,file = 'data/all_chill_projections.csv')
