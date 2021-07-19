#####calculate distance of point to the sea
#####saves distance metric to the stations table

library(rgdal)
library(maptools)
library(gridExtra)
library(tmap) #used for mapping function, contains also a data set with the outline of the world's countries
library(sf) #use as_spatial function
library(raster) #use for extent function
library(maptools) #still not sure, maybe to write a simplified SA outline as a shape file

#read climate station data
stations <- read.csv('data/re_analysis/all_chill_projections.csv')

#transform to spatial data frame
Porig<-SpatialPointsDataFrame(stations[,c("Longitude","Latitude")],
                              proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                              data=stations[,c(5 : 28)])

###prepare shapefile of SA, convert it to the right format and save it 

data(World) #read dataset from tmap
#select countries for SA outline, include france because of french guyana
countries <- c('FRA','ARG', 'BOL', 'BRA', 'CHL', 'COL', 'ECU',  'GUY', 'PER', 'PRY', 'SUR', 'URY', 'VEN')
SA <- World[World$iso_a3 %in% countries,]
SA_extent <- extent(c(-82,-30,-67,17)) #set the boundaries of SA
SA <- as_Spatial(SA)
SA <- crop(SA, SA_extent) #findout which package is required for this one
SA <- spTransform(SA, CRSobj = crs(Porig))
#remove the area column because it caused trouble when saving the shape file
SA$area <- NA
#savethe shapefile
shapefile(SA, filename='data/sa_outline/SA_outline_2.shp', overwrite = TRUE)

#read the saved shapefile to see if it works
SA <- readOGR('data/sa_outline/SA_outline.shp')

#try plotting it
tm_shape(SA) + 
  tm_borders(col = 'black')+
  tm_shape(Porig) + 
  tm_dots(size = 0.2)+
  tm_grid()


#give every polygon the same id so they can be merged. mergin is needed to calculate the distance to the coastline 
#(=border) of the south american continent
SA$id <- 'a'

#merge all the country polygons to one big polygon
#Wunion <- unionSpatialPolygons(Worig,IDs = Worig$id)
SA_union <- unionSpatialPolygons(SA,IDs = SA$id)

tm_shape(SA_union) + 
  tm_borders(col = 'black') + 
  tm_shape(Porig) + tm_dots(size = 0.2)

#calculate distance to outline of polygon, is provided in meters
dist.mat <- geosphere::dist2Line(p = Porig, line = SA_union)

#add distance to data frame
stations_df <- cbind(stations, dist.mat[,1])

#drop row name
stations_df <- stations_df[ , -which(names(stations_df) %in% c('X'))]

#change column name to distance
colnames(stations_df)[30] <- 'distance_occean'

#save update stations dataframe
write.csv(stations_df,'data/all_chill_projections.csv',
          row.names = F)

#most stations are less than 200km away from the coastline
hist(stations_df$distance_occean/1000)
