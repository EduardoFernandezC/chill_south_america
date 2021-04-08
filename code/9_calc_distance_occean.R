#####calculate distance of point to the sea
#####saves distance metric to the stations table

library(rgdal)
library(maptools)
library(gridExtra)
library(tmap)

#read map of south america
Worig<-readOGR('southamerica_chill/chill_south_america/sa_shapefile/SouthAmerica.shp')
#put everything in one group, 
Worig$id <- 'a'

#merge all the country polygons to one big polygon
Wunion <- unionSpatialPolygons(Worig,IDs = Worig$id)

#read climate station data
stations <- read.csv('southamerica_chill/chill_south_america/data/all_chill_projections.csv')
#transform to spatial data frame
Porig<-SpatialPointsDataFrame(stations[,c("Longitude","Latitude")],
                              proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                              data=stations[,c(2,6:27)])
#check if the union worked
tm_shape(Worig) + 
  tm_polygons() + 
  tm_shape(Porig) + tm_dots(size = 0.2)+
  tm_shape(Porig[c(75,84,89,88),])+ tm_dots(size = 0.2, col = 'red')

#calculate distance to outline of polygon, is provided in meters
dist.mat <- geosphere::dist2Line(p = Porig, line = Wunion)

#add distance to data frame
stations_df <- cbind(stations, dist.mat[,1])

#drop row name
stations_df <- stations_df[ , -which(names(stations_df) %in% c('X'))]

#change column name to distance
colnames(stations_df)[31] <- 'distance_occean'

write.csv(stations_df,'southamerica_chill/chill_south_america/data/all_chill_projections.csv',
          row.names = F)

write.csv(dist.mat,'southamerica_chill/chill_south_america/data/distances_occean.csv', row.names = F)

hist(stations_df$distance_occean/1000)
