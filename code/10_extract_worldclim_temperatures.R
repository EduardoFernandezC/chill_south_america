library(rgdal)
library(tmap)
library(spatstat) # Used for the dirichlet tessellation function
library(maptools) # Used for conversion from SPDF to ppp
library(raster) # Used to clip out thiessen polygons
library(sp) # Used for the spsample function
library(rgeos)

stations <- read.csv('data/re_analysis/all_chill_projections.csv')
Porig<-SpatialPointsDataFrame(stations[,c("Longitude","Latitude")],
                              proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                              data=stations[,c(5:28)])

#read files of avg temp per month from WorldClim
#https://www.worldclim.org/data/worldclim21.html
avg_temp_jul <- raster('data/world_clim/wc2/wc2.1_30s_tavg_07.tif')
avg_temp_aug <- raster('data/world_clim/wc2/wc2.1_30s_tavg_08.tif')

min_temp_jul <- raster('data/world_clim/wc2-2/wc2.1_30s_tmin_07.tif')
min_temp_aug <- raster('data/world_clim/wc2-2/wc2.1_30s_tmin_08.tif')

max_temp_jul <- raster('data/world_clim/wc2-3/wc2.1_30s_tmax_07.tif')
max_temp_aug <- raster('data/world_clim/wc2-3/wc2.1_30s_tmax_08.tif')

#adjust boundig box for plotting
b <- bbox(avg_temp_jul)
b[1,] <- c(-83,-33)
b[2,] <- c(-57,13)
b <- bbox(t(b))

#check for one example map and points
tm_shape(avg_temp_jul, bbox = b) + 
  tm_raster(breaks = c(-10,0, 5, 10, 15, 20, 25,40)) +
  tm_shape(Porig) + tm_dots(size = 0.2)+
  tm_grid()+
  tm_legend(legend.outside=TRUE)

#extract avg temp per month from the files to the station df
stations$avg_temp_jul <- raster::extract(avg_temp_jul,Porig)
stations$avg_temp_aug <- raster::extract(avg_temp_aug,Porig)

stations$min_temp_jul <- raster::extract(min_temp_jul,Porig)
stations$min_temp_aug <- raster::extract(min_temp_aug,Porig)

stations$max_temp_jul <- raster::extract(max_temp_jul,Porig)
stations$max_temp_aug <- raster::extract(max_temp_aug,Porig)

#save updated stations-df
write.csv(stations, 'data/re_analysis/all_chill_projections.csv',
          row.names = F)
