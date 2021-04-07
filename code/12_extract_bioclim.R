#extract other climatic varibales from world clim like annual temperature range or seasonality

library(rgdal)
library(tmap)
library(spatstat) # Used for the dirichlet tessellation function
library(maptools) # Used for conversion from SPDF to ppp
library(raster) # Used to clip out thiessen polygons
library(gstat) # Use gstat's idw routine
library(sp) # Used for the spsample function
library(rgeos)

stations <- read.csv('southamerica_chill/chill_south_america/data/all_chill_projections.csv')
Porig<-SpatialPointsDataFrame(stations[,c("Longitude","Latitude")],
                              proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                              data=stations[,c(2,6:27)])

annual_mean_temp <- raster('E:/chil/wc2.1_30s_bio/wc2.1_30s_bio_1.tif')

b <- bbox(annual_mean_temp)
b[1,] <- c(-85,-30)
b[2,] <- c(-60,15)
b <- bbox(t(b))

tm_shape(annual_mean_temp, bbox = b) + 
  tm_raster(breaks = c(-10,0, 5, 10, 15, 20, 25,40))+
  tm_legend(legend.outside=TRUE)

stations$annual_mean_temp <- extract(annual_mean_temp,Porig)

mean_diurnal_range <- raster('E:/chil/wc2.1_30s_bio/wc2.1_30s_bio_2.tif')
stations$mean_diurnal_range <- extract(mean_diurnal_range,Porig)

seasonality <- raster('E:/chil/wc2.1_30s_bio/wc2.1_30s_bio_4.tif')
stations$seasonality <- extract(seasonality,Porig)

temp_annual_range <- raster('E:/chil/wc2.1_30s_bio/wc2.1_30s_bio_7.tif')
stations$temp_annual_range <- extract(temp_annual_range,Porig)

write.csv(stations,'southamerica_chill/chill_south_america/data/all_chill_projections.csv', row.names = F)
