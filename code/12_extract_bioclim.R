#extract other climatic varibales from world clim like annual temperature range or seasonality

library(rgdal)
library(tmap)
library(spatstat) # Used for the dirichlet tessellation function
library(maptools) # Used for conversion from SPDF to ppp
library(raster) # Used to clip out thiessen polygons
library(sp) # Used for the spsample function
library(rgeos)

stations <- read.csv('data/all_chill_projections.csv')
#somehow I introduced an error to the stations file but I don't know how and why
#it contains a lot of empty rows, so I'll remove them, also there is a column 'X' which is not needed
stations <- stations[1:157,]
drop <- c('X')
stations <- stations[,-which(names(stations) %in% drop)]


Porig<-SpatialPointsDataFrame(stations[,c("Longitude","Latitude")],
                              proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                              data=stations[,c(2,6:27)])


#bioclimatic variables, downloaded from 
#https://www.worldclim.org/data/worldclim21.html
#I used now the low resolution version because my internet is kinda slow, but the high resolution folder would be better in general
mean_diurnal_range <- raster('D:/chil/wc2.1_10m_bio/wc2.1_10m_bio_2.tif')

#have a look at the data
b <- bbox(mean_diurnal_range)
b[1,] <- c(-85,-30)
b[2,] <- c(-60,15)
b <- bbox(t(b))
tm_shape(mean_diurnal_range, bbox = b) + 
  tm_raster(breaks = seq(0,25,by=2.5))+
  tm_legend(legend.outside=TRUE)+
  tm_shape(Porig)+
  tm_dots(size = 0.2)

#extract the data
stations$mean_diurnal_range <- extract(mean_diurnal_range,Porig)

seasonality <- raster('D:/chil/wc2.1_10m_bio/wc2.1_10m_bio_4.tif')
stations$seasonality <- extract(seasonality,Porig)

temp_annual_range <- raster('D:/chil/wc2.1_10m_bio/wc2.1_10m_bio_7.tif')
stations$temp_annual_range <- extract(temp_annual_range,Porig)

write.csv(stations,'data/all_chill_projections.csv', row.names = F)
