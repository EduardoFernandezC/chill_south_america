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

#read files of avg temp per month
avg_temp_jan <- raster('wc2.1_30s_tavg/wc2.1_30s_tavg_01.tif')
avg_temp_feb <- raster('wc2.1_30s_tavg/wc2.1_30s_tavg_02.tif')
avg_temp_mar <- raster('wc2.1_30s_tavg/wc2.1_30s_tavg_03.tif')
avg_temp_apr <- raster('wc2.1_30s_tavg/wc2.1_30s_tavg_04.tif')
avg_temp_may <- raster('wc2.1_30s_tavg/wc2.1_30s_tavg_05.tif')
avg_temp_jun <- raster('wc2.1_30s_tavg/wc2.1_30s_tavg_06.tif')
avg_temp_jul <- raster('wc2.1_30s_tavg/wc2.1_30s_tavg_07.tif')
avg_temp_aug <- raster('wc2.1_30s_tavg/wc2.1_30s_tavg_08.tif')
avg_temp_sep <- raster('wc2.1_30s_tavg/wc2.1_30s_tavg_09.tif')
avg_temp_oct <- raster('wc2.1_30s_tavg/wc2.1_30s_tavg_10.tif')
avg_temp_nov <- raster('wc2.1_30s_tavg/wc2.1_30s_tavg_11.tif')
avg_temp_dec <- raster('wc2.1_30s_tavg/wc2.1_30s_tavg_12.tif')

min_temp_may <- raster('E:/chil/wc2.1_30s_tmin/wc2.1_30s_tmin_05.tif')
min_temp_jun <- raster('E:/chil/wc2.1_30s_tmin/wc2.1_30s_tmin_06.tif')
min_temp_jul <- raster('E:/chil/wc2.1_30s_tmin/wc2.1_30s_tmin_07.tif')
min_temp_aug <- raster('E:/chil/wc2.1_30s_tmin/wc2.1_30s_tmin_08.tif')

max_temp_may <- raster('E:/chil/wc2.1_30s_tmax/wc2.1_30s_tmax_05.tif')
max_temp_jun <- raster('E:/chil/wc2.1_30s_tmax/wc2.1_30s_tmax_06.tif')
max_temp_jul <- raster('E:/chil/wc2.1_30s_tmax/wc2.1_30s_tmax_07.tif')
max_temp_aug <- raster('E:/chil/wc2.1_30s_tmax/wc2.1_30s_tmax_08.tif')

#adjust boundig box for plotting
b <- bbox(avg_temp_jan)
b[1,] <- c(-85,-35)
b[2,] <- c(-55,15)
b <- bbox(t(b))

#check for one example map and points
tm_shape(avg_temp_jan, bbox = b) + 
  tm_raster(breaks = c(-10,0, 5, 10, 15, 20, 25,40)) +
  tm_shape(Porig) + tm_dots(size = 0.2)+
  tm_legend(legend.outside=TRUE)

#extract avg temp per month from the files to the station df
stations$avg_temp_jan <- extract(avg_temp_jan,Porig)
stations$avg_temp_feb <- extract(avg_temp_feb,Porig)
stations$avg_temp_mar <- extract(avg_temp_mar,Porig)
stations$avg_temp_apr <- extract(avg_temp_apr,Porig)
stations$avg_temp_may <- extract(avg_temp_may,Porig)
stations$avg_temp_jun <- extract(avg_temp_jun,Porig)
stations$avg_temp_jul <- extract(avg_temp_jul,Porig)
stations$avg_temp_aug <- extract(avg_temp_aug,Porig)
stations$avg_temp_sep <- extract(avg_temp_sep,Porig)
stations$avg_temp_oct <- extract(avg_temp_oct,Porig)
stations$avg_temp_nov <- extract(avg_temp_nov,Porig)
stations$avg_temp_dec <- extract(avg_temp_dec,Porig)

stations$min_temp_may <- extract(min_temp_may,Porig)
stations$min_temp_jun <- extract(min_temp_jun,Porig)
stations$min_temp_jul <- extract(min_temp_jul,Porig)
stations$min_temp_aug <- extract(min_temp_aug,Porig)

stations$max_temp_may <- extract(max_temp_may,Porig)
stations$max_temp_jun <- extract(max_temp_jun,Porig)
stations$max_temp_jul <- extract(max_temp_jul,Porig)
stations$max_temp_aug <- extract(max_temp_aug,Porig)

#save updated stations-df
write.csv(stations,'southamerica_chill/chill_south_america/data/all_chill_projections.csv',
          row.names = F)
