############
#create some plots for the report
############
library(tidyverse)
library(tmaptools)
library(tmap)
library(rgdal)
library(raster)
library(cartography)

stations <- read.csv('southamerica_chill/chill_south_america/data/all_chill_projections.csv')

SA_countries <- borders("world", regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname",
                                             "Colombia", "Venezuela", "Bolivia", "Ecuador", "Chile", "Paraguay",
                                             "Peru", "Guyana"),
                        fill = "white", colour = "black",
                        size = 0.3)


test_sub <- subset(stations, stations$Latitude < -10 & stations$Latitude > -45)
test_sub$lat_group <- cut(test_sub$Latitude,breaks = seq(-10,-45,by=-5))
test_sub$lat_group <- factor(test_sub$lat_group, levels=rev(levels(test_sub$lat_group)))

ggplot()+
  geom_point(data = test_sub,aes(y=X1981,x=min_temp_jul),col = 'black', size = 0.7)+
  geom_smooth(data = test_sub,mapping = aes(y=X1981,x=min_temp_jul), se = FALSE,method = 'lm',col='blue')+
  geom_point(data = test_sub,aes(y=X1981,x=max_temp_jul),col = 'black', size = 0.7)+
  geom_smooth(data = test_sub,mapping = aes(y=X1981,x=max_temp_jul), se = FALSE,method = 'lm', col = 'red')+
  facet_wrap(~lat_group)+
  ylab('Safe Winter Chill [CP]')+
  xlab('Minimum (blue) and Maximum Temperature [°C]')+
  theme_bw()
ggsave('chill_maps/effect_tmin_tmax_latitude.jpg',height = 10,width = 15,units = 'cm')

sub <-stations %>%
  filter(CTRY %in% 'BR')

sub <-stations %>%
  filter(Name %in% c('EDUARDO GOMES INTL'))

ggplot() + SA_countries +  theme_bw() +
  geom_point(aes(Longitude, Latitude), data = stations, col = 'black')+
  geom_point(aes(Longitude, Latitude), data = sub, col = 'red')+
  #xlim(-85,-35)
  coord_equal(xlim = c(-85,-35))

is_outlier <- abs(stations$avg_temp_jul - stations$obs_avg_temp_jul) > 2
stations$outlier <- is_outlier
stations_clean <- stations[!stations$outlier,]

library(colorRamps) 
ggplot(stations_clean,aes(x = min_temp_jul, y = max_temp_jul, col = X1981)) + geom_point()+
  ylab('Maximum Temperature, July [°C]')+
  xlab('Minimum Temperature, July [°C]')+
  scale_color_gradientn(colours=matlab.like(15),
                        trans = 'reverse',name="Safe Chill\nPortions [CP]")+
  theme_bw(base_size=12)
ggsave('dot_plot.jpg',device = 'jpeg', height = 15, width = 20, units = "cm")

climate <- readOGR('E:/chil/agroecological_zones_sa/wrl_mcli.shp')
climate$CLIMATE <- as.factor(climate$CLIMATE)

length(levels(climate$CLIMATE))
bb <- extent(-90, -30, -60, 17)


levels(as.factor(stations$CLIMATE))

Porig<-SpatialPointsDataFrame(stations[,c("Longitude","Latitude")],
                              proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                              data=stations[,c(2,5:63)])

data(World)#for map of south america

countries <- c('FRA','ARG', 'BOL', 'BRA', 'CHL', 'COL', 'ECU',  'GUY', 'PER', 'PRY', 'SUR', 'URY', 'VEN')
SA <- World[World$iso_a3 %in% countries,]
SA_sfc <- hatchedLayer(x= SA,mode = "sfc", pattern = 'right2left', density = 5)
#set bounding box for SA
SA_region = extent(c(-8000000,-3000000,-6700000,1700000))
SA_sfc <- as_Spatial(SA_sfc) #convert to S4 object
SA_sfc <- crop(SA_sfc,SA_region) #crop france out of the selection, because it was needed for french guyana
SA_test <- spTransform(SA_sfc,CRSobj = crs(Porig))

clim_map <-  tm_shape(SA_test)+
  tm_lines()+
  tm_shape(climate, bbox = bb) + 
  tm_polygons('CLIMATE',palette=get_brewer_pal('Accent', n=14)) +
  tm_shape(Porig) + tm_dots(size = 0.2)+
  tm_graticules(lines =F)+
  tm_compass(position = c(0.64,0.1))+
  tm_scale_bar(position = c(0.58,0.01),bg.color = 'white')+
  tm_layout(legend.outside=T,outer.margins = c(0.001,0.001,0,0.001))
tmap_save(clim_map, filename = 'chill_maps/climate_zones.jpg',height = 13,width=12,units = 'cm')  







stations$CLIMATE_name
max(stations$Latitude)

stations$lat_group <- cut(stations$Latitude,breaks = seq(5,-55,by=-5))
levels(stations$lat_group)
stations$lat_group <- factor(stations$lat_group, levels=rev(levels(stations$lat_group)))

names(stations$max_temp_jul)

ggplot(stations,aes(y=X1981,x=Elevation))+geom_point()+
  geom_smooth(se = FALSE,method = 'lm')+
  facet_wrap(~lat_group)+
  ylab('Safe Winter Chill [CP]')+
  xlab('Elevation [m]')+
  theme_bw()
ggsave('chill_maps/effect_elevation_latgroup.jpg',height = 10,width = 15,units = 'cm')

ggplot(stations,aes(y=X1981,x=distance_occean))+geom_point()+
  geom_smooth(se = FALSE,method = 'lm')+
  facet_wrap(~lat_group)+
  ylab('Safe Winter Chill [CP]')+
  xlab('Distance to occean [km]')+
  theme_bw()
ggsave('chill_maps/effect_distocean_latgroup.jpg',height = 10,width = 15,units = 'cm')

ggplot(stations,aes(y=X1981,x=seasonality))+geom_point()+
  geom_smooth(se = FALSE,method = 'lm')+
  facet_wrap(~lat_group)+
  ylab('Safe Winter Chill [CP]')+
  xlab('Seasonality [ ]')+
  theme_bw()
ggsave('chill_maps/effect_seasonality_latgroup.jpg',height = 10,width = 15,units = 'cm')

ggplot(stations,aes(y=X1981,x=mean_diurnal_range))+geom_point()+
  geom_smooth(se = FALSE,method = 'lm')+
  facet_wrap(~lat_group)+
  ylab('Safe Winter Chill [CP]')+
  xlab('Mean Diurnal Range [°C]')+
  theme_bw()
ggsave('chill_maps/effect_diurnalrange_latgroup.jpg',height = 10,width = 15,units = 'cm')

ggplot(stations,aes(y=X1981,x=avg_temp_jul))+geom_point()+
  geom_smooth(se = FALSE,method = 'lm')+
  facet_wrap(~lat_group)+
  ylab('Safe Winter Chill [CP]')+
  xlab('Average Temperature, July [°C]')+
  theme_bw()
ggsave('chill_maps/effect_meantempjul_latgroup.jpg',height = 10,width = 15,units = 'cm')

ggplot(stations,aes(y=X1981,x=min_temp_jul))+geom_point()+
  geom_smooth(se = FALSE,method = 'lm')+
  facet_wrap(~lat_group)+
  ylab('Safe Winter Chill [CP]')+
  xlab('Minimum Temperature, July [°C]')+
  theme_bw()
ggsave('chill_maps/effect_mintempjul_latgroup.jpg',height = 10,width = 15,units = 'cm')

ggplot(stations,aes(y=X1981,x=max_temp_jul))+geom_point()+
  geom_smooth(se = FALSE,method = 'lm')+
  facet_wrap(~lat_group)+
  ylab('Safe Winter Chill [CP]')+
  xlab('Maximum Temperature, July [°C]')+
  theme_bw()
ggsave('chill_maps/effect_maxtempjul_latgroup.jpg',height = 10,width = 15,units = 'cm')


SA_countries <- borders("world", regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname",
                                             "Colombia", "Venezuela", "Bolivia", "Ecuador", "Chile", "Paraguay",
                                             "Peru", "Guyana"),
                        fill = "white", colour = "black",
                        size = 0.3)

sa_map <- ggplot() + SA_countries +  theme_bw() +
  geom_point(aes(Longitude, Latitude), data = stations, col = 'black')+
  ylab('Latitude')+xlab('Longitude')+
  scale_x_continuous(breaks = seq(-80, -40, by = 10))+
  scale_y_continuous(breaks = seq(-50, 10, by = 10))+
  coord_equal(xlim = c(-35, -85))
sa_map




df_help <- data.frame('obs_avg_temp_jul' = -2:30,'avg_temp_jul'=-2:30)

ggplot(stations, aes(x=obs_avg_temp_jul,y=avg_temp_jul)) +
  geom_ribbon(data= df_help,aes(ymax = avg_temp_jul+2,ymin = avg_temp_jul-2,x=obs_avg_temp_jul),
              col='grey',alpha= 0.2)+
  geom_point()+
  geom_abline(slope = 1)+
  coord_cartesian(xlim = c(0,28),ylim=c(0,28))+
  ylab('WorldClim: Avergae Temperature, July [°C]')+
  xlab('Station: Avergae Temperature, July [°C]')+
  theme_bw()
ggsave('chill_maps/WorldClim_vs_Station.jpg')

ggplot(stations, aes(x=Elevation,y=X1981)) +
  geom_point()+
  ylab('Safe Chill Portions [CP]')+
  xlab('Elevation [m]')+
  theme_bw()
ggsave('chill_maps/chill_vs_elevation.jpg')




is_outlier <- abs(stations$avg_temp_jul - stations$obs_avg_temp_jul) > 2
stations$outlier <- is_outlier

upper_break = 20
lower_break = 8

####test mean temp july

#subset of original station data according to breakes
low <- subset(stations, stations$avg_temp_jul <= lower_break)
mid <- subset(stations, stations$avg_temp_jul > lower_break & stations$avg_temp_jul <upper_break)
up <- subset(stations, stations$avg_temp_jul >= upper_break)

low_clean<- low[!(low$outlier),]
mid_clean<- mid[!(mid$outlier),]
up_clean<- up[!(up$outlier),]

ggplot(stations,aes(x = avg_temp_jul, y = X1981)) +
  geom_point(col = 'grey')+
  ylab('Safe Chill Portion [CP]')+
  xlab('Average Temperature, July [°C]')+
  #coord_cartesian(ylim = c(0,100))+
  ylim(c(0,100))+
  geom_point(data = low_clean, aes(x = avg_temp_jul, y = X1981))+
  geom_smooth(data = low_clean, method = 'lm', col = 'black',se=F)+
  geom_point(data = mid_clean, aes(x = avg_temp_jul, y = X1981))+
  geom_smooth(data = mid_clean, method = 'lm', col = 'black',se=F)+
  geom_point(data = up_clean, aes(x = avg_temp_jul, y = X1981))+
  geom_smooth(data = up_clean, method = 'lm', col = 'black',se=F)+
  geom_vline(xintercept=c(lower_break,upper_break))+
  theme_bw()
ggsave('chill_maps/three_phases.jpg')
