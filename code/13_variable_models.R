library(tidyverse)


#read climate stations
stations <- read.csv('data/all_chill_projections.csv')
#somehow I introduced an error to the stations file but I don't know how and why
#it contains a lot of empty rows, so I'll remove them, also there is a column 'X' which is not needed

stations$lat_group <- cut(stations$Latitude,breaks = seq(5,-55,by=-5))
levels(stations$lat_group)
stations$lat_group <- factor(stations$lat_group, levels=rev(levels(stations$lat_group)))

ggplot(stations,aes(y=X1981,x=Elevation))+geom_point()+
  geom_smooth(se = FALSE,method = 'lm')+
  facet_wrap(~lat_group)+
  ylab('Safe Winter Chill [CP]')+
  xlab('Elevation [m]')+
  theme_bw()
ggsave('figures/chill_proxies/effect_elevation_latgroup.jpg',height = 10,width = 15,units = 'cm')

ggplot(stations,aes(y=X1981,x=distance_occean))+geom_point()+
  geom_smooth(se = FALSE,method = 'lm')+
  facet_wrap(~lat_group)+
  ylab('Safe Winter Chill [CP]')+
  xlab('Distance to occean [km]')+
  theme_bw()
ggsave('figures/chill_proxies/effect_distocean_latgroup.jpg',height = 10,width = 15,units = 'cm')

ggplot(stations,aes(y=X1981,x=seasonality))+geom_point()+
  geom_smooth(se = FALSE,method = 'lm')+
  facet_wrap(~lat_group)+
  ylab('Safe Winter Chill [CP]')+
  xlab('Seasonality [ ]')+
  theme_bw()
ggsave('figures/chill_proxies/effect_seasonality_latgroup.jpg',height = 10,width = 15,units = 'cm')

ggplot(stations,aes(y=X1981,x=mean_diurnal_range))+geom_point()+
  geom_smooth(se = FALSE,method = 'lm')+
  facet_wrap(~lat_group)+
  ylab('Safe Winter Chill [CP]')+
  xlab('Mean Diurnal Range [°C]')+
  theme_bw()
ggsave('figures/chill_proxies/effect_diurnalrange_latgroup.jpg',height = 10,width = 15,units = 'cm')

ggplot(stations,aes(y=X1981,x=avg_temp_jul))+geom_point()+
  geom_smooth(se = FALSE,method = 'lm')+
  facet_wrap(~lat_group)+
  ylab('Safe Winter Chill [CP]')+
  xlab('Average Temperature, July [°C]')+
  theme_bw()
ggsave('figures/chill_proxies/effect_meantempjul_latgroup.jpg',height = 10,width = 15,units = 'cm')

ggplot(stations,aes(y=X1981,x=min_temp_jul))+geom_point()+
  geom_smooth(se = FALSE,method = 'lm')+
  facet_wrap(~lat_group)+
  ylab('Safe Winter Chill [CP]')+
  xlab('Minimum Temperature, July [°C]')+
  theme_bw()
ggsave('figures/chill_proxies/effect_mintempjul_latgroup.jpg',height = 10,width = 15,units = 'cm')

ggplot(stations,aes(y=X1981,x=max_temp_jul))+geom_point()+
  geom_smooth(se = FALSE,method = 'lm')+
  facet_wrap(~lat_group)+
  ylab('Safe Winter Chill [CP]')+
  xlab('Maximum Temperature, July [°C]')+
  theme_bw()
ggsave('figures/chill_proxies/effect_maxtempjul_latgroup.jpg',height = 10,width = 15,units = 'cm')

#try to combine tmin and tmax in one figure, exclude some of the less interesting lat groups
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
  xlab('Minimum (blue) and Maximum (red) Temperature [°C] in July')+
  theme_bw()
ggsave('figures/chill_proxies/effect_tmin_tmax_latitude.jpg',height = 10,width = 15,units = 'cm')
