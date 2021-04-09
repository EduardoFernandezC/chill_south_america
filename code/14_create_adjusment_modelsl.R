#adjustment model for chill based on mean temperature ("three-phases model")
#and adjustmend model based on combination of min and max temperature ("3D model)

library(ggplot2)
library(ggrepel)

stations <- read.csv('data/all_chill_projections.csv')

#hand-defined breaks for three phase model
upper_break = 20
lower_break = 8

#mark stations which are outlying in worldclim
is_outlier <- abs(stations$avg_temp_jul - stations$obs_avg_temp_jul) > 2
stations$outlier <- is_outlier

####test mean temp july

#subset of original station data according to breakes
low <- subset(stations, stations$avg_temp_jul <= lower_break)
mid <- subset(stations, stations$avg_temp_jul > lower_break & stations$avg_temp_jul <upper_break)
up <- subset(stations, stations$avg_temp_jul >= upper_break)

#create model for the subset of chill explained by mean temperature in august
model_low <- lm(data = low, X1981~avg_temp_jul)
model_mid <- lm(data = mid, X1981~avg_temp_jul)
model_up <- lm(data = up, X1981~avg_temp_jul)

#plot three phases model with all stations included, outliers marked by name
ggplot(stations,aes(x = avg_temp_jul, y = X1981)) +
  ylab('Chill Portion')+
  #coord_cartesian(ylim = c(0,100))+
  ylim(c(0,100))+
  geom_point(data = low, aes(x = avg_temp_jul, y = X1981), col = 'blue')+
  geom_smooth(data = low, method = 'lm', col = 'blue')+
  geom_point(data = mid, aes(x = avg_temp_jul, y = X1981), col = 'black')+
  geom_smooth(data = mid, method = 'lm', col = 'black')+
  geom_point(data = up, aes(x = avg_temp_jul, y = X1981), col = 'red')+
  geom_smooth(data = up, method = 'lm', col = 'red')+
  geom_vline(xintercept=c(lower_break,upper_break))+
  geom_label_repel(
    data = subset(stations, outlier),
    aes(label = station_name),
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines"),
    min.segment.length = 0,
    nudge_x = 5
  )+  theme_bw()


#check model without the supposed outliers

#subset without outliers
low_clean <- low[!(low$outlier),]
mid_clean <- mid[!(mid$outlier),]
up_clean <- up[!(up$outlier),]

#create model for the subset of chill explained by mean temperature in august
model_low_clean <- lm(data = low_clean, X1981~avg_temp_jul)
model_mid_clean <- lm(data = mid_clean, X1981~avg_temp_jul)
model_up_clean <- lm(data = up_clean, X1981~avg_temp_jul)

stations_clean <- stations[!stations$outlier,]

ggplot(stations,aes(x = avg_temp_jul, y = X1981)) +
  geom_point(col = 'grey')+
  ylab('Safe Chill Portion [CP]')+
  xlab('Average Temperature, July [°C]')+
  #coord_cartesian(ylim = c(0,100))+
  ylim(c(0,100))+
  geom_point(data = low_clean, aes(x = avg_temp_jul, y = X1981), col = 'blue')+
  geom_smooth(data = low_clean, method = 'lm', col = 'blue',se=F)+
  geom_smooth(data = low, method = 'lm', col = 'blue',se=F,linetype = "dashed")+
  geom_point(data = mid_clean, aes(x = avg_temp_jul, y = X1981), col = 'black')+
  geom_smooth(data = mid_clean, method = 'lm', col = 'black',se=F)+
  geom_smooth(data = mid, method = 'lm', col = 'black',se=F,linetype = "dashed")+
  geom_point(data = up_clean, aes(x = avg_temp_jul, y = X1981), col = 'red')+
  geom_smooth(data = up_clean, method = 'lm', col = 'red',se=F)+
  geom_smooth(data = up, method = 'lm', col = 'red',se=F, linetype = "dashed")+
  geom_vline(xintercept=c(lower_break,upper_break))+
  theme_bw()
ggsave('figures/three_phases_july.jpg',width = 20, height = 15, units = 'cm')



#####test the same thing with august

#mark outlier
is_outlier <- abs(stations$avg_temp_aug - stations$obs_avg_temp_aug) > 2
stations$outlier <- is_outlier

#subset of original station data according to breakes
low <- subset(stations, stations$avg_temp_aug <= lower_break)
mid <- subset(stations, stations$avg_temp_aug > lower_break & stations$avg_temp_aug <upper_break)
up <- subset(stations, stations$avg_temp_aug >= upper_break)

#create model for the subset of chill explained by mean temperature in august
model_low <- lm(data = low, X1981~avg_temp_aug)
model_mid <- lm(data = mid, X1981~avg_temp_aug)
model_up <- lm(data = up, X1981~avg_temp_aug)



ggplot(stations,aes(x = avg_temp_aug, y = X1981)) +
  ylab('Chill Portion')+
  #coord_cartesian(ylim = c(0,100))+
  ylim(c(0,100))+
  geom_point(data = low, aes(x = avg_temp_aug, y = X1981), col = 'blue')+
  geom_smooth(data = low, method = 'lm', col = 'blue')+
  geom_point(data = mid, aes(x = avg_temp_aug, y = X1981), col = 'black')+
  geom_smooth(data = mid, method = 'lm', col = 'black')+
  geom_point(data = up, aes(x = avg_temp_aug, y = X1981), col = 'red')+
  geom_smooth(data = up, method = 'lm', col = 'red')+
  geom_vline(xintercept=c(lower_break,upper_break))+
  geom_label_repel(
    data = subset(stations, outlier),
    aes(label = station_name),
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines"),
    min.segment.length = 0,
    nudge_x = 5
  )+  theme_bw()


#check model without the supposed outliers

#subset without outliers
low_clean<- low[!(low$outlier),]
mid_clean<- mid[!(mid$outlier),]
up_clean<- up[!(up$outlier),]

#create model for the subset of chill explained by mean temperature in august
model_low_clean <- lm(data = low_clean, X1981~avg_temp_aug)
model_mid_clean <- lm(data = mid_clean, X1981~avg_temp_aug)
model_up_clean <- lm(data = up_clean, X1981~avg_temp_aug)

stations_clean <- stations[!stations$outlier,]

ggplot(stations,aes(x = avg_temp_aug, y = X1981)) +
  geom_point(col = 'grey')+
  ylab('Safe Chill Portion [CP]')+
  xlab('Average Temperature, August [°C]')+
  #coord_cartesian(ylim = c(0,100))+
  ylim(c(0,100))+
  geom_point(data = low_clean, aes(x = avg_temp_aug, y = X1981), col = 'blue')+
  geom_smooth(data = low_clean, method = 'lm', col = 'blue',se=F)+
  geom_smooth(data = low, method = 'lm', col = 'blue',se=F,linetype = "dashed")+
  geom_point(data = mid_clean, aes(x = avg_temp_aug, y = X1981), col = 'black')+
  geom_smooth(data = mid_clean, method = 'lm', col = 'black',se=F)+
  geom_smooth(data = mid, method = 'lm', col = 'black',se=F,linetype = "dashed")+
  geom_point(data = up_clean, aes(x = avg_temp_aug, y = X1981), col = 'red')+
  geom_smooth(data = up_clean, method = 'lm', col = 'red',se=F)+
  geom_smooth(data = up, method = 'lm', col = 'red',se=F, linetype = "dashed")+
  geom_vline(xintercept=c(lower_break,upper_break))+
  theme_bw()
ggsave('figures/three_phases_aug.jpg',width = 20, height = 15, units = 'cm')


