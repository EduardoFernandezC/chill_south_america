#calculate mean temperatures based on original temperature observations of the climate stations
#in second step compare to data of worldclim to identify outliers

library(stringr)#needed for functions to extract the stations name from the file names
library(ggplot2)
library(ggrepel)#needed to avoid overlap of labels

#read historic temperatures. These are generated from actual historic records plus a patching procedure plus 
# an interpolating procedure as the latest option for filling the gaps...
#setwd('data/patched_temps/')
path <- 'data/patched_temps/'
temp <- list.files(path, pattern = "*.csv")
temp_files <- lapply(temp, function (x) read.csv(paste0(path, x)))

#just a test to see if it works
temp1 <- read.csv(paste0(path, 'WS_1_ARTURO MERINO BENITEZ INTL.csv'))
sub1 <- temp1[temp1$Month == 7,]
sub1$t_mean <- round((sub1$Tmax + sub1$Tmin) / 2, digits = 2)
mean(sub1$t_mean)

calc_tmean <- function(df, month){
  #subset the data frame
  sub1 <- df[df$Month == month, ]
  return(mean((sub1$Tmax + sub1$Tmin)/2, na.rm = T))
}

calc_tmean_sd <- function(df, month){
  #subset the data frame
  sub1 <- df[df$Month == month, ]
  return(sd((sub1$Tmax + sub1$Tmin)/2, na.rm = T))
}


#extract station name
station_names <- sapply(temp, function(x){
  return(str_split(str_split(x, pattern = '_')[[1]][3], pattern = '.csv')[[1]][1])
})

#remove names of vectotr
station_names <- unname(station_names)

#calculate mean temperature and sd of mean temp of jun, jul, aug
t_jul <- sapply(temp_files, calc_tmean, 7)
t_jul_sd <- sapply(temp_files, calc_tmean_sd, 7)
t_aug <- sapply(temp_files, calc_tmean, 8)
t_aug_sd <- sapply(temp_files, calc_tmean_sd, 8)

#combine to data frame
df_temp <- data.frame(station_names, t_jul, t_jul_sd, t_aug, t_aug_sd)

colnames(df_temp) <- c('Name', 'obs_avg_temp_jul',
                       'sd_obs_avg_temp_jul', 'obs_avg_temp_aug', 'sd_obs_avg_temp_aug')

#read stations data frame
stations <- read.csv('data/all_chill_projections.csv')

#combine df of stations and observed temperatures
stations <- merge.data.frame(stations, df_temp, by.x = 'station_name', by.y = 'Name')

#update the stations dataframe
write.csv(stations, file = 'data/all_chill_projections.csv', row.names = F)


# Some quality check plots

# df needed to draw the ribbon
df <- data.frame(obs_avg_temp_jul = 0:30, avg_temp_jul = 0:30)

ggplot(stations,aes(x= obs_avg_temp_jul, y = avg_temp_jul)) + 
  geom_ribbon(data = df, aes(ymin = avg_temp_jul -2, ymax = avg_temp_jul +2), fill="grey", alpha=.5)+
  geom_point() + 
  geom_abline(slope = 1,intercept = 0)+
  coord_cartesian(xlim = c(0,28),ylim=c(0,28))+
  ylab('WorldClim: Avergae Temperature, July [°C]')+
  xlab('Station: Avergae Temperature, July [°C]')+
  theme_bw()
ggsave('figures/WorldClim_vs_Station.jpg')

#shouldnt I also include the sd for this analysis. maybe points on the edge of the ribbon actually overlap it 
#when sd is included



is_outlier <- abs(stations$avg_temp_jul - stations$obs_avg_temp_jul) > 2
stations$outlier <- is_outlier

ggplot(stations,aes(x = obs_avg_temp_jul, y = avg_temp_jul)) +
  geom_ribbon(data = df, aes(ymin = avg_temp_jul -2, ymax = avg_temp_jul +2), fill="grey", alpha=.5)+
  geom_point()+
  geom_abline(intercept = 0,slope = 1)+
  geom_label_repel(
    data = subset(stations, outlier),
    aes(label = station_name),
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines"),
    min.segment.length = 0,
    nudge_x = 5
  )+
  ylab('WorldClim')+
  xlab('Climate Station')+
  ggtitle('Mean Temperature in July')+
  theme_bw()

ggsave("figures/outliers_wordclim_vs_stations.png")

#identify the outliers in july

