#calculate mean temperatures based on original temperature observations of the climate stations
#in second step compare to data of worldclim to identify outliers

library(stringr)#needed for functions to extract the stations name from the file names
library(ggplot2)
library(ggrepel)#needed to avoid overlap of labels
library(tidyverse)

#read historic temperatures. These are generated from actual historic records plus a patching procedure plus 
# an interpolating procedure as the latest option for filling the gaps...
#setwd('data/patched_temps/')
path <- 'data/re_analysis/fixed_temps/'
temp <- list.files(path, pattern = "*.csv")

temp_files <- lapply(temp, function (x) read.csv(paste0(path, x)))

# Compatibility adjustment for functions
temp <- str_replace(temp, "patched_fixed", "WS")

#just a test to see if it works
temp1 <- read.csv(paste0(path, 'WS_1_ARTURO MERINO BENITEZ INTL.csv'))
min_year <- 1980
max_year <- 2000

sub1 <- subset(temp1, Year >= min_year & Year <= max_year & Month == 7)
sub1$t_mean <- round((sub1$Tmax + sub1$Tmin) / 2, digits = 2)
mean(sub1$t_mean)
mean(sub1$Tmin)
mean(sub1$Tmax)

calc_tmean <- function(df, month, max_year, min_year){
  #subset the data frame
  sub1 <- subset(df, Year >= min_year & Year <= max_year & Month == month)
  return(c(mean((sub1$Tmax + sub1$Tmin)/2, na.rm = T),mean(sub1$Tmin, na.rm = T),mean(sub1$Tmax, na.rm = T)))
}

calc_tmean_sd <- function(df, month, max_year, min_year){
  #subset the data frame
  sub1 <- subset(df, Year >= min_year & Year <= max_year & Month == month)
  return(c(sd((sub1$Tmax + sub1$Tmin)/2, na.rm = T),sd(sub1$Tmin, na.rm = T),sd(sub1$Tmax, na.rm = T)))
}


#extract station name
station_names <- sapply(temp, function(x){
  return(str_split(str_split(x, pattern = '_')[[1]][3], pattern = '.csv')[[1]][1])
})

calc_tmean(temp_files[[1]], month = 7, min_year = 1980, max_year = 2000)

#remove names of vectotr
station_names <- unname(station_names)

# Correct Punta Arenas 2
station_names[which(station_names == "Punta Arenas")] <- c("Punta Arenas", "Punta Arenas2")

#calculate mean temperature and sd of mean temp of jun, jul, aug
t_jul <- t(sapply(temp_files, calc_tmean, 7, 2000,1980))
t_jul_sd <- t(sapply(temp_files, calc_tmean_sd, 7,2000,1980))
t_aug <- t(sapply(temp_files, calc_tmean, 8,2000,1980))
t_aug_sd <- t(sapply(temp_files, calc_tmean_sd, 8, 2000,1980))

#combine to data frame
df_temp <- data.frame(station_names, t_jul, t_jul_sd, t_aug, t_aug_sd)

colnames(df_temp) <- c('Name', 'obs_avg_temp_jul','obs_tmin_jul','obs_tmax_jul',
                       'sd_obs_avg_temp_jul', 'sd_obs_tmin_jul','sd_obs_tmax_jul',
                       'obs_avg_temp_aug','obs_tmin_aug','obs_tmax_aug',
                       'sd_obs_avg_temp_aug', 'sd_obs_tmin_aug','sd_obs_tmax_aug')

#round the data because we dont need so many digits
df_temp[,-1] <- round(df_temp[,-1],digits = 2)

#read stations data frame
stations <- read.csv('data/re_analysis/all_chill_projections.csv')

#combine df of stations and observed temperatures
stations <- merge.data.frame(stations, df_temp, by.x = 'station_name', by.y = 'Name')

#update the stations dataframe
write.csv(stations, file = 'data/re_analysis/all_chill_projections.csv', row.names = F)

# Read the data from the folder
stations <- read.csv("data/re_analysis/all_chill_projections.csv")


# Some quality check plots

# df needed to draw the ribbon
df <- data.frame(obs_avg_temp_jul = 0:30, avg_temp_jul = 0:30)


####  Tmean

ggplot(stations, aes(x= obs_avg_temp_jul, y = avg_temp_jul)) + 
  geom_ribbon(data = df, aes(ymin = avg_temp_jul -2, ymax = avg_temp_jul +2), fill="grey", alpha=.5)+
  geom_point() +
  #geom_errorbar(aes(ymin=avg_temp_jul-sd_obs_avg_temp_jul, ymax = avg_temp_jul + sd_obs_avg_temp_jul))+
  geom_abline(slope = 1,intercept = 0)+
  coord_cartesian(xlim = c(0,28),ylim=c(0,28))+
  ylab('WorldClim: Avergae Temperature, July [°C]')+
  xlab('Station: Avergae Temperature, July [°C]')+
  theme_bw()
ggsave('figures/outliers_tmean.png')

#shouldnt I also include the sd for this analysis. maybe points on the edge of the ribbon actually overlap it 
#when sd is included


stations$outlier_tmean_jul <- abs(stations$avg_temp_jul - stations$obs_avg_temp_jul) > 2
sum(stations$outlier_tmean_jul)

ggplot(stations,aes(x = obs_avg_temp_jul, y = avg_temp_jul)) +
  geom_ribbon(data = df, aes(ymin = avg_temp_jul -2, ymax = avg_temp_jul +2), fill="grey", alpha=.5)+
  geom_point()+
  geom_abline(intercept = 0,slope = 1)+
  geom_label_repel(
    data = subset(stations, outlier_tmean_jul),
    aes(label = station_name),
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines"),
    min.segment.length = 0,
    nudge_x = 5
  )+
  ylab('WorldClim: Avergae Temperature, July [°C]')+
  xlab('Station: Avergae Temperature, July [°C]')+
  theme_bw()
ggsave("figures/outliers_tmean_annotated.png")

#identify the outliers in july

####Tmin

#for ribbon
df <- data.frame(obs_tmin_jul = -6:26, min_temp_jul = -6:26)

ggplot(stations,aes(x= obs_tmin_jul, y = min_temp_jul)) + 
  geom_ribbon(data = df, aes(ymin = min_temp_jul -2, ymax = min_temp_jul +2), fill="grey", alpha=.5)+
  geom_point() +
  #geom_errorbar(aes(ymin=avg_temp_jul-sd_obs_avg_temp_jul, ymax = avg_temp_jul + sd_obs_avg_temp_jul))+
  geom_abline(slope = 1,intercept = 0)+
  coord_cartesian(xlim = c(-4,24),ylim=c(-4,24))+
  ylab('WorldClim: Minimum Temperature, July [°C]')+
  xlab('Station: Minimum Temperature, July [°C]')+
  theme_bw()
ggsave('figures/outliers_tmin.png')

stations$outlier_tmin_jul <- abs(stations$min_temp_jul - stations$obs_tmin_jul) > 2
sum(stations$outlier_tmin_jul)

ggplot(stations,aes(x= obs_tmin_jul, y = min_temp_jul)) + 
  geom_ribbon(data = df, aes(ymin = min_temp_jul -2, ymax = min_temp_jul +2), fill="grey", alpha=.5)+
  geom_point() +
  #geom_errorbar(aes(ymin=avg_temp_jul-sd_obs_avg_temp_jul, ymax = avg_temp_jul + sd_obs_avg_temp_jul))+
  geom_abline(slope = 1,intercept = 0)+
  geom_label_repel(
    data = subset(stations, outlier_tmin_jul),
    aes(label = station_name),
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines"),
    min.segment.length = 0,
    nudge_x = 6
  )+
  ylab('WorldClim: Minimum Temperature, July [°C]')+
  xlab('Station: Minimum Temperature, July [°C]')+
  theme_bw()
ggsave("figures/outliers_tmin_annotated.png")

#### Tmax
df <- data.frame(obs_tmax_jul = -5:35, max_temp_jul = -5:35)

ggplot(stations,aes(x= obs_tmax_jul, y = max_temp_jul)) + 
  geom_ribbon(data = df, aes(ymin = max_temp_jul -2, ymax = max_temp_jul +2), fill="grey", alpha=.5)+
  geom_point() +
  #geom_errorbar(aes(ymin=avg_temp_jul-sd_obs_avg_temp_jul, ymax = avg_temp_jul + sd_obs_avg_temp_jul))+
  geom_abline(slope = 1,intercept = 0)+
  coord_cartesian(xlim = c(0,33),ylim=c(0,33))+
  ylab('WorldClim: Maximum Temperature, July [°C]')+
  xlab('Station: Maximum Temperature, July [°C]')+
  theme_bw()
ggsave('figures/outliers_tmax.png')


stations$outlier_tmax_jul <- abs(stations$max_temp_jul - stations$obs_tmax_jul) > 2
sum(stations$outlier_tmax_jul)

ggplot(stations,aes(x= obs_tmax_jul, y = max_temp_jul)) + 
  geom_ribbon(data = df, aes(ymin = max_temp_jul -2, ymax = max_temp_jul +2), fill="grey", alpha=.5)+
  geom_point() +
  #geom_errorbar(aes(ymin=avg_temp_jul-sd_obs_avg_temp_jul, ymax = avg_temp_jul + sd_obs_avg_temp_jul))+
  geom_abline(slope = 1,intercept = 0)+
  geom_label_repel(
    data = subset(stations, outlier_tmax_jul),
    aes(label = station_name),
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines"),
    min.segment.length = 0,
    nudge_x = 6
  )+
  ylab('WorldClim: Maximum Temperature, July [°C]')+
  xlab('Station: Maximum Temperature, July [°C]')+
  theme_bw()
ggsave("figures/outliers_tmax_annotated.png")

#number of stations outlying either in tmin or tmax
sum((stations$outlier_tmin_jul | stations$outlier_tmax_jul)) 

#update the stations dataframe
write.csv(stations, file = 'data/re_analysis/all_chill_projections.csv', row.names = F)


# Create one figure including the outliers for tmin and tmax

stations <- read.csv("data/re_analysis/all_chill_projections.csv")

stations_tmin <- dplyr::select(stations, station_name, min_temp_jul, obs_tmin_jul)

colnames(stations_tmin) <- c("station_name", "WorldClim", "On-site")

stations_tmin["Var"] <- "Tmin"


# Tmax
stations_tmax <- dplyr::select(stations, station_name, max_temp_jul, obs_tmax_jul)

colnames(stations_tmax) <- c("station_name", "WorldClim", "On-site")

stations_tmax["Var"] <- "Tmax"


# Merge both data frames
stations_both <- bind_rows(stations_tmax, stations_tmin)


# Plot

ggplot(stations_both, aes(`On-site`, WorldClim)) + 
  geom_point() +
  geom_ribbon(data = df, aes(max_temp_jul, obs_tmax_jul, ymin = max_temp_jul - 2, ymax = max_temp_jul + 2),
              fill = "grey", alpha = .5) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(. ~ factor(Var, labels = c("Maximum temperature", "Minimum temperature")), nrow = 2) +
  geom_label_repel(
    data = stations_both[abs(stations_both$WorldClim - stations_both$`On-site`) > 2, ],
    aes(label = station_name),
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines"),
    min.segment.length = 0,
    nudge_x = 5,
    nudge_y = 5,
    max.overlaps = 25,
    direction = "both",
    force = 30) +
  xlab("On-site data (°C)") +
  ylab("WorldClim data (°C)") +
  theme_bw(base_size = 14)

ggsave("figures/final_figures/figure_S1_b.png", height = 19, width = 17, units = "cm", dpi = 600)







