library(reshape2) #melt function
library(ggplot2) #plotting

#creates line graphs of the change in chill per station with a ribbon for the future scenarios
#also in the end makes simple corr plot

#read station coordinates with the projected chill (future and historic)
stations <- read.csv('data/all_chill_projections.csv')

#drop all columns which are not needed
stations <- stations[, c(1, 5 : 28)]

#melt data frame
stations_melt <- melt(stations, id.vars = c('station_name', 'CTRY'))

#add columns to the dataframe, because the RCP, year and type of model output (optimisitic etc.) 
#need to be individually indicated. but sofar it is only as combined infor in the variable column, so the string
#needs to split up and the info needs to be stored in the seperate columns
stations_melt$rcp <- NA
stations_melt$year <- NA
stations_melt$scenario <- NA

#filter the right rows for year, rcp and scenarion to be added in the column
stations_melt[grepl('opti',stations_melt$variable),'scenario'] <- 'optimistic'
stations_melt[grepl('pessi',stations_melt$variable),'scenario'] <- 'pessimistic'
stations_melt[grepl('intermed',stations_melt$variable),'scenario'] <- 'intermediate'
stations_melt[grepl('observed',stations_melt$variable),'year'] <- 1998.5
stations_melt[grepl('2085',stations_melt$variable),'year'] <- 2085
stations_melt[grepl('2050',stations_melt$variable),'year'] <- 2050
stations_melt[grepl('1981',stations_melt$variable),'year'] <- 1981
stations_melt[grepl('1985',stations_melt$variable),'year'] <- 1985
stations_melt[grepl('1989',stations_melt$variable),'year'] <- 1989
stations_melt[grepl('1993',stations_melt$variable),'year'] <- 1993
stations_melt[grepl('1997',stations_melt$variable),'year'] <- 1997
stations_melt[grepl('2001',stations_melt$variable),'year'] <- 2001
stations_melt[grepl('2005',stations_melt$variable),'year'] <- 2005
stations_melt[grepl('2009',stations_melt$variable),'year'] <- 2009
stations_melt[grepl('2013',stations_melt$variable),'year'] <- 2013
stations_melt[grepl('2017',stations_melt$variable),'year'] <- 2017
stations_melt[grepl('rcp45',stations_melt$variable),'rcp'] <- 'rcp45'
stations_melt[grepl('rcp85',stations_melt$variable),'rcp'] <- 'rcp85'
#add 'observed' to scenearios column of years 1981 - 2017 so they can be differentiated in the plot
stations_melt[is.na(stations_melt$scenario),]$scenario <- 'historic'
#change order of scenarios so it looks prettier in the plot
stations_melt$scenario <- factor(stations_melt$scenario,levels = c('historic','optimistic','intermediate','pessimistic'))

#make names to factor, so it is done in alphabetical order
stations$station_name <- as.factor(stations$station_name)

ggplot(stations_melt,aes(x=as.factor(year),y=value,fill=scenario)) + 
  geom_boxplot() + 
  facet_grid(~rcp,scales = 'free_x',space='free_x')+
  ylab('Safe Chill Portion')+
  xlab('Year')+
  #labs(title = 'Change in Chill compared to 2017')+ 
  scale_fill_manual(breaks = c("observed", "optimistic", "intermediate", "pessimistic"), 
                    values=c('white',"#4DAF4A","#377EB8","#E41A1C"))+
  #scale_fill_brewer(palette="Set1")+
  theme_bw()
ggsave('figures/boxplot_change_in_chill.jpeg',height = 10, width = 15, units = 'cm',device = 'jpeg')


#cycle through all station names
for(name in levels(stations$station_name)){
  #subset the data frame
  sub_df <- subset(stations_melt,stations_melt$station_name == name)
  #add the historic years twice so they can be labbeled with both rcps
  sub_df <- rbind(sub_df, sub_df[1:11, ])
  #label both historic time spans
  sub_df$rcp[1:11] <- 'rcp45'
  sub_df$rcp[23:34] <- 'rcp85'
  #add year 2017 six times, so that each of them can get a scenario, so that the line plot connects
  sub_df <- rbind(sub_df,sub_df[11,],sub_df[11,],sub_df[11,],sub_df[34,],sub_df[34,],sub_df[34,])
  #change rownames because they are ugly
  rownames(sub_df) <- 1:length(sub_df$station_name)
  #add the scenarios to the duplicates of 2017
  sub_df[35:40,]$scenario <- rep(c('optimistic','intermediate','pessimistic'),2)
  
  # Extract the observed rows
  obs <- sub_df[sub_df$variable == "observed_SWC", ]
  sub_df <- sub_df[sub_df$variable != "observed_SWC", ]
  
  #get upper and lower values to shade the area between future scenarios
  extremes <- aggregate(sub_df$value, by=list(sub_df$rcp,sub_df$year),FUN = max)
  lower <- aggregate(sub_df$value, by=list(sub_df$rcp,sub_df$year),FUN = min)
  extremes$lower <- lower$x
  names(extremes)[1:3] <- c('rcp','year','upper')
  
  #extract CTRY info
  country <- sub_df$CTRY[1]
  #do the plotting
  p<- ggplot()+
    geom_ribbon(data = extremes, aes(ymax = upper,ymin = lower,x=year),col='grey',alpha= 0.2,)+
    geom_point(data = sub_df,aes(x=year, y=value,col = scenario),size=1.5)+
    geom_line(data = sub_df,aes(x=year, y=value,col = scenario),size = 1)+
    geom_point(data = obs,aes(x=year, y=value,col = scenario),size=3, color = "grey40", shape = 8)+
    scale_color_manual(breaks = c("historic", "optimistic", "intermediate", "pessimistic"), 
                       values=c('black',"#4DAF4A","#377EB8","#E41A1C"))+
    labs(y='Safe winter chill [Chill Portions]', x = 'Year')+
    ggtitle(paste(name,' [',country,']', sep = ''))+
    ylim(0,100)+
    facet_grid(~ factor(rcp, labels = c("RCP4.5", "RCP8.5"))) +
    theme_bw()+
    theme(legend.position = "none")
  #p
  ggsave(p, filename =  paste('figures/line_plots/', name, '.jpg', sep = ''),
         height = 10, width = 10, units = 'cm')
  
}

library(corrplot)  
#subset wanted columns
test <- cor(stations[c(6,35,31,33,30,2,41,43,42)])
#change names so they are pretty
colnames(test) <- c('Safe Chill 1981',  'Max. Temperature (July)',
                    'Mean Temperature (July)',
                    'Min. Temperature (July)','Distance to ocean',
                    'Elevation','Diurnal Range','Annual Temperature Range',
                    'Seasonality')

#adjust row name
rownames(test)[1] <- 'Safe Chill (1981)'
#mame simple reduced corrplot
corrplot(test[1,2:9, drop=FALSE],tl.col ='black',method = 'color',outline = TRUE,
         addCoef.col = "black",number.cex = .7,
         cl.pos = "n")
