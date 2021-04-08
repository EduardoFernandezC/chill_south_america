library(ggplot2)
library(reshape2)
library(RColorBrewer)

stations <- read.csv('southamerica_chill/chill_south_america/data/all_chill_projections.csv')

#calculate change in chill units from 2017
change_df <- data.frame(stations[,1:2],stations[,17:28]-stations[,'X2017'],stations[,'X2017'] - stations[,'X1981'])
names(change_df)[15] <- '1981 - 2017'
#melt df
change_melt <- melt(change_df,id.vars = c('Name','CTRY'))

#add columns for year, rcp, scenario
change_melt$rcp <- NA
change_melt$year <- NA
change_melt$scenario <- NA

#filter the right rows for year, rcp and scenarion to be added in the column
change_melt[grepl('opti',change_melt$variable),'scenario'] <- 'optimistic'
change_melt[grepl('pessi',change_melt$variable),'scenario'] <- 'pessimistic'
change_melt[grepl('intermed',change_melt$variable),'scenario'] <- 'intermediate'
change_melt[grepl('2085',change_melt$variable),'year'] <- 2085
change_melt[grepl('2050',change_melt$variable),'year'] <- 2050
change_melt[grepl('rcp45',change_melt$variable),'rcp'] <- 'rcp45'
change_melt[grepl('rcp85',change_melt$variable),'rcp'] <- 'rcp85'

change_melt[grepl('2017',change_melt$variable),'rcp'] <- '1981 - 2017'
change_melt[grepl('2017',change_melt$variable),'year'] <- '2017'
change_melt[grepl('2017',change_melt$variable),'scenario'] <- 'observed'

change_melt$scenario <- factor(change_melt$scenario,levels = c('observed','optimistic','intermediate','pessimistic'))

sum(change_df$`1981 - 2017` < 0) / length(change_df$Name)
sum(change_df$`1981 - 2017` == 0) / length(change_df$Name)
sum(change_df$`1981 - 2017` > 0) / length(change_df$Name)
max(change_df$`1981 - 2017`)

ggplot(change_melt,aes(x=as.factor(year),y=value,fill=scenario)) + 
  geom_boxplot() + 
  facet_grid(~rcp,scales = 'free_x',space='free_x')+
  ylab('Safe Chill Portion')+
  xlab('Year')+
  #labs(title = 'Change in Chill compared to 2017')+ 
  scale_fill_manual(breaks = c("observed", "optimistic", "intermediate", "pessimistic"), 
                    values=c('white',"#4DAF4A","#377EB8","#E41A1C"))+
  #scale_fill_brewer(palette="Set1")+
  theme_bw()
ggsave('chill_maps/boxplot_change_in_chill.jpeg',height = 10, width = 15, units = 'cm',device = 'jpeg')
  
brewer.pal(n = 3, name = "Set1")
  display.brewer.pal(n = 8, name = 'Set1')
  
  
library(corrplot)  
  
names(stations)[c(9,31,5,38,44:47,52,62)]
#test <- cor(stations[c(7,46,47,45,5,31,52,44,38,62)])
test <- cor(stations[c(7,62,38,44,52,31,5,45,47,46)])
colnames(test) <- c('Safe Chill 1981',  'Max. Temperature (July)',
                    'Mean Temperature (July)','Annual Mean Temperature',
                    'Min. Temperature (July)','Distance to ocean',
                    'Elevation','Diurnal Range','Annual Temperature Range',
                    'Seasonality')

rownames(test)[1] <- 'Safe Chill (1981)'
corrplot(test[1,2:10, drop=FALSE],tl.col ='black',method = 'color',outline = TRUE,
         addCoef.col = "black",number.cex = .7,
         cl.pos = "n")

corrplot(test[1:8,],tl.col ='black',method = 'color',outline = TRUE,
         addCoef.col = "black",number.cex = .7,
         cl.pos = "b")



write.csv(change_melt,'southamerica_chill/chill_south_america/data/change_in_chill.csv',row.names = F)
