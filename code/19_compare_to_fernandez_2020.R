library(reshape2)
library(ggplot2)

fernandez <- read.csv('southamerica_chill/chill_south_america/data/future_chill_fernandez_chile.csv')

quantile(fernandez[1,4:18],probs = c(0.15,0.5,0.85))

get_scenario <- function(vec, probs = c(0.15,0.5,0.85)){
  return(quantile(vec,probs))
}
scen_chill <- apply(fernandez[,4:18], 1, get_scenario)

fernandez_test <- data.frame(fernandez[,1:3],t(scen_chill))
names(fernandez_test) <- c('name','year','rcp','pessimistic','intermediate','optimistic')

stations <- read.csv('southamerica_chill/chill_south_america/data/all_chill_projections - with City.csv')

stations <- stations[,c(2:4,19:30)]
stations <- subset(stations,stations$CTRY == 'CL')

stations_melt <- melt(stations,id.vars = c('Station.Name','Location.Name','CTRY'),value.name = 'ours')

stations_melt$rcp <- NA
stations_melt$year <- NA
stations_melt$scenario <- NA


stations_melt[grepl('opti',stations_melt$variable),'scenario'] <- 'optimistic'
stations_melt[grepl('pessi',stations_melt$variable),'scenario'] <- 'pessimistic'
stations_melt[grepl('intermed',stations_melt$variable),'scenario'] <- 'intermediate'
stations_melt[grepl('2085',stations_melt$variable),'year'] <- 2085
stations_melt[grepl('2050',stations_melt$variable),'year'] <- 2050
stations_melt[grepl('rcp45',stations_melt$variable),'rcp'] <- 'rcp45'
stations_melt[grepl('rcp85',stations_melt$variable),'rcp'] <- 'rcp85'
names(stations_melt)[2] <- 'name'


fernandez_melt <- melt(fernandez_test,id.vars = names(fernandez_test)[1:3],
                        variable.name = 'scenario',value.name = 'fernandez')
fernandez_melt$rcp <- as.factor(fernandez_melt$rcp)
levels(fernandez_melt$rcp) <- c('rcp45','rcp85')

levels(fernandez_melt$name) %in% levels(stations_melt$Location.Name)

comp_df <- merge.data.frame(fernandez_melt,stations_melt,by=c('name','year','rcp','scenario'))
comp_df <- comp_df[,-c(6,7,8)]

library(chillR)
get_rmse <- function(predicted,observed){
  return(sqrt(sum((observed - predicted)^2)/length(predicted)))
}

RPIQ(predicted = comp_df$ours,observed = comp_df$fernandez)
get_rmse(predicted = comp_df$ours, observed = comp_df$fernandez)

ggplot(comp_df,aes(x=fernandez,y=ours))+geom_point(aes(color = name))+
  geom_abline(slope = 1,linetype = "dashed")+
  ylab('Our data [CP]')+
  xlab('Fernandez et al. (2020) [CP]')+
  annotate("text", x = 10, y = 80, label = "italic(RPIQ) == 2.98",
           parse = TRUE)+
  annotate("text", x = 10, y = 75, label = "italic(RMSE) == 12.4",
           parse = TRUE,)+
  ggtitle('Comparison across two RCPs (4.5, 8.5) and \ntwo time points (2050, 2085) for futre safe chill [CP]\nin six Chilean climate stations')+
  theme_bw()

sqrt(sum((comp_df$ours - comp_df$fernandez)^2)/length(comp_df$ours))

sqrt(sum((comp_df$fernandez - comp_df$ours)^2)/length(comp_df$ours))
