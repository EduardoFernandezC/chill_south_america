#this code is outdated, it prodcues a ton of figures but I think they are not so great
#better use a simplified script, which makes all graphs of every lat group in one figure with the help of facet_grid



#iterates other all variables and creates scatter plots of chill and the variables
#also splits up the climate stations by latitude, latitude + longitude and by climatic zone
#saves all pictures to seperate folder


#southern hemisphere season 1st of May - 31st of August

library(ggplot2)
library(gridExtra)
library(grid)
library(ggrepel)

stations <- read.csv('data/all_chill_projections.csv')
#set climate as a factor
stations$CLIMATE <- as.factor(stations$CLIMATE)

#choose scenarip to investigate
scen <- "X1981"

model_variables <- c('max_temp_jul','max_temp_aug')

#extract borders of sa
SA_countries <- borders("world", regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname",
                                             "Colombia", "Venezuela", "Bolivia", "Ecuador", "Chile", "Paraguay",
                                             "Peru", "Guyana"),
                        fill = "white", colour = "black",
                        size = 0.3)

#create map with stations
sa_map <- ggplot() + SA_countries +  theme_bw() +
  geom_point(aes(Longitude, Latitude), data = stations, col = 'black')+
  coord_equal(xlim = c(-85, -35))

#function to extract p value of modek
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

#function to place the plots side to side
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#function to automate the saving of graphs
create_moodel_pictures <- function(pred.model,scen,string_xvar, f_path, f_name,f_name2, sub1, fig_title,
                                   actual_map){
  
  #extract p-value
  p_val <- lmp(pred.model)
  #either yes or no if slope of model is significantly different from zero
  signific <- 'No'

  if(p_val < 0.05){
      signific <- 'Yes'
    }

  #create subtitle
  mysubtitle <- paste('slope significant different from zero (p<0.05)?',signific , sep = ' ')

  #create paths for figures
  fname <- paste(f_path,f_name,sep = '')
  fname2 <- paste(f_path,f_name2,sep = '')
  
  #create regression plot 
  model_plot <- ggplot(sub1,aes_string(x = string_xvar , y = scen)) +
    ylab('Chill Portion')+
    coord_cartesian(ylim = c(0,100))+
    stat_summary(fun.data= mean_cl_normal) + 
    geom_smooth(method='lm') + 
    ggtitle(paste(fig_title,'\n',mysubtitle, sep = ''))+
    geom_point()+
    theme_bw()
  
  fname <- paste(f_path,f_name,sep = '')

  jpeg(file=fname, width = 700, height = 500)
  dual_plot <- multiplot(model_plot, actual_map, cols=2)
  dev.off()
  
  #save annotaed model fig
  model_plot <- model_plot +  geom_text_repel(data = sub1, aes(label = Name),     
                                              xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
                                              nudge_y = -3, box.padding = 1.75)
  
  ggsave(model_plot,filename = fname2, width = 20, height = 15, units = 'cm')
}


#split into groups by latitude
stations$lat_group <- cut(stations$Latitude,breaks = seq(-65,5, by=5))
lat_level <- levels(stations$lat_group)

#split also by longitude
stations$long_group <- cut(stations$Longitude,breaks = seq(-90,-30, by=5))
long_level <- levels(stations$long_group)


step_size = 5

#model_variables <- c('Elevation','distance_occean', 'avg_temp_may','avg_temp_jun', 'avg_temp_jul','avg_temp_aug',
#                     'annual_mean_temp','mean_diurnal_range','seasonality','temp_annual_range')

model_variables <- c('max_temp_jul','max_temp_aug')

for (test_var in model_variables) {
  plot_nr=0
  starting_lat = -65
  starting_long = -90
  
  
  #create overall modelplot for the variable, including every observation 
  pred.model <- lm(data=stations,as.formula(paste(scen,'~',test_var, sep = '')))
  p_val <- lmp(pred.model)
  signific <- 'No'
  if(p_val < 0.05){
    signific <- 'Yes'
  }
  fname <-paste(test_var,'total_data.jpg',sep = '')
  fpath <- paste('models/',test_var,'/',sep = '')
  
  #create folder for tested variable
  dir.create(fpath)
  
  ggplot(stations,aes_string(x = test_var , y = scen)) +
    ylab('Chill Portion')+
    coord_cartesian(ylim = c(0,100))+
    stat_summary(fun.data= mean_cl_normal) + 
    geom_smooth(method='lm') + 
    geom_point()+
    ggtitle(paste('Chill Portions (',scen,') ~ ',test_var,'\nSlope sign. diff. zero (alpha < 0.05):', signific,sep = ''))+
    theme_bw()
  ggsave(paste(fpath, fname, sep = ''), width = 12, height = 10, units = 'cm')
  
#####split the data:
    #first comes the split by latitude
    for (groupnr in lat_level) {
      
      #create subset
      sub1 <- subset(stations, stations$lat_group == groupnr)
      
      if(length(sub1$Name) < 4){
        #update latitude when we skip
        starting_lat <- starting_lat + step_size
        next()
      }
      
      #create model
      pred.model <- lm(data=sub1,as.formula(paste(scen,'~',test_var, sep = '')))
      
      p_val <- lmp(pred.model)
      if(is.nan(p_val)){
        #update the latitude if we skip this step
        starting_lat <- starting_lat + step_size
        next()
      } 
      
      #indicate on the map from which part the data is taken
      actual_map <- sa_map + geom_hline(aes(yintercept = c(starting_lat,starting_lat + step_size)))
      
      #create title of plot
      f_name <- paste(test_var,plot_nr,'.jpg', sep = '')
      f_name2 <- paste(test_var,plot_nr,'annotated.jpg', sep = '')
      f_path <- paste('models/', test_var,'/by_lat/', sep = '') 
      fig_title <- paste('Lat group:',groupnr)
      
      #create folder for by lat, at first instance
      if(plot_nr == 0){
        dir.create(f_path)
      }
      
      #function to save and create plot
      create_moodel_pictures(pred.model = pred.model,sub1 = sub1, scen = scen,
                             string_xvar = test_var,
                             f_name = f_name,f_name2 = f_name2, f_path = f_path,fig_title = fig_title,
                             actual_map = actual_map)
      
      #increase lat by stepsize for next plot in the loop
      starting_lat <- starting_lat + step_size
      plot_nr <- plot_nr +1
    }

  plot_nr=0
  starting_lat = -65
  starting_long = -90
  
  
  #now split by lat long
  for (groupnr in lat_level) {
    
    starting_long = -90
    
    for (long_gr in long_level) {
      
      #create subset
      sub1 <- subset(stations, stations$lat_group == groupnr & stations$long_group == long_gr )
      
      if(length(sub1$Name) < 4){
        #update latitude when we skip
        starting_long <- starting_long + step_size
        next()
      }
      
      #create model
      pred.model <- lm(data=sub1,as.formula(paste(scen,'~',test_var, sep = '')))
      
      p_val <- lmp(pred.model)
      if(is.nan(p_val)){
        #update the latitude if we skip this step
        starting_long <- starting_long + step_size
        next()
      } 
      
      #indicate on the map from which part the data is taken
      actual_map <- sa_map + geom_rect(aes(xmin = starting_long, 
                                           xmax = starting_long + step_size,
                                           ymin = starting_lat, 
                                           ymax = starting_lat + step_size), fill = NA, colour = 'black', size = 1.2)      
      #create title of plot
      f_name <- paste(test_var,plot_nr,'.jpg', sep = '')
      f_name2 <- paste(test_var,plot_nr,'annotated.jpg', sep = '')
      f_path <- paste('models/', test_var,'/by_long_lat/', sep = '') 
      fig_title <- paste('Lat group:',groupnr, 'Long group:',long_gr)
      
      #create folder for by lat, at first instance
      if(plot_nr == 0){
        dir.create(f_path)
      }
      
      #function to save and create plot
      create_moodel_pictures(pred.model = pred.model,sub1 = sub1, scen = scen,
                             string_xvar = test_var,
                             f_name = f_name,f_name2 = f_name2, f_path = f_path,fig_title = fig_title,
                             actual_map = actual_map)
      
      #increase lat by stepsize for next plot in the loop
      starting_long <- starting_long + step_size
      plot_nr <- plot_nr +1
    }
    #increase lat by one step
    starting_lat <- starting_lat + step_size

  }
  
  plot_nr=0
  
  #now for climate groups
  for(climate_grp in levels(stations$CLIMATE)){
    
    #create subset
    sub1 <- subset(stations, stations$CLIMATE == climate_grp)
    
    if(length(sub1$Name) < 4){
      #update latitude when we skip
      starting_lat <- starting_lat + step_size
      next()
    }
    
    #create model
    pred.model <- lm(data=sub1,as.formula(paste(scen,'~',test_var, sep = '')))
    
    p_val <- lmp(pred.model)
    if(is.nan(p_val)){
      #update the latitude if we skip this step
      starting_lat <- starting_lat + step_size
      next()
    } 
    
    #indicate on the map from which part the data is taken
    actual_map <- sa_map + geom_point(data = sub1, aes(x = Longitude, y = Latitude),
                                      col = 'red')
    
    #create title of plot
    f_name <- paste(test_var,plot_nr,'.jpg', sep = '')
    f_name2 <- paste(test_var,plot_nr,'annotated.jpg', sep = '')
    f_path <- paste('models/', test_var,'/by_climate/', sep = '') 
    fig_title <- paste('Climate gorup:',climate_grp)
    
    #create folder for by lat, at first instance
    if(plot_nr == 0){
      dir.create(f_path)
    }
    
    #function to save and create plot
    create_moodel_pictures(pred.model = pred.model,sub1 = sub1, scen = scen,
                           string_xvar = test_var,
                           f_name = f_name,f_name2 = f_name2, f_path = f_path,fig_title = fig_title,
                           actual_map = actual_map)
    
    plot_nr <- plot_nr +1
  }
}

