########
#test cross validation with kriging
########
library(gstat)
library(sp)
library(rgdal)#to read shape file
library(tmap)#for map making
library(reshape2)#to melt data frame
library(fields)#for Krig function used in correction model
library(tidyverse)#for plotting of correction model
library(metR)#for geom_contour_fill function
library(colorRamps)#for matlab.like function
library(data.table)#needed to Concatenate a list of data frames
library(tmaptools) #get brewer pal

data(meuse)
coordinates(meuse) <- ~x+y
m <- vgm(.59, "Sph", 874, .04)
# five-fold cross validation:
x <- krige.cv(log(zinc)~1, meuse, m, nmax = 40, nfold=5)

bubble(x, "residual", main = "log(zinc): 5-fold CV residuals")

summary(x)
# mean error, ideally 0:
mean(x$residual)
# MSPE, ideally small
mean(x$residual^2)
# Mean square normalized error, ideally close to 1
mean(x$zscore^2)
# correlation observed and predicted, ideally 1
cor(x$observed, x$observed - x$residual)
# correlation predicted and residual, ideally 0
cor(x$observed - x$residual, x$residual)


#####
#test the validation on the simple kriging of chill
####

stations <- read.csv('data/all_chill_projections.csv')
Porig<-SpatialPointsDataFrame(stations[,c("Longitude","Latitude")],
                              proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                              data=stations[,c(6:27,33,35)])
scen <- 'X1981'

f.1 <- as.formula(paste(scen, "~ Longitude + Latitude"))
var.smpl <- variogram(f.1, Porig, cloud = FALSE)
dat.fit <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                         vgm(psill = 275, model="Sph", nugget = 20, range = 1500))

#k-fold cross-validation

#set number of repetitions
r <- 5

#set number of splits 
k <- 10

#empty list
crossval_df <- data.frame(NA)

#loop to save the outputs in a líst and compute mean values
for(i in 1:r){
  out <- krige.cv(f.1, Porig, dat.fit, nfold = k)
  crossval_df <- cbind(crossval_df,out$residual)
}
#first column is empty, so drop it
crossval_df <- crossval_df[,-1]

#calculate the mean resiudal and standard deviation per row
crossval_df <- transform(crossval_df, mean_residual=apply(crossval_df,1, mean, na.rm = TRUE))
crossval_df <- transform(crossval_df, sd_residual=apply(crossval_df[,1:5],1, sd, na.rm = TRUE))

crossval_krig <- Porig
crossval_krig$residual <- crossval_df$mean_residual
crossval_krig$abs_residual <- abs(crossval_krig$residual)

#out <- krige.cv(f.1, Porig, dat.fit, nfold = 10)
#out$abs_residual <- abs(out$residual)
#bubble(out, "residual", main = "chill 1981: 10-fold CV residuals")


SA <- readOGR('data/sa_outline/SA_outline.shp')


chill_residual <- tm_shape(SA)+
  tm_borders(col = 'black')+
  tm_shape(crossval_krig) + 
  tm_bubbles(size = 'abs_residual',col = 'residual', palette=get_brewer_pal("RdBu"),
             midpoint = 0, breaks=seq(-40,70,by=10))+
  tm_grid()+ 
  tm_layout(legend.outside=T,outer.margins = c(0.001,0.001,0,0.001))
chill_residual
tmap_save(chill_residual, filename = 'figures/cross-validation/residual_uncorrected-krig.jpg',height = 13,width=12,units = 'cm')
#uncorrected chill maps is especially in vicinity of andes insecure, highest cases of deviation are in Bolivia, peru and Colombia
#makes sense because these are the single 'blorps' of the andes in the more northern parts of southamerica
#small changes there lead to tremendous changes in the kriged map
#its nice to see that especially argentinas residuals are low, much lower than chiles. is this because chiles topography is more complex and leads thus to more potential errors?


######
#cross validation for correction model
######

#somehow I can't manage to use the above used function on the correction model, because I dont manage to use the krige function on non-spatial data correctly
#so I assume I need to construct the cross validation by hand

#concept of k-fold cross validation (http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/#cross%20validation)

# 1) Randomly split the data set into k-subsets (or k-fold) (for example 5 subsets)
# 2) Reserve one subset and train the model on all other subsets
# 3) Test the model on the reserved subset and record the prediction error
# 4) Repeat this process until each of the k subsets has served as the test set.
# 5) Compute the average of the k recorded errors. This is called the cross-validation error serving as the performance metric for the model.


#remove otuliers from the set
is_outlier <- abs(stations$avg_temp_jul - stations$obs_avg_temp_jul) > 2
stations$outlier <- is_outlier
stations_clean <- stations[!stations$outlier,]

#drop all columns which are not needed right now
stations_clean <- stations_clean[,c(1,6,33,35)]

#set seed so the process is reproducible
set.seed(123)

#set number of groups for the k-fold cross validation
k <-5
#set number of repetitions
r <- 5
#decide which chill year is evaluated
scen <- 'X1981' #set the column of chill values to perform analysis on

#function which gets the chill correction for a tmin and tmax entry, work only for individual values, needs to be used in a loop / apply function
get_chill_correction <-  function(tmin, tmax, lookup = pred){
  if(is.na(tmin) == T){
    return(NA)
  } else if(is.na(tmax) == T){
    return(NA)
  } else{
    tmin_index <- which.min(abs(lookup$x - tmin))
    tmax_index <- which.min(abs(lookup$y - tmax))
    return(lookup$z[tmin_index,tmax_index])
  }
}

for(rep in 1:r){
  #split data set in k even groups
  split_df <- split(stations_clean, sample(1:k, nrow(stations_clean), replace=T))
  
  #create empty list in which the values are stored
  eval_list <- vector(mode = "list", length = k)
  
  #loop, i is marking the data frame which is held back to be used as validation
  for(i in 1:k){
    #combine all splits except the one for the training
    train_df <-as.data.frame(rbindlist(split_df[-i]))
    eval_df <- split_df[[i]]
    
    #do the krigging
    krig_model <- Krig(x=as.matrix(train_df[,c("min_temp_jul","max_temp_jul")]),
                       Y=train_df[scen])
    pred <- predictSurface(krig_model)
    
    #rename rows and columns
    colnames(pred$z)<-pred$y
    rownames(pred$z)<-pred$x
    
    #melt the data frame
    melted<-melt(pred$z)
    colnames(melted)<-c("min_temp_jul","max_temp_jul","value")
    
    #do plotting
    correction_plane <- ggplot(melted,
                               aes(x=min_temp_jul,y=max_temp_jul,z=value)) +
      geom_contour_fill(bins=100) +
      scale_fill_gradientn(colours=alpha(matlab.like(15)),
                           name=paste("\nSafe Chill Portions\n[CP]",sep=''), trans = 'reverse') +
      geom_contour(col="black")  +
      geom_point(data=train_df,
                 aes(x=min_temp_jul,y=max_temp_jul,z=NULL),
                 size=0.7) +
      geom_point(data = eval_df,aes(x=min_temp_jul,y=max_temp_jul,z=NULL),
                 size = 1, shape = 25,fill = 'black')+
      geom_text_contour(stroke = 0.2,size = 2) +
      labs(title = scen)+
      ylab('Maximum Temperature, July [°C]')+
      xlab('Minimum Temperature, July [°C]')+
      theme_bw(base_size=12)
    correction_plane
    #now a number of validation points are outside the correction range
    #--> should I just exclude these or should I have a special subset of the outline points and always include them in the training data set?
    
    #add emtpy row to df to save model chill values
    eval_df$model_value <- NA
    #function to extract chill value from the model, function operates only on one row, so need for loop
    for(row in 1:nrow(eval_df)){
      eval_df$model_value[row] <- get_chill_correction(tmin = eval_df$min_temp_jul[row], tmax = eval_df$max_temp_jul[row], lookup = pred)
      
    }
    #save results to bigger data frame
    eval_list[[i]] <- eval_df
    #stations_clean <- merge.data.frame(stations_clean,eval_df[,c(1,5)],by='station_name',all.x = T,)
  }
  
  #combine all observations to one data frame
  eval_df <- as.data.frame(rbindlist(eval_list))
  
  #calculate the resiudal (observation - prediction)
  eval_df$residual <- eval_df$X1981 - eval_df$model_value
  
  if(rep == 1){
    #create new df where the results of the repititons are saved
    eval_df_final <- eval_df[,-5]
  } else {
    eval_df_final <- cbind(eval_df_final,eval_df$residual)
  }
}

eval_df_final <- transform(eval_df_final, mean_residual=apply(eval_df_final[,5:9],1, mean, na.rm = TRUE))
eval_df_final <- transform(eval_df_final, sd_residual=apply(eval_df_final[,5:9],1, sd, na.rm = TRUE))

#drop undwanted columns from eval_df_final
eval_df_final <- eval_df_final[,-c(5:9)]

summary(eval_df_final)

eval_df_final$res_binned <- cut(eval_df_final$mean_residual,breaks=c(seq(-10,10,by=2)))

correction_plane <- ggplot(eval_df_final, aes(x=min_temp_jul, y=max_temp_jul, size = abs(mean_residual),col = res_binned)) +
  geom_point()+
  scale_colour_brewer(type = 'div', palette = "RdBu")+
  ylab('Maximum Temperature, July [°C]')+
  xlab('Minimum Temperature, July [°C]')+
  theme_bw()
correction_plane
ggsave(plot = correction_plane,filename = 'figures/cross-validation/correction-model.jpg',
       height = 10,width = 15, units = 'cm')


######do actual correction plane
#krig the tmin tmax data on a plane
k <- Krig(x=as.matrix(stations_clean[,c("min_temp_jul","max_temp_jul")]),
          Y=stations_clean[scen])
pred <- predictSurface(k)
#error <- predictSurfaceSE(k)

#adjust row and column name of object
colnames(pred$z)<-pred$y
rownames(pred$z)<-pred$x
#colnames(error$z)<-error$y
#rownames(error$z)<-error$x

#melt df
melted<-melt(pred$z)
#melted_error <- melt(error$z)

colnames(melted)<-c("min_temp_jul","max_temp_jul","value")
#colnames(melted_error)<-c("min_temp_jul","max_temp_jul","value")

#plot the grid
correction_plane <- ggplot(melted,
                           aes(x=min_temp_jul,y=max_temp_jul,z=value)) +
  geom_contour_fill(bins=100,alpha = 0.1) +
  scale_fill_gradientn(colours=alpha(matlab.like(15)),
                       name=paste("\nSafe Chill Portions\n[CP]",sep=''), trans = 'reverse',guide = 'none') +
  geom_contour(col="black")  +
  geom_point(data=stations,
             aes(x=min_temp_jul,y=max_temp_jul,z=NULL),
             size=0.7) +
  geom_text_contour(stroke = 0.2,size = 2) +
  ylab('Maximum Temperature, July [°C]')+
  xlab('Minimum Temperature, July [°C]')+
  theme_bw(base_size=12)+
  geom_point(data = eval_df_final,aes(x=min_temp_jul,y=max_temp_jul,z=NULL,size = abs(mean_residual),col = res_binned))+
  scale_colour_brewer(type = 'div', palette = "RdBu")
correction_plane
ggsave(plot = correction_plane,filename = 'figures/cross-validation/correction-model2.jpg',
       height = 10,width = 15, units = 'cm')

