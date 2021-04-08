#example taken from http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/
#cross validation

library(tidyverse)
library(caret) #needed for cross validation

# Load the data
data("swiss")
# Inspect the data
sample_n(swiss, 3)

# Split the data into training and test set
set.seed(123)
training.samples <- swiss$Fertility %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- swiss[training.samples, ]
test.data <- swiss[-training.samples, ]
# Build the model
model <- lm(Fertility ~., data = train.data)
# Make predictions and compute the R2, RMSE and MAE
predictions <- model %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$Fertility),
            RMSE = RMSE(predictions, test.data$Fertility),
            MAE = MAE(predictions, test.data$Fertility))

RMSE(predictions, test.data$Fertility)/mean(test.data$Fertility)

#--> disadvantage: the model performance highly depends on which data is left out

#####
#leave one out cross validation
#####

# Define training control
train.control <- trainControl(method = "LOOCV")
# Train the model
model <- train(Fertility ~., data = swiss, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

#---> disadvantage: this process is done for every bit of observation, so possibly high run time
#furthermore, if data is outlier the scores become variable 

#####
#k-fold cross validation
#####

# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(Fertility ~., data = swiss, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)


######
#repeated cross validation: the process of splitting the data set is carried out several times
######
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
# Train the model
model <- train(Fertility ~., data = swiss, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)



########
#test cross validation with kriging
########
library(gstat)
library(sp)
data(meuse)
coordinates(meuse) <- ~x+y
m <- vgm(.59, "Sph", 874, .04)
# five-fold cross validation:
x <- krige.cv(log(zinc)~1, meuse, m, nmax = 40, nfold=5)

plot(SA)
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

stations <- read.csv('southamerica_chill/chill_south_america/data/all_chill_projections.csv')
Porig<-SpatialPointsDataFrame(stations[,c("Longitude","Latitude")],
                              proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                              data=stations[,c(7:28,52,62)])
scen <- 'X1981'

f.1 <- as.formula(paste(scen, "~ Longitude + Latitude"))
var.smpl <- variogram(f.1, Porig, cloud = FALSE)
dat.fit <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                         vgm(psill = 275, model="Sph", nugget = 20, range = 1500))
out <- krige.cv(f.1, Porig, dat.fit, nfold = 10)
out$abs_residual <- abs(out$residual)
bubble(out, "residual", main = "chill 1981: 10-fold CV residuals")

library(tmap)
library(raster)
library(sf)
data(World)#for map of south america


countries <- c('FRA','ARG', 'BOL', 'BRA', 'CHL', 'COL', 'ECU',  'GUY', 'PER', 'PRY', 'SUR', 'URY', 'VEN')
SA <- World[World$iso_a3 %in% countries,]

#set bounding box for SA
SA_region = extent(c(-8000000,-3000000,-6700000,1700000))
SA <- as_Spatial(SA) #convert to S4 object
SA <- crop(SA,SA_region) #crop france out of the selection, because it was needed for french guyana
SA <- spTransform(SA,CRSobj = crs(Porig))

tm_shape(SA)+
  tm_borders(col = 'black')+
  tm_shape(out) + 
  tm_bubbles(size = 'abs_residual',col = 'residual')+
  tm_grid()+ 
  tm_layout(legend.outside=T,outer.margins = c(0.001,0.001,0,0.001))


#cross validation auch bei adjustment modell machen

#check for outliers in the tmean and remove them
is_outlier <- abs(stations$avg_temp_jul - stations$obs_avg_temp_jul) > 2
stations$outlier <- is_outlier
stations_clean <- stations[!stations$outlier,]

#make stations_clean spatial
test <- SpatialPointsDataFrame(stations_clean[,c("min_temp_jul","max_temp_jul")],
                       data=stations_clean[,c(7:28)])
#formula for kriging
f.1 <- as.formula(paste(scen, "~ min_temp_jul + max_temp_jul"))
var.smpl <- variogram(f.1, test, cloud = FALSE)
dat.fit <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                         vgm(psill = 350, model="Gau", nugget = 40, range = 8))
plot(var.smpl,dat.fit)

out <- krige.cv(f.1, test, dat.fit, nfold = 10)
bubble(out, "residual", main = "chill 1981: 10-fold CV residuals",)

#irgendwie funktioniert das nicht mit nicht-räumlichen coordinaten. gibt es bei fields auch eine cross validation function oder muss ich das 'von hand' machen?
