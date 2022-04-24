
# Start of Tutorial #8 & HW #6

# Installing Packages
# install.packages(c("caret", "randomForest", "rgdal", "sf", "raster"))

library(caret)
library(randomForest)
library(rgdal)
library(sf)
library(raster)

may <- stack("/cloud/project/activity08/May_19.tif")
june1 <- stack("/cloud/project/activity08/June_10.tif")
june2 <- stack("/cloud/project/activity08/June_18.tif")
oct <- stack("/cloud/project/activity08/Oct_12.tif")

plot(oct)


plotRGB(oct, r=3,g=2,b=1, scale=0.7, stretch="lin")


drStack <- stack("/cloud/project/activity08/May_19.tif",
                 "/cloud/project/activity08/June_10.tif",
                 "/cloud/project/activity08/June_18.tif",
                 "/cloud/project/activity08/Oct_12.tif")


plot(drStack)

plotRGB(drStack, r=3, g=2, b=1, scale=1, stretch="lin")

plot(drStack[[16:20]])


lc <- st_read("/cloud/project/activity08/land_pts.shp")
head(lc)

plot(lc["landcover"],add=TRUE, pch=19, cex=0.5,
     pal = hcl.colors(3, palette = "harmonic"))


# plot the main reforestation field and surrounding areas:
plotRGB(oct, r=3,g=2,b=1, scale=0.7, stretch="lin")
# add the known land cover points to the RGB plot
plot(lc["landcover"], add=TRUE,
     pch=19,cex=0.5, # make small filled in points
     pal= hcl.colors(3, palette = "Harmonic")) # colors from a color palette
# make a legend
legend("topleft",
       c("field", "path", "tree"), #add legend names
       pch=19, pt.cex=0.5, #make small filled in points
       col=hcl.colors(3, palette = "Harmonic"), # color palette
       bty="n")


drStack@ncols*drStack@nrows


# subset to only focus on training pts (60 in each class)
trainPts <- subset(lc, lc$train=="train", drop=FALSE)


# extract pixel data
train <- extract(drStack, trainPts)
# get attribute table from points
trainTable <- st_drop_geometry(trainPts)
# combine into one table that has a y column
# for land cover and all other data are predictions
trainDF <- na.omit(cbind(y=as.factor(trainTable[,3]), train))




# Question #1

# Random Forest
# Performing the Kfold cross validation
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
###random forests
#Typically square root of number of variables
nbands <- 20 #20 bands of information
rf.grid <- expand.grid(mtry=1:round(sqrt(nbands))) # number of variables available for splitting at each tree node


# set random seed for algorithm so you can get the same results when
# running multiple times
set.seed(43)


#note that caret:: will make sure we use train from the caret package
rf_model <- caret::train(x = trainDF[,2:21], #digital number data
                         y = as.factor(trainDF[,1]), #land class we want to predict
                         method = "rf", #use random forest
                         metric="Accuracy", #assess by accuracy
                         trainControl = tc, #use parameter tuning method
                         tuneGrid = rf.grid) #parameter t

rf_model


#use the model to predict land cover class for the entire raster stack
rf_prediction <- raster::predict(drStack, rf_model )
# plot the land cover class (uses LCID number)
plot(rf_prediction, col= hcl.colors(3, palette = "Harmonic"))


# subset land cover points for validation
validPts <- subset(lc, lc$train=="valid", drop=FALSE)
# convert to data frame
valid_Table <- st_drop_geometry(validPts)


# extract predicted land cover for each point
valid_rf <- extract(rf_prediction, validPts)
# turn into table
validDF_rf <- data.frame(y=valid_Table[,3], rf=valid_rf)


# Generating a Confusion Matrix
# LCID 1 = field
# LCID 2 =  tree
# LCID 3 = path
# confusion Matrix, first argument is prediction second is data
rf_errorM = confusionMatrix(as.factor(validDF_rf$rf),as.factor(validDF_rf$y))
# make LCID easier to interpret
colnames(rf_errorM$table) <- c("field","tree","path")
rownames(rf_errorM$table) <- c("field","tree","path")
rf_errorM




# Neural Net
# starting parameters for neural net
nnet.grid <- expand.grid(size = seq(from = 1, to = 10, by = 1), # number of neurons units in the hidden layer 
                         decay = seq(from = 0.001, to = 0.01, by = 0.001)) # regularization parameter to avoid over-fitting


# train nnet
set.seed(18)
nnet_model <- caret::train(x = trainDF[,c(2:21)], y = as.factor(trainDF[,1]),
                           method = "nnet", metric="Accuracy", 
                           trainControl = tc, tuneGrid = nnet.grid,
                           trace=FALSE)
nnet_model


# predictions
nn_prediction <- raster::predict(drStack, nnet_model)
# map
plot(nn_prediction, col= hcl.colors(3, palette = "Harmonic"))

# extract predicted land cover for each point
valid_nn <- extract(nn_prediction, validPts)
# turn into table
validDF_nn <- data.frame(y=valid_Table[,3], nn=valid_nn)


# Generating a confusion matrix
# LCID 1 = field
# LCID 2 =  tree
# LCID 3 = path
# confusion Matrix, first argument is prediction second is data
nn_errorM = confusionMatrix(as.factor(validDF_nn$nn),as.factor(validDF_nn$y))
# make LCID easier to interpret
colnames(nn_errorM$table) <- c("field","tree","path")
rownames(nn_errorM$table) <- c("field","tree","path")
nn_errorM


# Make Plots Side by Side
par(mfrow=c(1,2))
plot(nn_prediction, col= hcl.colors(3, palette = "Harmonic"),
     legend=FALSE, axes=FALSE, main="Neural network", box=FALSE)
legend("bottomleft", c("field","tree","path"),
       fill=hcl.colors(3, palette = "Harmonic") ,bty="n")

plot(rf_prediction, col= hcl.colors(3, palette = "Harmonic"),
     legend=FALSE, axes=FALSE, main="Random forest", box=FALSE)
legend("bottomleft", c("field","tree","path"),
       fill=hcl.colors(3, palette = "Harmonic") ,bty="n")




# Question #2 - Calculating NDVI

# May NDVI
ndvi_may <- (may[[4]]-may[[3]])/(may[[4]]+may[[3]])
par(mfrow=c(1,2))
plot(ndvi_may)

# June1 NDVI
ndvi_june1 <- (june1[[4]]-june1[[3]])/(june1[[4]]+june1[[3]])
par(mfrow=c(1,2))
plot(ndvi_june1)

# June2 NDVI
ndvi_june2 <- (june2[[4]]-june2[[3]])/(june2[[4]]+june2[[3]])
par(mfrow=c(1,2))
plot(ndvi_june2)

# Oct NDVI
ndvi_oct <- (oct[[4]]-oct[[3]])/(oct[[4]]+oct[[3]])
par(mfrow=c(1,2))
plot(ndvi_oct)





# Question #3

# Neural Net with larger parameter ranges
# starting parameters for neural net
nnet.gridQ3 <- expand.grid(size = seq(from = 1, to = 20, by = 2), # number of neurons units in the hidden layer 
                         decay = seq(from = 0.001, to = 0.02, by = 0.002)) # regularization parameter to avoid over-fitting


# train nnet
set.seed(18)
nnet_modelQ3 <- caret::train(x = trainDF[,c(2:21)], y = as.factor(trainDF[,1]),
                           method = "nnet", metric="Accuracy", 
                           trainControl = tc, tuneGrid = nnet.gridQ3,
                           trace=FALSE)
nnet_modelQ3


# predictions
nn_predictionQ3 <- raster::predict(drStack, nnet_modelQ3)
# map
plot(nn_predictionQ3, col= hcl.colors(3, palette = "Harmonic"))

# extract predicted land cover for each point
valid_nnQ3 <- extract(nn_predictionQ3, validPts)
# turn into table
validDF_nnQ3 <- data.frame(y=valid_Table[,3], nn=valid_nnQ3)


# Generating a confusion matrix
# LCID 1 = field
# LCID 2 =  tree
# LCID 3 = path
# confusion Matrix, first argument is prediction second is data
nn_errorMQ3 = confusionMatrix(as.factor(validDF_nnQ3$nn),as.factor(validDF_nnQ3$y))
# make LCID easier to interpret
colnames(nn_errorMQ3$table) <- c("field","tree","path")
rownames(nn_errorMQ3$table) <- c("field","tree","path")
nn_errorMQ3






