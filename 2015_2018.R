library(sp)
library(raster)
library(sf)
library(foreign)
library(shapefiles)
library (RStoolbox)
library(lattice)
library(ggplot2)
library(caret)
library(randomForest)
library(e1071)
library(rgdal)

#Read the tiff file from gee
Landsat<-stack("C:/Users/Ileeya/Documents/ArcGIS/2015/2015_2018mask.tif")
Landsat

# read the shape file of the training points
setwd("C:/Users/Ileeya/Documents/ArcGIS/training_ points")
train_shp <- readOGR(".","Delta_",stringsAsFactors = T)
str(train_shp@data)

# train_shp@data$Cover[train_shp@data$Cover== 1] <- "Water"

train_shp@data$Cover <- factor(train_shp@data$Cover, levels = c("1","2","3","4","5","6","7","8"))
train_shp@data$Cover



ext <- extract(Landsat,train_shp)
colnames(ext) <- c("Blue","Green","Red","NIR","SWIR1","SWIR2")
vals <- data.frame(ID=train_shp$Cover,LC=train_shp$Cover,as.data.frame.matrix(ext))
head(vals)


which(coordinates(train_shp)[,2] < 472748.973276)
which(coordinates(train_shp)[,1] < 711137.131873)

train_shp[c(2004,34545,6740),]@data

train_shp <- train_shp[-c(2004,34545,6740),]
plot(train_shp)
plot(Landsat)

#train_shp@data$Cover <- as.factor(train_shp@data$Cover)
#str(train_shp@data)

## Fit classifier (splitting training into 50% training data, 50% validation data)
SC <- superClass(Landsat, trainData = train_shp, responseCol = "Land_cover", model = "rf", tuneLength = 1, trainPartition = 0.5)
SC

SC <- superClass(Landsat, trainData = train_shp, responseCol = "Land_cover", model = "rf", tuneLength = 1, trainPartition = 0.5, mode = "classification", predict = "TRUE", filename = "C:/Users/Ileeya/Documents/ArcGIS/2015/out2018_15_3mask.tif", overwrite = TRUE)


SC <- superClass(Landsat, trainData = train_shp, responseCol = "Id", model = "rf", tuneLength = 1, trainPartition = 0.7)
SC