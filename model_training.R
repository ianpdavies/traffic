library(raster) # for classification and extraction to road network
library(rgdal) # for geographic transformations and projections
library(magick)
library(RgoogleMaps)
library(httr) #for in-memory download of API image
library(RStoolbox) 

setwd('F:/Levin_Lab/stormwater/src/traffic')
source("map_api_edit.R") # edited function GetBingMaps from package `RGoogleMaps`
setwd('F:/Levin_Lab/stormwater/data')

#==================================================================
# Train supervised classification
#==================================================================
#Ran 'run_script.R' with c(47.5,-122.7,47.8,-122) for bbox, 180615_12_08, and added to traffic_api.R: file.rename(paste(time.stamp, "mosaic.tif", sep=""), "traffic_classification_trainingimg.tif") 
#===================================
# get test images
apiKey = "AinLOS3zG8oO80pPTZqNx_Pl4SQvO-JhY6tNCujUOJr0iRrbACjQSuLE3_9ir849"

# smaller image to test, 2
map.test=GetBingMap2( # aerial image
  mapArea=c(47.556256, -122.296838, 47.565523, -122.281560),
  maptype="CanvasDark", # use aerial for final because it doesn't have labels
  zoom=15,
  apiKey=apiKey,
  extraURL="&mapLayer=TrafficFlow",
  destfile="test2.png",
  verbose=0,
  size=1500,
  DISK=FALSE,
  MEMORY=TRUE,
  labels = FALSE
)

# read png as rasterbricks
r<-brick("traffic_classification_trainingimg.tif")
names(r) <- c("band1","band2","band3")
#==============================================
# supervised classification of road conditions
#==============================================
# Created > 700 training points in ArcGIS, extract value from raster based on these points
pts <- readOGR(dsn='trainings_pts.shp') #Import training points
pts <- pts[,-1] #Remove Id column
pts <- spTransform(pts, crs(r))
RGBvals <- extract(r, pts, method='simple') #Get RBG values from training raster at each point's coordinates 
colnames(RGBvals) <- c("band1","band2","band3")
pts$class <- factor(pts$class) # convert to factor
pts <- cbind(pts, RGBvals)

####Random forest model###
sclass_rf <- superClass(r, trainData=pts, responseCol="class",model = "rf", tuneLength = 1, kfold=5) # supervised classification
sclass_rf$modelFit
# plot classified image vs. original image to check
m <- rbind(c(1, 2))
layout(m)
plotRGB(r)
plot(sclass_rf$map)
writeRaster(sclass_rf$map,'training_class_output_rf.tif' , format="GTiff", overwrite=T)

###MLC###
sclass_mlc <- superClass(r, trainData=pts, responseCol="class",model = "mlc", tuneLength = 1, kfold=5) # supervised classification
sclass_mlc$modelFit
writeRaster(sclass_mlc$map,'training_class_output_mc.tif' , format="GTiff", overwrite=T)

save(sclass_rf, sclass_mlc, file="sclasses")
#load("sclasses")