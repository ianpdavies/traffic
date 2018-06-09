#==================================================================
# Train supervised classification
#==================================================================
#Ran 'run_script.R' with c(47.5, -122.5, 48, -122) for bbox, and added to traffic_api.R: file.rename(paste(time.stamp, "mosaic_proj.tif", sep=""), "traffic_classification_trainingimg.tif") 
#===================================
# get test images
apiKey = "AinLOS3zG8oO80pPTZqNx_Pl4SQvO-JhY6tNCujUOJr0iRrbACjQSuLE3_9ir849"

# smaller image to test, 2
map.test=GetBingMap2( # aerial image
  mapArea=c(47.556256, -122.296838, 47.565523, -122.281560),
  maptype="CanvasDark", # use aerial for final because it doesn't have labels
  # zoom=15,
  apiKey=apiKey,
  extraURL="&mapLayer=TrafficFlow",
  destfile="test2.png",
  verbose=1,
  labels = FALSE
)
#,labels=FALSE
image_write(image_read("test2.png"), path="test2.png", format="png") # not sure why we have to do this. otherwise, just using brick("test.png") results in 1 band rasterbrick

# read png as rasterbricks
r<-brick("traffic_classification_trainingimg.tif")
r2<-brick("test2.png")

crs(r2) <- WebMercator

#==============================================
# supervised classification of road conditions
#==============================================
# Created > 800 training points in ArcGIS, extract value from raster based on these points
pts <- readOGR(dsn='training_pts.shp') #Import training points
pts <- pts[,-1] #Remove Id column
RGBvals <- extract(r, pts, method='simple') #Get RBG values from training raster at each point's coordinates 
colnames(RGBvals) <- c("band1","band2","band3")


# create training data
pts$class <- factor(pts$class) # convert to factor
pts <- cbind(pts, RGBvals)
sclass <- superClass(r, trainData=pts, responseCol="class",model = "rf", tuneLength = 1) # supervised classification

# plot classified image vs. original image to check
m <- rbind(c(1, 2))
layout(m)
plotRGB(r)
plot(sclass$map)
writeRaster(sclass$map,'training_class_output.tif' , format="GTiff")

# now try using classification on another image
names(r2) <- names(r) # give dataframe columns (i.e. image bands) the same names
sclass2 <- predict(r2, sclass$model) # classify using model generated from training points
plot(sclass2)

save(sclass, sclass2, file="sclasses")
#load("sclasses")