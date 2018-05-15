# This trains the supervised classification model using training data from a sample image

#==================================================================
# Load dependencies and data  
#==================================================================

library(superClass)
library(sp)

load("training_pts") # load training points selected from training image
load("test_imgs") # load training image

#==================================================================
# Download image to test
#==================================================================

map.test=GetBingMap2( # aerial image
    mapArea=c(47.591640, -122.324618, 47.598470, -122.315305),
    maptype="CanvasDark", # use aerial for final because it doesn't have labels
    # zoom=15,
    apiKey=apiKey,
    extraURL="&mapLayer=TrafficFlow",
    destfile="test.png",
    verbose=1,
    labels=FALSE
  )

image_write(image_read("test.png"), path="test.png", format="png") # not sure why we have to do this. otherwise, just using brick("test.png") results in 1 band rasterbrick
r<-brick("test.png") # load image as rasterbrick
crs(r) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs" # set projection to that used by Bing Maps

#==================================================================
# Supervised classification 
#==================================================================

# # Select training points
# # If you didn't already have points (loaded earlier) this is how you would select them by clicking on the graphics display
# 
# pts<-click(r,n=21,id=TRUE,xy=TRUE,cell=TRUE) # select points
# pts$class=c("G","G","G", # add text labels
#              "Y","Y","Y",
#              "O","O","O",
#              "R","R","R",
#              "B","B","B",
#              "A","A","A",
#              "W","W","W") 


train <- pts[c("x","y")] # extract lat/long of training points
vals <- data.frame(class=pts$class) # create a dataframe with class labels.
vals$class <- factor(vals$class) # convert to factor
train <- SpatialPointsDataFrame(train, vals, proj4string=crs("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")) # create spatial dataframe with training points
sclass <- superClass(r, trainData=train, responseCol="class",model = "rf", tuneLength = 1) # supervised classification using random forest

# sclass can be applied to other images to classify them
