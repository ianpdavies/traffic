
#=================================================================================================================
# This is the "testing script" to tweak all of the other functions. Can uncomment the script to run from scratch, 
# or run as it is and just use the saved files. Does not include mosaicking since we test with a small image.

#=================================================================================================================


#===================================
# Get small test images 

# # smaller image to test, 1
# map.test=GetBingMap2( # aerial image
#   mapArea=c(47.591640, -122.324618, 47.598470, -122.315305),
#   maptype="CanvasDark", # use aerial for final because it doesn't have labels
#   # zoom=15,
#   apiKey=apiKey,
#   extraURL="&mapLayer=TrafficFlow",
#   destfile="test.png",
#   verbose=1,
#   labels=FALSE
# )
# image_write(image_read("test.png"), path="test.png", format="png") # not sure why we have to do this. otherwise, just using brick("test.png") results in 1 band rasterbrick
# 
# # smaller image to test, 3, but this time at the minimum map tile size
# z4 <- c(-122.324618, 47.598470) # top left corner
# z1<-destPointRhumb(z4, b=90, r=radius, d=img.size) # find distance to top right corner using the size of map tile at zoom 15, 640 px
# z2 <- destPointRhumb(z1, b=180, r=radius, d=img.size)# bottom right corner
# z3 <- destPointRhumb(z2, b=-90, r=radius, d=img.size)# bottom left corner
# 
# 
# map.test=GetBingMap2( 
#   mapArea=c(z3[2], z3[1], z1[2], z1[1]),
#   maptype="CanvasDark", 
#   # zoom=15,
#   apiKey=apiKey,
#   extraURL="&mapLayer=TrafficFlow",
#   destfile="test3.png",
#   verbose=1,
#   labels=FALSE
# )
# 
# image_write(image_read("test3.png"), path="test3.png", format="png") 
# 
# # read png as rasterbricks
# r<-brick("test.png")
# r3<-brick("test3.png")
# crs(r) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
# crs(r3) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
# 
# save(r, r3, file="test_imgs")

load("test_imgs") # load saved static maps. 

#===================================
# supervised classification

# Idea: use a supervised classification, then apply the classification model to other images


# # get training points and values
# pts<-click(r,n=21,id=TRUE,xy=TRUE,cell=TRUE)
# pts$class=c("G","Y","O","R","B","A","W") # add text labels

# pts2<-click(r,n=21,id=TRUE,xy=TRUE,cell=TRUE)
# pts2$class=c("G","G","G",
#              "Y","Y","Y",
#              "O","O","O",
#              "R","R","R",
#              "B","B","B",
#              "A","A","A",
#              "W","W","W") # add text labels

# load("training_pts")
load("training_pts") # training points for image r with traffic colors

# create training data
train <- pts[c("x","y")] # extract lat/long of training points
vals <- data.frame(class=pts$class) # create a dataframe with class labels. 
vals$class <- factor(vals$class) # convert to factor
crs(r) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs" # assign CRS used by bing maps
train <- SpatialPointsDataFrame(train, vals, proj4string=crs("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")) # create spatial dataframe with training points

sclass <- superClass(r, trainData=train, responseCol="class",model = "rf", tuneLength = 1) # supervised classification


# plot classified image vs. original image to check
m <- rbind(c(1, 2))
layout(m)
plotRGB(r)
plot(sclass$map)

# now try using classification on another image
names(r3) <- names(r) # give dataframe columns (i.e. image bands) the same names
sclass3 <- predict(r3, sclass$model) # classify using model generated from training points

#===================================
# extract raster values to road vector

# fgdb <- "C:\\Users\\ipdavies\\Documents\\traffic\\bing\\24KLRS_2016\\24k.gdb"
# roads <- readOGR(dsn=fgdb, layer="sr24kLines_20161231")
# roads <- readOGR(dsn="C:\\Users\\ipdavies\\Documents\\traffic\\bing\\StreetMap_Data\\data", layer="mroads")
# save(roads, file="roads")
load("roads")

# clip road feature to raster extent
r.class <- sclass3
projection(r.class) = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")
xmin(r.class) <-  z3[1]
xmax(r.class) <- z1[1] 
ymin(r.class) <- z3[2]
ymax(r.class) <- z1[2] 


# project and add extent in meters to raster
r.class <- projectRaster(r.class,"+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")
# matrix with extent coordinates
coords <- matrix(c(z3[1],z3[2], # xmin, ymin
                   z2[1], z2[2], # xmax, ymin
                   z1[1], z1[2], # xmax, ymax
                   z4[1], z4[2]), # xmin, ymax
                 nrow=4, byrow=TRUE)
# reproject coords from lat/long to meter
rproj <- project(coords, "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")
rproj <- list(x=rproj[,1], y=rproj[,2]) # separate east/west and north/south coordinates
extent(r.class) <- rproj # set extent of raster

# reproject and clip road to raster extent
# roads <- spTransform(roads,crs("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"))
# roads_clip = crop(roads, r.class)
# save(roads_clip, file="roads_clip")

plot(r.class)
plot(roads_clip, add=TRUE)

writeRaster(r.class, filename="rclass3.tif", options=c('TFW=YES'), overwrite=TRUE)
writeOGR(obj=roads_clip, dsn="C:\\Users\\ipdavies\\Documents\\traffic\\bing\\images", layer="road_clip", driver="ESRI Shapefile")
