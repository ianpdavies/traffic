#==================================================================
# Description
#==================================================================


# This script downloads static map images from Bing Maps API, mosaicks them together, and performs a supervised classification
# to classify traffic conditions. This script is meant to be run multiple times each day for a week to get a large sample of images
# which can then be summarized using raster math to get average traffic conditions

# NOTES: 
# This script requires Windows to run because it uses the Windows Task Scheduler
# You will need to get your own Bing Maps REST API key and save it as a text file ("bing_key.txt") for this to work
# Depending on the size of the image and how long you average for (recommended 1 week) this script results in over 100 large pngs - make sure your hard drive has space!
# (Eventually, we should remove the mosaicked pngs after classification. Keeping them for first few runs to make sure everything works as intended)

#==================================================================
# Dependencies
#==================================================================
print(Sys.time())
library(RgoogleMaps) # for accessing Bing Maps API # Need it for XY2LatLon
library(stringr) # stringr::str_pad for file naming
library(geosphere) # for spherical geometry calculations
library(raster) # for classification and extraction to road network
library(rgdal) # for geographic transformations and projections
library(magick) # for mosaicking static images together
library(taskscheduleR) # for scheduling hourly image downloads
library(RStoolbox) # don't need this - perhaps for sclass?
library(OSMscale) #problably not useful after troubleshooting

#==================================================================
# Set constants and map parameters
#==================================================================

source("map_api_edit.R") # edited function GetBingMaps from package `RGoogleMaps`
load("mapValues")

#Convert lat-long coordinates to Bing map to raw pixel coordinates
latlong_to_pixelcoords <- function(lat, lon, zoom) {
  rawXY <- LatLon2XY(lat,lon,zoom) #Lat Long to tile # + pixel coordinate within tile
  pixX <- rawXY$Tile[1,][1]*256+floor(rawXY$Coords[1,][1]) #Pixel coordinate = tile number*256+pixel coordinate within tile 
  pixY <- rawXY$Tile[1,][2]*256+floor(rawXY$Coords[1,][2]) #There are always 256 pixels/tile and 2^zoom tiles in the world, so 256*2^zoom pixels in the world
  return(c(pixX, pixY))
}

#Convert pixel coordinates to lat-long coordinates
pixelcoords_to_latlong <- function(pixY, pixX, zoom) {
  mapSize <- 256*2^zoom
  dx = (pixX/mapSize) - 0.5
  dy = 0.5 - (pixY/mapSize)
  latitude = 90-360*atan(exp(-dy * 2 * pi))/pi
  longitude = 360*dx
  return(c(latitude,longitude))
}

#==================================================================
# Set constants and map parameters - FOR TROUBLESHOOTING
#==================================================================
# These will be called in the traffic_api.R script 

# apiKey = scan("bing_key.txt", what="") # text file with Bing Maps API key
apiKey = "AinLOS3zG8oO80pPTZqNx_Pl4SQvO-JhY6tNCujUOJr0iRrbACjQSuLE3_9ir849"

# projection used by Bing Maps (Official reference, EPSG:3857)
myProj <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"

# Full extent of area to be covered.
#bbox <- c(47.5, -122.5, 48, -122)  # Coordinates are lower left and upper right lat/long (in that order)
bbox <- c(47.614086, -122.235819, 47.637687, -122.128015)  # Coordinates are lower left and upper right lat/long (in that order)

# calculate optimal number of images to fetch
zoom <- 15
radius = 6378137 # radius of earth used in Bing Maps
px = 500 # length of the image in pixels (maximum that Bing/Google will output)

#Computer pixel coordinate X and Y for the lower left and upper right corner of every row and column
bbox.list.ll.x <- seq(latlong_to_pixelcoords(bbox[1],bbox[2],zoom)[1],latlong_to_pixelcoords(bbox[3],bbox[4],zoom)[1],px)
bbox.list.ur.x <- bbox.list.ll.x + (px-1)
bbox.list.ur.y <- seq(latlong_to_pixelcoords(bbox[3],bbox[4],zoom)[2],latlong_to_pixelcoords(bbox[1],bbox[2],zoom)[2],px)
bbox.list.ll.y <- bbox.list.ur.y + (px-1)

imgs.w <- length(bbox.list.ll.x) #Number of columns
imgs.h <- length(bbox.list.ll.y) #Number of rows

save(apiKey, myProj, zoom, radius, px,imgs.w, imgs.h, file = "mapValues")

#==================================================================
# Download static images
#==================================================================

map.params <- list(maptype="CanvasDark", # parameters needed to construct URL for API call
                   zoom = zoom,
                   apiKey=apiKey,
                   extraURL="&mapLayer=TrafficFlow",
                   verbose=1,
                   size=px,
                   labels=FALSE) #Setting labels=FALSE removes all features and labels of the map but roads and traffic. 

time.stamp <- format(Sys.time(), "%y%m%d_%H_%M_") # want all images taken in an instance to have same timestamp


imgs <- c() # holds images
coords <- NULL # holds coordinates of each image
for (i in 1:imgs.h){ #loops over rows
  for(j in 1:imgs.w){ # loops over columns
    extent.img.ll <- pixelcoords_to_latlong(bbox.list.ll.y[i], bbox.list.ll.x[j], zoom) #Define coordinates of four corners of each image (for later georeferencing)
    extent.img.lr <- pixelcoords_to_latlong(bbox.list.ll.y[i], bbox.list.ur.x[j], zoom)
    extent.img.ul <- pixelcoords_to_latlong(bbox.list.ur.y[i], bbox.list.ll.x[j], zoom)
    extent.img.ur <- pixelcoords_to_latlong(bbox.list.ur.y[i], bbox.list.ur.x[j], zoom)
    extent.img <- c(extent.img.ll, extent.img.ur) #Lower left, upper right coordinates for Bing Map download
    #################Create projected polygon envelopes for each image, to troubleshoot and visualize##########
    # filename <- paste(time.stamp, # time stamp
    #                   str_pad(j, nchar(imgs.h), pad = "0"), "_", # pad img number with leading zeros and row number
    #                   str_pad(i, nchar(imgs.w), pad = "0"),"_envelope")
    # poly <- as(raster::extent(extent.img[c(2,4,1,3)]),"SpatialPolygons")
    # proj4string(poly) <- CRS("+proj=longlat +datum=WGS84")
    # polydf <- SpatialPolygonsDataFrame(poly, data.frame(ID=filename))
    # writeOGR(polydf ,getwd(),filename,driver="ESRI Shapefile", overwrite_layer = T)
    #####
    filename <- paste(time.stamp, # time stamp
                      str_pad(j, nchar(imgs.h), pad = "0"), "_", # pad img number with leading zeros and row number
                      str_pad(i, nchar(imgs.w), pad = "0"), # pad img number with leading zeros and column number
                      ".png", sep="")
    map <- do.call(GetBingMap2, c(list(mapArea=extent.img,destfile=filename), map.params))# download map
    imgs <- c(imgs, filename) # list of filenames
    coords.tmp <- c(extent.img.ul,extent.img.ll,extent.img.lr,extent.img.ur)
    coords <- rbind(coords, coords.tmp) # upper left, lower left, lower right, upper right
    print(coords.tmp)
  }
}
rm(i,j,coords.tmp, map, extent.img) # remove temp objects

#==================================================================
# Mosaic images into one raster
#==================================================================

mu<-list(image_read(paste(getwd(), "/", imgs, sep=""))) # get all saved images from files

p<-list() # empty list to hold appended rows of images
for(i in 1:imgs.h){ # for each row of images, create an element in list p of east-west appended images
  print(i)
  print((imgs.w*(i-1)+1):(imgs.w*i))
  p[[i]] <- image_append(mu[[1]][(imgs.w*(i-1)+1):(imgs.w*i)])
}

w <- p[[1]] # create mosaicked image starting with first row 
if(imgs.h > 1){ # if there's more than one row of images, append the other rows
  for(j in 2:imgs.h){
    w <- image_append(c(w, p[[j]]), stack=TRUE)
  }
}
rm(i,j)

# save mosaic raster
image_write(w, path=paste(time.stamp, "mosaic.png", sep=""), format="png")

# create log of mosaic image names
write(paste(time.stamp, "mosaic.png", sep=""), file="image_log.txt", append=TRUE)

# delete image pieces
file.remove(imgs)
rm(mu)

#==================================================================
# Georeference the raster
#==================================================================
r<-brick(paste(time.stamp, "mosaic.png", sep=""), package="raster") # convert mosaic image to a rasterbrick object

#Create a polygon envelope with long-lat of actual bounding box, project coordinates to Bing Map projected coordinate system (Web Mercator)
#Then use it as a template to georeference image 
envelope <- c(min(coords[,c(2,4,6,8)]),max(coords[,c(2,4,6,8)]),min(coords[,c(1,3,5,7)]), max(coords[,c(1,3,5,7)])) #min lat, max lat, min long, max lon
poly <- as(raster::extent(envelope),"SpatialPolygons")
proj4string(poly) <- CRS("+proj=longlat +datum=WGS84")
WebMercator <- CRS("+init=epsg:3857")
polyproj <- spTransform(poly, CRSobj=WebMercator)

#Define extent in Web Mercator coordinates
xmax(r) <- xmax(polyproj) # max lon
xmin(r) <- xmin(polyproj) # min lon
ymax(r) <- ymax(polyproj) # max lat
ymin(r) <- ymin(polyproj) # min lat

crs(r) <- WebMercator # Define coordinate system

writeRaster(r, filename=paste(time.stamp, "mosaic_proj.tif", sep=""), format="GTiff", overwrite=TRUE)
print(Sys.time())

#=================================================
# Supervised classification of traffic conditions
#=================================================

load("sclasses") # get trained classification model
names(r) = c("band1","band2","band3") # give image bands the same names as those used in sclass
rclass <- predict(r, sclass$model) # classify using model generated from training points

# save as compressed geotiff
writeRaster(rclass, filename=paste(time.stamp, "class.tif", sep=""), format="GTiff", overwrite=TRUE)

# create log of classified image names
write(paste(time.stamp, "class.tif", sep=""), file="classified_image_log.txt", append=TRUE)

plot(rclass)
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================
#===================================================================


#==================================================================
# TESTING DIFFERENT METHODS
#==================================================================

#===================================
# get test images
apiKey = "AinLOS3zG8oO80pPTZqNx_Pl4SQvO-JhY6tNCujUOJr0iRrbACjQSuLE3_9ir849"

# # smaller image to test, 1
# map.test=GetBingMap2( # aerial image
#     mapArea=c(47.591640, -122.324618, 47.598470, -122.315305),
#     maptype="Aerial", # use aerial for final because it doesn't have labels
#     # zoom=15,
#     apiKey=apiKey,
#     extraURL="&mapLayer=TrafficFlow",
#     destfile="test.png",
#     verbose=1
#   )
# #,labels=FALSE
# image_write(image_read("test.png"), path="test.png", format="png") # not sure why we have to do this. otherwise, just using brick("test.png") results in 1 band rasterbrick
# 
# # smaller image to test, 2
# map.test=GetBingMap2( # aerial image
#   mapArea=c(47.556256, -122.296838, 47.565523, -122.281560),
#   maptype="Aerial", # use aerial for final because it doesn't have labels
#   # zoom=15,
#   apiKey=apiKey,
#   extraURL="&mapLayer=TrafficFlow",
#   destfile="test2.png",
#   verbose=1
# )
# #,labels=FALSE
# image_write(image_read("test2.png"), path="test2.png", format="png") # not sure why we have to do this. otherwise, just using brick("test.png") results in 1 band rasterbrick
# 
# # smaller image to test, 3, but this time at the minimum map tile size
# 
# z4 <- c(-122.324618, 47.598470) # top left corner
# z1<-destPointRhumb(z4, b=90, r=radius, d=img.size) # find distance to top right corner using the size of map tile at zoom 15, 640 px
# z2 <- destPointRhumb(z1, b=180, r=radius, d=img.size)# bottom right corner
# z3 <- destPointRhumb(z2, b=-90, r=radius, d=img.size)# bottom left corner
# 
# 
# map.test=GetBingMap2(
#   mapArea=c(z3[2], z3[1], z1[2], z1[1]),
#   maptype="Aerial",
#   # zoom=15,
#   apiKey=apiKey,
#   extraURL="&mapLayer=TrafficFlow",
#   destfile="test3.png",
#   verbose=1
# )
# #,labels=FALSE
# image_write(image_read("test3.png"), path="test3.png", format="png")
# 
# # read png as rasterbricks
# r<-brick("test.png")
# r2<-brick("test2.png")
# r3<-brick("test3.png")
# crs(r) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
# crs(r2) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
# crs(r3) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
# 
# save(r,r2, r3, file="test_imgs")
# load("test_imgs")


#==============================================
# supervised classification of road conditions
#==============================================

# # get training points and values
# pts<-click(r,n=21,id=TRUE,xy=TRUE,cell=TRUE)
# pts$class=c("G","Y","O","R","B","A","W") # add text labels
# 
plotRGB(r)
pts2<-click(r,n=42,id=TRUE,xy=TRUE,cell=TRUE)
pts2$class=c("G","G","G","G","G","G",
             "Y","Y","Y","Y","Y","Y",
             "O","O","O","O","O","O",
             "R","R","R","R","R","R",
             "B","B","B","B","B","B",
             "A","A","A","A","A","A",
             "W","W","W","W","W","W") # add text labels

# load("training_pts")
load("training_pts")

#pts <- pts2
# create training data
train <- pts[c("x","y")] # extract lat/long of training points
vals <- data.frame(class=pts$class) # create a dataframe with class labels. 
vals$class <- factor(vals$class) # convert to factor
crs(r) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs" # assign CRS used by bing maps
train <- SpatialPointsDataFrame(train, vals, proj4string=crs("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")) # create spatial dataframe with training points
names(r) <- c("band1","band2","band3")
sclass <- superClass(r, trainData=train, responseCol="class",model = "rf", tuneLength = 1) # supervised classification

# plot classified image vs. original image to check
m <- rbind(c(1, 2))
layout(m)
plotRGB(r)
plot(sclass$map)

# now try using classification on another image
names(r2) <- names(r) # give dataframe columns (i.e. image bands) the same names
sclass2 <- predict(r2, sclass$model) # classify using model generated from training points

# now try using classification on another image
names(r3) <- names(r) # give dataframe columns (i.e. image bands) the same names
sclass3 <- predict(r3, sclass$model) # classify using model generated from training points

save(sclass, sclass2, sclass3, file="sclasses")
load("sclasses")


