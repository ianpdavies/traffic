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
library(RStoolbox) # don't need this - perhaps for sclass?
library(OSMscale) #problably not useful after troubleshooting

#==================================================================
# Set constants and map parameters
#==================================================================
setwd('F:/Levin_Lab/stormwater/src/traffic')
source("map_api_edit.R") # edited function GetBingMaps from package `RGoogleMaps`
source("coord_conversion.R")
load("mapValues")
load("sclasses") #Load trained classification model
setwd('F:/Levin_Lab/stormwater/results/bing')

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
    #print(coords.tmp)
  }
}
rm(i,j,coords.tmp, map, extent.img) # remove temp objects

#==================================================================
# Mosaic images into one raster
#==================================================================

mu<-list(image_read(paste(getwd(), "/", imgs, sep=""))) # get all saved images from files

p<-list() # empty list to hold appended rows of images
for(i in 1:imgs.h){ # for each row of images, create an element in list p of east-west appended images
  #print((imgs.w*(i-1)+1):(imgs.w*i))
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
file.remove(paste0(imgs,'.rda'))
rm(mu)

#==================================================================
# Georeference the raster
#==================================================================
r<-brick(paste(time.stamp, "mosaic.png", sep=""), package="Raster") # convert mosaic image to a rasterbrick object
rm(w)
#Create a polygon envelope with long-lat of actual bounding box, project coordinates to Bing Map projected coordinate system (Web Mercator)
#Then use it as a template to georeference image 
envelope <- c(min(coords[,c(2,4,6,8)]),max(coords[,c(2,4,6,8)]),min(coords[,c(1,3,5,7)]), max(coords[,c(1,3,5,7)])) #min lat, max lat, min long, max lon

#Could be simplified to:
# envelope <- matric(c(min(coords[,c(2,4,6,8)]),max(coords[,c(2,4,6,8)]),
#                       min(coords[,c(1,3,5,7)]), max(coords[,c(1,3,5,7)]),
#                      nrow=4, byrow=TRUE))
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

#writeRaster(r, filename=paste(time.stamp, "mosaic_proj.tif", sep=""), format="GTiff", overwrite=TRUE)
#print(Sys.time())
#file.rename(paste(time.stamp, "mosaic_proj.tif", sep=""), "traffic_classification_trainingimg.tif") #for training points

#=================================================
# Supervised classification of traffic conditions
#=================================================
names(r) = c("band1","band2","band3") # give image bands the same names as those used in sclass
rclass <- predict(r, sclass$model) # classify using model generated from training points

# save as compressed geotiff
writeRaster(rclass, filename=paste(time.stamp, "class.tif", sep=""), format="GTiff", datatype='INT2U',overwrite=TRUE)

# create log of classified image names
write(paste(time.stamp, "class.tif", sep=""), file="classified_image_log.txt", append=TRUE)