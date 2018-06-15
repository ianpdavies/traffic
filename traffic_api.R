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
library(RgoogleMaps) # for accessing Bing Maps API # Need it for XY2LatLon
library(stringr) # stringr::str_pad for file naming
library(geosphere) # for spherical geometry calculations
library(raster) # for classification and extraction to road network
library(rgdal) # for geographic transformations and projections
library(RStoolbox) # don't need this - perhaps for sclass?
library(httr) #for in-memory download of API image
library(gdalUtils) #for mosaicking

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
                   verbose=0,
                   size=px,
                   DISK=FALSE,
                   MEMORY=TRUE,
                   labels=FALSE) #Setting labels=FALSE removes all features and labels of the map but roads and traffic. 

time.stamp <- format(Sys.time(), "%y%m%d_%H_%M_") # want all images taken in an instance to have same timestamp

tic()
imgs <- c() # holds images
coords <- NULL # holds coordinates of each image
for (i in 1:imgs.h){ #loops over rows
  for(j in 1:imgs.w){ # loops over columns
    print(100*(imgs.w*(i-1)+j)/(imgs.w*imgs.h))
    extent.img.ll <- pixelcoords_to_latlong(bbox.list.ll.y[i], bbox.list.ll.x[j], zoom) #Define coordinates of four corners of each image (for later georeferencing)
    extent.img.lr <- pixelcoords_to_latlong(bbox.list.ll.y[i], bbox.list.ur.x[j], zoom)
    extent.img.ul <- pixelcoords_to_latlong(bbox.list.ur.y[i], bbox.list.ll.x[j], zoom)
    extent.img.ur <- pixelcoords_to_latlong(bbox.list.ur.y[i], bbox.list.ur.x[j], zoom)
    coords.tmp <- c(extent.img.ul,extent.img.ll,extent.img.lr,extent.img.ur)
    envelope <- as(raster::extent(coords.tmp[c(4,8,3,7)]),"SpatialPolygons")
    proj4string(envelope) <- CRS("+proj=longlat +datum=WGS84")
    if (polybound==TRUE & all(is.na(over(spTransform(envelope,CRSobj=CRS(proj4string(PSwatershed))), PSwatershed)))) { #Make sure that tile falls within boundaries of Puget Sound watershed
     print(paste0('Row ',i,', column ',j,' does not intersect with polygon boundaries'))
    } else {
    filename <- paste(time.stamp, # time stamp
                        str_pad(i, nchar(imgs.h), pad = "0"), "_", # pad img number with leading zeros and row number
                        str_pad(j, nchar(imgs.w), pad = "0"), # pad img number with leading zeros and column number
                        ".tif", sep="")
    map <- 255L*brick(do.call(GetBingMap2, c(list(mapArea=coords.tmp[c(3,4,7,8)],destfile=filename), map.params)))-1L # download map based on lower left and upper right coordinates
    ##########ATTEMPT ################
    envelope <- spTransform(envelope, CRSobj=WebMercator)
    #Define extent in Web Mercator coordinates
    xmax(map) <- xmax(envelope) # max lon
    xmin(map) <- xmin(envelope) # min lon
    ymax(map) <- ymax(envelope) # max lat
    ymin(map) <- ymin(envelope) # min lat
    crs(map) <- WebMercator # Define coordinate system
    
    writeRaster(map, filename, format="GTiff",datatype='INT1U', overwrite=TRUE)
    
    imgs <- c(imgs, filename) # list of filenames
    #coords <- rbind(coords, coords.tmp) # upper left, lower left, lower right, upper right
    #print(coords.tmp)
    }
  }
}
rm(i,j,coords.tmp, map) # remove temp objects
print(paste0('Downloading tiles took ', toc()))

#==================================================================
# Mosaic images into one raster
#==================================================================
tic()
#mu<-list(image_read(paste0(getwd(), "/", imgs))) # get all saved images from files
mosaic <- mosaic_rasters(paste0(getwd(), "/", imgs), paste0(time.stamp, "mosaic.tif"), output_Raster=T)
toc()

#Remove tiles
file.remove(imgs)
file.remove(paste0(imgs,'.rda'))

#=================================================
# Supervised classification of traffic conditions
#=================================================
tic()
names(mosaic) = c("band1","band2","band3") # give image bands the same names as those used in sclass
rclass <- predict(mosaic, sclass$model) # classify using model generated from training points

Sys.time()
tic()
mosaicrange <- max(mosaic)-min(mosaic) #Took 2 minutes
rclass[mosaicrange<=50] <- 1
toc()

###### Try multiple majority filter methods ######
#spatial.tools::rasterEngine
library(spatial.tools)
majority_smoother <- function(x) {
  #Assumes 3-d array
  uniquex <- unique(x)
  uniquex[which.max(tabulate(match(x,uniquex)))]
}
tic()
sfQuickInit()
rclassmajority <- rasterEngine(x=rclass, fun=majority_smoother, window_dims=c(3,3))
sfQuickStop()
toc()

#raster::focal
tic()
rclassmajority <- focal(rclass, w=matrix(1,3,3), fun=modal, pad=T, na.rm=T)
toc()
writeRaster(rclassmajority, filename=paste(time.stamp, "classmaj.tif", sep=""), format="GTiff", datatype='INT2U',overwrite=TRUE)

#python arcpy




# save as compressed geotiff
writeRaster(rclass, filename=paste(time.stamp, "class.tif", sep=""), format="GTiff", datatype='INT2U',overwrite=TRUE)



# create log of classified image names
write(paste(time.stamp, "class.tif", sep=""), file="classified_image_log.txt", append=TRUE)
print(paste0('Supervised classification and export took',toc()))