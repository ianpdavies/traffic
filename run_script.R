#==================================================================
# Description
#==================================================================

# This script sets up the task scheduler and parameters for the region of interest, then calls the traffic_api.R script to get hourly static images
# After all the images are downloaded and classified, you can use the raster_math.R script to summarize them into single rasters for analysis.

#==================================================================
# Dependencies
#==================================================================

library(taskscheduleR)
library(lubridate)
library(geosphere)
library(tictoc)

source('coord_conversion.R')
setwd('F:/Levin_Lab/stormwater/src/traffic')
datadir <- 'F:/Levin_Lab/stormwater/results/'
#==================================================================
# Set constants and map parameters
#==================================================================
# These will be called in the traffic_api.R script 

# apiKey = scan("bing_key.txt", what="") # text file with Bing Maps API key
apiKey = "AinLOS3zG8oO80pPTZqNx_Pl4SQvO-JhY6tNCujUOJr0iRrbACjQSuLE3_9ir849"

# Full extent of area to be covered
PSwatershed <- readOGR(file.path(datadir, 'PSwtshd_dissolve.shp'))
PSwatershedbbox <- spTransform(PSwatershed, CRSobj=CRS("+proj=longlat +datum=WGS84"))@bbox
polybound <- TRUE

bbox <- c(46.7, -123, 47.2, -122.5)  # Coordinates are lower left and upper right lat/long (in that order)
#bbox <- c(PSwatershedbbox[2,1],PSwatershedbbox[1,1],PSwatershedbbox[2,2],PSwatershedbbox[1,2])  # Coordinates are lower left and upper right lat/long (in that order)

# calculate optimal number of images to fetch
zoom <- 15
px = 1500 # length of the image in pixels (maximum that Bing/Google will output)

#Computer pixel coordinate X and Y for the lower left and upper right corner of every row and column
bbox.list.ll.x <- seq(latlong_to_pixelcoords(bbox[1],bbox[2],zoom)[1],latlong_to_pixelcoords(bbox[3],bbox[4],zoom)[1],px)
bbox.list.ur.x <- bbox.list.ll.x + (px-1)
bbox.list.ur.y <- seq(latlong_to_pixelcoords(bbox[3],bbox[4],zoom)[2],latlong_to_pixelcoords(bbox[1],bbox[2],zoom)[2],px)
bbox.list.ll.y <- bbox.list.ur.y + (px-1)

imgs.w <- length(bbox.list.ll.x) #Number of columns
imgs.h <- length(bbox.list.ll.y) #Number of rows

save(apiKey, PSwatershed, polybound, zoom, px, 
     bbox.list.ll.x, bbox.list.ll.y, bbox.list.ur.x, bbox.list.ur.y,
     imgs.w, imgs.h, file = "mapValues")

#==================================================================
# Schedule tasks
#==================================================================
source("traffic_api.R")

#For 840 tiles
#With 500 pixel images, without selective tiling: download 296 sec, mosaic 18 min, classification and export 30 min
#with 1500 pixel inmages, without selective tiling: download 64 sec, mosaic 5 min, classification and export 30 min



# Run script hourly
# taskscheduler_create("GetRasters", rscript="F:/Levin_Lab/stormwater/src/traffic/traffic_api.R",
#                      starttime = format(Sys.time() +55, "%H:%M:%S"), schedule='ONCE')
# taskscheduler_delete("GetRasters")
                     
taskscheduler_create("GetRasters", rscript = "traffic_api.R", schedule = 'HOURLY',
                     starttime=format(ceiling_date(Sys.time(), unit="hour"), "%H:%M"))


