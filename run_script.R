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
#==================================================================
# Set constants and map parameters
#==================================================================
# These will be called in the traffic_api.R script 

# apiKey = scan("bing_key.txt", what="") # text file with Bing Maps API key
apiKey = "AinLOS3zG8oO80pPTZqNx_Pl4SQvO-JhY6tNCujUOJr0iRrbACjQSuLE3_9ir849"

# Full extent of area to be covered
bbox <- c(47.5, -122.5, 48, -122)  # Coordinates are lower left and upper right lat/long (in that order)
#bbox <- c(47.614086, -122.235819, 47.637687, -122.128015)  # Coordinates are lower left and upper right lat/long (in that order)

# calculate optimal number of images to fetch
zoom <- 15
px = 500 # length of the image in pixels (maximum that Bing/Google will output)

#Computer pixel coordinate X and Y for the lower left and upper right corner of every row and column
bbox.list.ll.x <- seq(latlong_to_pixelcoords(bbox[1],bbox[2],zoom)[1],latlong_to_pixelcoords(bbox[3],bbox[4],zoom)[1],px)
bbox.list.ur.x <- bbox.list.ll.x + (px-1)
bbox.list.ur.y <- seq(latlong_to_pixelcoords(bbox[3],bbox[4],zoom)[2],latlong_to_pixelcoords(bbox[1],bbox[2],zoom)[2],px)
bbox.list.ll.y <- bbox.list.ur.y + (px-1)

imgs.w <- length(bbox.list.ll.x) #Number of columns
imgs.h <- length(bbox.list.ll.y) #Number of rows

save(apiKey, zoom, px, 
     bbox.list.ll.x, bbox.list.ll.x, bbox.list.ll.x, bbox.list.ll.x,
     imgs.w, imgs.h, file = "mapValues")

#==================================================================
# Schedule tasks
#==================================================================
tic("Download, mosaic, and georeference")
source("traffic_api.R") 
toc()



# want to run the script hourly from 4 AM to 9 PM
# taskscheduler_create("GetRasters", rscript = "traffic_api.R", schedule = 'HOURLY', 
#                      starttime=format(ceiling_date(Sys.time(), unit="hour"), "%H:%M"))
# 
# taskscheduler_create("GetRasters", rscript="traffic_api.R", 
#                      starttime = format(Sys.time() +2, "%H:%M:%S"), schedule='ONCE')
# 
# 
# taskscheduler_delete("test")



