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

#==================================================================
# Set constants and map parameters
#==================================================================
# These will be called in the traffic_api.R script 

# apiKey = scan("bing_key.txt", what="") # text file with Bing Maps API key
apiKey = "AinLOS3zG8oO80pPTZqNx_Pl4SQvO-JhY6tNCujUOJr0iRrbACjQSuLE3_9ir849"

# projection used by Bing Maps
myProj <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"

# Full extent of area to be covered.
bbox <- c(47.614086, -122.235819, 47.637687, -122.128015)  # Coordinates are lower left and upper right lat/long (in that order)

# calculate optimal number of images to fetch
zoom <- 15
radius = 6378137 # radius of earth used in Bing Maps
px = 640 # number of pixels in image
groundres <- (cos(bbox[1] * pi/180) * 2 * pi * radius) / (256 * 2^zoom) # size of pixel in meters for zoom level at latitude
img.size <- px*groundres # number of pixels * ground res = image height and width in meters
bbox.w <- distRhumb(bbox[c(2,3)], bbox[c(4,3)], r=radius) # ground distance of bounding box (east to west), top left corner to top right
bbox.h <- distRhumb(bbox[c(2,3)], bbox[c(2,1)], r=radius) # ground distance of bounding box (north to south), top left corner to bottom left corner
imgs.w <- ceiling(bbox.w/img.size) # optimal number of images east to west
imgs.h <- ceiling(bbox.h/img.size) # optimal number of images north to south

save(apiKey, myProj, zoom, radius, px, groundres, 
     img.size, bbox.w, bbox.h, imgs.w, imgs.h, 
     file = "mapValues")

#==================================================================
# Schedule tasks
#==================================================================

# want to run the script hourly from 6 AM to 9 PM

taskscheduler_create("GetRasters", rscript = "traffic_api.R", schedule = 'HOURLY', 
                     starttime=format(ceiling_date(Sys.time(), unit="hour"), "%H:%M"))

taskscheduler_create("test", rscript="C:\\Users\\ipdavies\\Documents\\traffic\\traffic\\test.R", 
                     starttime = format(Sys.time() +2, "%H:%M:%S"), schedule='ONCE')
taskscheduler_delete("test")



