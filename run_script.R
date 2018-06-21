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
library(rgdal)
library(sp)
library(raster)
library(tictoc)

setwd('F:/Levin_Lab/stormwater/src/traffic')
source('coord_conversion.R')
datadir <- 'F:/Levin_Lab/stormwater/results/'
#==================================================================
# Set constants and map parameters
#==================================================================
# These will be called in the traffic_api.R script 

# apiKey = scan("bing_key.txt", what="") # text file with Bing Maps API key
apiKey = "AinLOS3zG8oO80pPTZqNx_Pl4SQvO-JhY6tNCujUOJr0iRrbACjQSuLE3_9ir849"

# Full extent of area to be covered
PSwatershed <- readOGR(file.path(datadir, 'PSwtshd_roads_dissolve.shp'))
PSwatershedbbox <- spTransform(PSwatershed, CRSobj=CRS("+proj=longlat +datum=WGS84"))@bbox
polybound <- TRUE

#bbox <- c(47.7,-122.7,47,-122) # Coordinates are lower left and upper right lat/long (in that order)
bbox <- c(PSwatershedbbox[2,1],PSwatershedbbox[1,1],PSwatershedbbox[2,2],PSwatershedbbox[1,2])  # Coordinates are lower left and upper right lat/long (in that order)

# calculate optimal number of images to fetch
zoom <- 15
Xpx = 2000 # length of the image in pixels (maximum that Bing/Google will output)
Ypx = 1500
WebMercator <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs") #Define Bing map projection

#Computer pixel coordinate X and Y for the lower left and upper right corner of every row and column
bbox.list.ll.x <- seq(latlong_to_pixelcoords(bbox[1],bbox[2],zoom)[1],latlong_to_pixelcoords(bbox[3],bbox[4],zoom)[1],Xpx)
bbox.list.ur.x <- bbox.list.ll.x + Xpx
bbox.list.ur.y <- seq(latlong_to_pixelcoords(bbox[3],bbox[4],zoom)[2],latlong_to_pixelcoords(bbox[1],bbox[2],zoom)[2],Ypx)
bbox.list.ll.y <- bbox.list.ur.y + Ypx

imgs.w <- length(bbox.list.ll.x) #Number of columns
imgs.h <- length(bbox.list.ll.y) #Number of rows

coords <- c()
coords_mercator <- c()
for (i in 1:imgs.h){ #loops over rows
  for(j in 1:imgs.w){ # loops over columns
    print(100*(imgs.w*(i-1)+j)/(imgs.w*imgs.h))
    
    #Define extent for calling API
    extent.img.ll <- pixelcoords_to_latlong(bbox.list.ll.y[i], bbox.list.ll.x[j], zoom) #Define coordinates of the upper left edge of the lower left corner pixel for the image 
    extent.img.ur <- pixelcoords_to_latlong(bbox.list.ur.y[i], bbox.list.ur.x[j], zoom) #Define coordinates of the upper left edge of the upper right corner pixel for the image 
    coords.tmp <- c(i,j,extent.img.ll,extent.img.ur)
    
    #Create envelope to overlay with Puget Sound roads and get Web Mercator bounding box coordinates for georeferencing
    envelope <- as(raster::extent(coords.tmp[c(4,6,3,5)]),"SpatialPolygons")
    proj4string(envelope) <- CRS("+proj=longlat +datum=WGS84") 
    if (polybound==TRUE & all(is.na(over(spTransform(envelope,CRSobj=CRS(proj4string(PSwatershed))), PSwatershed)))) { #Make sure that tile falls within boundaries of Puget Sound watershed
      print(paste0('Row ',i,', column ',j,' does not intersect with polygon boundaries'))
    } else {
      coords <- rbind(coords, coords.tmp) #API call vector
      
      envelope <- spTransform(envelope, CRSobj=WebMercator)
      coords_mercator <- rbind(coords_mercator, c(xmin(envelope),xmax(envelope),ymin(envelope),ymax(envelope))) #Georeferencing vector
    }
  }
}
rm(i,j,coords.tmp)
colnames(coords) <- c('row','col','yll','xll','yur','xur')
colnames(coords_mercator) <- c('xmin', 'xmax','ymin','ymax')
imgs.h <- max(coords[,'row'])
imgs.w <- max(coords[,'col'])
size <- c(Xpx, Ypx)

save(apiKey,  WebMercator, PSwatershed, polybound, zoom, size,coords_mercator,imgs.h,imgs.w, file = "mapValues")

#==================================================================
# Schedule tasks
#==================================================================
#source('traffic_api.R')


# Run four instances of the script, each every four hours                     
# task_script <- "F:/Levin_Lab/stormwater/src/traffic/traffic_api.R"
# taskscheduler_create("GetRasters_1", rscript = task_script, 
#                      schedule = 'HOURLY', modifier=4,
#                      startdate= format(Sys.time(),"%m/%d/%Y"),
#                      starttime=format(ceiling_date(Sys.time(), unit="hour"), "%H:%M"),
#                      schtasks_extra=paste0("/ET ",format(ceiling_date(Sys.time(), unit="hour")-1, "%H:%M")," /ED ",
#                                            format(as.Date(Sys.time(), tz='UTC')+ 7, '%m/%d/%Y')))
# tasklist <- taskscheduler_ls()
# taskscheduler_delete("GetRasters_1")


