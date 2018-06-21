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
library(tictoc)

#==================================================================
# Set constants and map parameters
#==================================================================
src <- 'F:/Levin_Lab/stormwater/src/traffic'
source(file.path(src,"map_api_edit.R")) # edited function GetBingMaps from package `RGoogleMaps`
source(file.path(src,"coord_conversion.R"))
load(file.path(src,"mapValues"))
load(file.path(src,"sclasses")) #Load trained classification model
res <- 'F:/Levin_Lab/stormwater/results/bingtest'

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
ntiles <- nrow(coords)
for (i in 1:ntiles) {
  print(i/ntiles)
  filename <- file.path(res,
                        paste(time.stamp, # time stamp
                              str_pad(coords[i,'row'], nchar(imgs.h), pad = "0"), "_", # pad img number with leading zeros and row number
                              str_pad(coords[i,'col'], nchar(imgs.w), pad = "0"), # pad img number with leading zeros and column number
                              ".tif", sep=""))
  map <- 255L*brick(do.call(GetBingMap2, c(list(mapArea=coords[i,c('yll','xll','yur','xur')],destfile=filename), map.params)))-1L # download map based on lower left and upper right coordinates

  #Define extent in Web Mercator coordinates
  xmin(map) <- coords_mercator[i,'xmin'] 
  xmax(map) <- coords_mercator[i,'xmax'] 
  ymin(map) <- coords_mercator[i,'ymin'] 
  ymax(map) <- coords_mercator[i,'ymax'] 
  crs(map) <- WebMercator # Define coordinate system
  
  
  
  writeRaster(map, filename, format="GTiff",datatype='INT1U', overwrite=TRUE)
  imgs <- c(imgs, filename) # list of filenames
}
rm(i, map) # remove temp objects
print('Done downloading tiles')
toc()

#==================================================================
# Mosaic images into one raster
#==================================================================
tic()
mosaic <- mosaic_rasters(imgs, file.path(res, paste0(time.stamp, "mosaic.tif")), output_Raster=T, force_ot="Byte")
#Remove tiles
file.remove(imgs)
file.remove(paste0(imgs,'.rda'))
print('Done mosaicking')
toc()

#file.rename(paste0(time.stamp, "mosaic.tif"), "'F:/Levin_Lab/stormwater/src/traffic/data/traffic_classification_trainingimg.tif") 

#=================================================
# Supervised classification of traffic conditions
#=================================================
Sys.time()
tic()
names(mosaic) = c("band1","band2","band3") # give image bands the same names as those used in sclass
r_class <- predict(mosaic, sclass_mlc$model) # classify using model generated from training points
# Reclassify
#reclas <- matrix(c(1,2,3,4,5,6,7,8,NA,NA,NA,NA,2,3,NA,1), ncol=2)
#r_reclassified <- reclassify(r_class, reclas) 
# save as compressed geotiff
writeRaster(r_class, filename=file.path(res,paste(time.stamp, "class_mlc.tif", sep="")), format="GTiff", datatype='INT1U',overwrite=TRUE)
file.remove(file.path(res,paste0(time.stamp, "mosaic.tif")))

# create log of classified image names
write(file.path(res, paste(time.stamp, "class.tif", sep="")), file="classified_image_log.txt", append=TRUE)
print('Done classifying')
toc()
