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
library(rprojroot)
library(tictoc)
library(data.table)

library(bigstatsr)
library(doParallel)
library(foreach)

#==================================================================
# Set constants and map parameters
#==================================================================
rootdir <- find_root(has_dir("src")) #UPDATE IF CHANGED DIRECTORY STRUCTURE
src <- file.path(rootdir, '/src/traffic')
source(file.path(src,"map_api_edit.R")) # edited function GetBingMaps from package `RGoogleMaps`
source(file.path(src,"coord_conversion.R"))
load(file.path(src,"mapValues_AQsites"))
load(file.path(src,"sclasses")) #Load trained classification model
resdir <- file.path(rootdir, 'results/airdata/tiles')
if (!dir.exists(resdir)) {
  dir.create(resdir)
}

#==================================================================
# Download static images
#==================================================================
time.stamp <- format(Sys.time(), "%y%m%d_%H_%M_") # want all images taken in an instance to have same timestamp
logopixpos <- which(!is.na(melt(t(logopars[['logopix']]))$value)) #Get position of Bing logo in rasterbrick format (cell index grows column wise then row wise)
iterate_tiles <- function(tiling_list) {
  map.params <- list(maptype="CanvasDark", # parameters needed to construct URL for API call
                     zoom = zoom,
                     apiKey= BING_KEY,
                     extraURL="&mapLayer=TrafficFlow",
                     verbose=0,
                     size=tiling_list$size,
                     DISK=F,
                     MEMORY=TRUE,
                     labels=FALSE) #Setting labels=FALSE removes all features and labels of the map but roads and traffic. 
  
  tic()
  imgs <- c() # holds images
  ntiles <- nrow(tiling_list$coords_wgs)
  ntiles <- 200
  
  cl <- parallel::makeCluster(bigstatsr::nb_cores()) #make cluster based on recommended number of cores
  doParallel::registerDoParallel(cl)
  imgs <- foreach(i=seq_len(ntiles)) %dopar% {
    #print(100*i/ntiles)
    
    filename <- file.path(resdir,
                          paste(time.stamp, # time stamp
                                stringr::str_pad(tiling_list$coords_wgs[i,'row'], 
                                                 nchar(tiling_list$imgs.h), pad = "0"), "_", # pad img number with leading zeros and row number
                                stringr::str_pad(tiling_list$coords_wgs[i,'col'], 
                                                 nchar(tiling_list$imgs.w), pad = "0"), # pad img number with leading zeros and column number
                                ".tif", sep=""))
    
    map <- raster::brick(255L*do.call(GetBingMap2, c(list(
      mapArea=tiling_list$coords_wgs[i,c('yll','xll','yur','xur')],
      destfile=filename), 
      map.params))-1L)
    
    if (max(map@data@values[-logopixpos,])>-1) { #Only continue if areas outside of logo have data 
      #Remove logo
      #map@data@values[logopixpos,] <- c(-1, -1, -1)
      
      #Define extent in Web Mercator coordinates
      raster::xmin(map) <- tiling_list$coords_mercator[i,'xmin']
      raster::xmax(map) <- tiling_list$coords_mercator[i,'xmax']
      raster::ymin(map) <- tiling_list$coords_mercator[i,'ymin']
      raster::ymax(map) <- tiling_list$coords_mercator[i,'ymax']
      raster::crs(map) <- WebMercator # Define coordinate system
    
      raster::writeRaster(map, filename, format="GTiff",datatype='INT1U', overwrite=TRUE)
      return(filename)
    }
  }
  parallel::stopCluster(cl)    
  toc()
  return(imgs)
  rm(i, map) # remove temp objects
}

#Run differently depending on whether it's an odd or even hour
if (as.numeric(format(Sys.time(), "%H"))%%2 > 0) {
  imgs_list <- iterate_tiles(tiling_main)
} else {
  imgs_list <- iterate_tiles(tiling_alt)
}

#==================================================================
# Mosaic images into one raster
#==================================================================
tic()
mosaic <- mosaic_rasters(imgs_list, file.path(resdir, paste0(time.stamp, "mosaic.tif")), output_Raster=T, co="COMPRESS=LZW")
#Remove tiles
file.remove(imgs_list)
file.remove(paste0(imgs_list,'.rda'))
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
writeRaster(r_class, filename=file.path(resdir,paste(time.stamp, "class_mlc.tif", sep="")), format="GTiff", datatype='INT1U', overwrite=TRUE)
toc()

file.remove(file.path(resdir,paste0(time.stamp, "mosaic.tif")))

# create log of classified image names
write(file.path(resdir, paste(time.stamp, "class.tif", sep="")), file="classified_image_log.txt", append=TRUE)
print('Done classifying')
toc()
