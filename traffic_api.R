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
#If can't load packages, make sure that .libPaths() are correct + do not point to R project/delete libs in R project directory

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
iterate_tiles <- function(tiling_list, zoom, BING_KEY, bingcrs, GetBingMap2, logopix, mlc, outdir) {
  time.stamp <- format(Sys.time(), "%y%m%d_%H_%M_") # want all images taken in an instance to have same timestamp
  map.params <- list(maptype="CanvasDark", # parameters needed to construct URL for API call
                     zoom = zoom,
                     apiKey= BING_KEY,
                     extraURL="&mapLayer=TrafficFlow",
                     verbose=0,
                     size=tiling_list$size,
                     DISK=F,
                     MEMORY=TRUE,
                     labels=FALSE) #Setting labels=FALSE removes all features and labels of the map but roads and traffic. 
  
  reclas <- matrix(c(1,2,3,4,5,6,7,8,NA,1,NA,NA,3,4,NA,2), ncol=2) #reclassification matrix
  imgs <- c() # holds images
  ntiles <- nrow(tiling_list$coords_wgs)
  #ntiles <- 100
  
  cl <- parallel::makeCluster(bigstatsr::nb_cores()) #make cluster based on recommended number of cores
  on.exit(stopCluster(cl))
  doParallel::registerDoParallel(cl)
  
  imgs <- foreach(i=seq_len(ntiles)) %dopar% {
    #print(100*i/ntiles)
    
    filename <- file.path(outdir,
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
    
    if (max(map@data@values[-logopix,])>-1) { #Only continue if areas outside of logo have data 
      #Remove logo
      map@data@values[logopix,] <- c(-1, -1, -1)
      
      #Define extent in Web Mercator coordinates
      raster::xmin(map) <- tiling_list$coords_mercator[i,'xmin']
      raster::xmax(map) <- tiling_list$coords_mercator[i,'xmax']
      raster::ymin(map) <- tiling_list$coords_mercator[i,'ymin']
      raster::ymax(map) <- tiling_list$coords_mercator[i,'ymax']
      raster::crs(map) <- bingcrs # Define coordinate system
      
      # classify image
      # using model generated from training points then reclassify to green - 1, yellow - 2, orange - 3, red - 4
      names(map) = c("band1","band2","band3") # give image bands the same names as those used in sclass
      map@data@values[map@data@values == c(-1,-1,-1)] <- NA
      r_class <- raster::reclassify(raster::predict(map, mlc$model), reclas) 
      
      #Write it out
      raster::writeRaster(r_class, filename, format="GTiff",datatype='INT1U', overwrite=TRUE)
      return(filename)
    }
  }
  return(imgs)
  rm(i, map) # remove temp objects
}

#Run differently depending on whether it's an odd or even hour
logopixpos <- which(!is.na(melt(t(logopars[['logopix']]))$value)) #Get position of Bing logo in rasterbrick format (cell index grows column wise then row wise)

tic()
if (as.numeric(format(Sys.time(), "%H"))%%2 > 0) {
  imgs_list <- iterate_tiles(tiling_list=tiling_main, zoom=zoom, BING_KEY=BING_KEY, bingcrs=WebMercator, 
                             mlc=sclass_mlc,
                             GetBingMap2=GetBingMap2, logopix = logopixpos, outdir=resdir)
} else {
  imgs_list <- iterate_tiles(tiling_list=tiling_alt, zoom=zoom, BING_KEY=BING_KEY, bingcrs=WebMercator,
                             mlc=sclass_mlc,
                             GetBingMap2=GetBingMap2, logopix = logopixpos, outdir=resdir)
}
toc()

#==================================================================
# Mosaic images into one raster - not performed in R anymore
#==================================================================
# tic()
# imgs_vec <- unlist(plyr::compact(imgs_list))
# mosaic <- mosaic_rasters(imgs_vec, 
#                          file.path(resdir, 
#                                    paste0(substr(basename(imgs_vec[1]), 1, 12), "mosaic.tif")), 
#                          output_Raster=T, co="COMPRESS=LZW") #,datatype='INT1U'
# #Remove tiles
# file.remove(imgs_vec)
# file.remove(paste0(imgs_vec,'.rda'))
# print('Done mosaicking')
# toc()

#file.rename(paste0(time.stamp, "mosaic.tif"), "'F:/Levin_Lab/stormwater/src/traffic/data/traffic_classification_trainingimg.tif") 