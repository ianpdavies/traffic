#==================================================================
# Description
#==================================================================

# This script collates, mosaicks, and classifies Bing traffic tiles for downloads that did not complete because of API issues.

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
rootdir <- 'F:/Levin_Lab/stormwater'
src <- file.path(rootdir, '/src/traffic')
source(file.path(src,"map_api_edit.R")) # edited function GetBingMaps from package `RGoogleMaps`
source(file.path(src,"coord_conversion.R"))
load(file.path(src,"mapValues"))
load(file.path(src,"sclasses")) #Load trained classification model
resdir <- file.path(rootdir, 'results/bing')

#==================================================================
# Collate incomplete tiles by hour
#==================================================================
allfiles <- list.files(resdir)
restiles <- allfiles[grepl("^[0-9_]+\\.tif$", allfiles)]

write('Missing tiles, number of tiles', file.path(resdir, 'incomplete_tiles.txt'))
for (t in unique(substr(restiles, 1, 12))) {
  print(paste0('Processing: ',t))
  imgs_list <- grep(t, restiles, value=T)
  #write(paste0(t, ', ', length(imgs_list)), file=file.path(resdir, 'incomplete_tiles.txt'),append=TRUE)
  
  #Mosaick them
  #mosaic <- mosaic_rasters(file.path(resdir, imgs_list), file.path(resdir, paste0(t, "_mosaic.tif")), output_Raster=T, co="COMPRESS=LZW")
  file.remove(file.path(resdir, imgs_list))
  print('Done mosaicking')
  
  #Classify them
  names(mosaic) = c("band1","band2","band3") # give image bands the same names as those used in sclass
  #r_class <- predict(mosaic, sclass_mlc$model) # classify using model generated from training points
  # save as compressed geotiff
  #writeRaster(r_class, filename=file.path(resdir,paste(t, "_class_mlc.tif", sep="")), format="GTiff", datatype='INT1U', overwrite=TRUE)
  file.remove(file.path(resdir,paste0(t, "_mosaic.tif")))
  print('Done classifying')
  gc()
}