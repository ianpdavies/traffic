#==================================================================
# Description
#==================================================================

# This script sets up the parameters for the region of interest to be called in traffic_api.R every hour through the windows Task Scheduler

#==================================================================
# Dependencies
#==================================================================
library(png)
library(lubridate)
library(gdalUtils)
library(geosphere)
library(rgdal)
library(sp)
library(raster)
library(tictoc)

rootdir <- 'F:/Levin_Lab/stormwater'
#rootdir <- 'C:/Mathis/ICSL/stormwater'
setwd(file.path(rootdir, 'src/traffic'))
source('coord_conversion.R')
source('map_api_edit.R')
resdir <- file.path(rootdir, 'results')

#Set constants
BING_KEY = Sys.getenv("BING_KEY") #Access API key (see https://cran.r-project.org/web/packages/httr/vignettes/secrets.html)
WebMercator <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs") #Define Bing map projection
zoom <- 15
Xpx <- 2000 # length of the image in pixels (maximum that Bing/Google will output)
Ypx <- 1500

#==================================================================
# Get pixel position of Bing logo in each tile
#==================================================================
#Define function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Get a single image in the middle of the ocean
ll <- c(47, -126) #lower left coordinate
bbox_ocean <- c(ll, 
          pixelcoords_to_latlong(latlong_to_pixelcoords(ll[1],ll[2],zoom)[2] - Ypx, 
                                 latlong_to_pixelcoords(ll[1],ll[2],zoom)[1] + Xpx,
                                 zoom)) #Create bounding box for a single tile

map.logob <- GetBingMap2(
  mapArea= bbox_ocean,
  maptype="CanvasDark", 
  zoom=15,
  apiKey= BING_KEY,
  extraURL="&mapLayer=TrafficFlow",
  destfile="logoremove_b.png",
  verbose=0,
  size=c(2000,1500),
  DISK=T,
  MEMORY=TRUE,
  labels = FALSE
)

map.logow <- GetBingMap2(
  mapArea= bbox_ocean,
  maptype="CanvasLight",
  zoom=15,
  apiKey= BING_KEY,
  extraURL="&mapLayer=TrafficFlow",
  destfile="logoremove_w.png",
  verbose=0,
  size=c(2000,1500),
  DISK=T,
  MEMORY=TRUE,
  labels = FALSE
)

#Identify pixels influenced by the Bing logo
mapw_sum <- apply(map.logow, c(1,2), sum)
logopix <- ((apply(map.logob, c(1,2), sum) != 0) | (mapw_sum != Mode(mapw_sum)))
logopix[logopix == 0] <- NA

#Check the bounding box of the logo in the image and determine extent shift
logoshift <- Ypx - apply(which(logopix, arr.ind=T), 2, min)[1] + 1 #Only in the bottom 23 pixels

#==================================================================
# Set map parameters (these will be called in the traffic_api.R script)
#==================================================================
# Full extent of area to be covered
PSwatershed <- readOGR(file.path(resdir, 'PSwtshd_OSMroads_dissolve.shp'))
PSwatershedbbox <- spTransform(PSwatershed, CRSobj=CRS("+proj=longlat +datum=WGS84"))@bbox

#rect <- c(47,-122.7,47.7,-122) # Coordinates are lower left and upper right lat/long (in that order) test extent
rect <- c(PSwatershedbbox[2,1],PSwatershedbbox[1,1],PSwatershedbbox[2,2],PSwatershedbbox[1,2])  # Coordinates are lower left and upper right lat/long (in that order)

#Create an alternate bounding box by extending it North by the height of the Bing logo bounding box
rect_alt <- c(rect[1], rect[2],
              pixelcoords_to_latlong(latlong_to_pixelcoords(rect[3],rect[4],zoom)[2]-25, 
                                     latlong_to_pixelcoords(rect[3],rect[4],zoom)[1], zoom=15))

#Determine number and bounding box of every tile to get
bbox_tile <- function(bbox, zoom, Xpx, Ypx, poly, polybound) {
  #Compute pixel coordinate X and Y for the lower left and upper right corner of every row and column
  bbox.list.ll.x <- seq(latlong_to_pixelcoords(bbox[1],bbox[2],zoom)[1],
                        latlong_to_pixelcoords(bbox[3],bbox[4],zoom)[1],Xpx)
  bbox.list.ur.x <- bbox.list.ll.x + Xpx
  
  bbox.list.ur.y <- seq(latlong_to_pixelcoords(bbox[3],bbox[4],zoom)[2],
                        latlong_to_pixelcoords(bbox[1],bbox[2],zoom)[2],Ypx)
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
      if (polybound==TRUE & all(is.na(over(spTransform(envelope,CRSobj=CRS(proj4string(poly))), poly)))) {
        #Make sure that tile falls within boundaries of polygon
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
  
  return(list(coords=coords, coords_mercator=coords_mercator, imgs.h=imgs.h, imgs.w=imgs.w, size=size))
}

tiling_main <- bbox_tile(bbox = rect, zoom=zoom, Xpx=Xpx, Ypx=Ypx, poly = PSwatershed, polybound=TRUE)
tiling_alt <- bbox_tile(bbox = rect_alt, zoom=zoom, Xpx=Xpx, Ypx=Ypx, poly = PSwatershed, polybound=TRUE)


logo_bool <- function(tiling_list, logoids, outraster) {
  llen <- nrow(tiling_list$coords_mercator)
  for (i in 1:llen) {
    print(100*i/llen)
    bbox <- tiling_list$coords_mercator[i,]
    r <- raster(logoids)
    xmin(r) <- bbox[1]
    xmax(r) <- bbox[2]
    ymin(r) <- bbox[3] 
    ymax(r) <- bbox[4]
    crs(r) <- WebMercator # Define coordinate system
    writeRaster(r, paste0(outraster, i, '.tif'), format='GTiff', datatype='INT1U', overwrite=T)
  }
  print('Done downloading')
  mosaic_rasters(paste0(outraster, 1:llen, '.tif'), paste0(outraster, '.tif'), output_Raster=F, co="COMPRESS=LZW")
  print('Done mosaicking, now deleting')
  do.call(file.remove, list(paste0(outraster, 1:llen, '.tif')))
}

logo_bool(tiling_main, logopix, file.path(resdir, 'boolean_logo'))
logo_bool(tiling_alt, logopix, file.path(resdir, 'boolean_logoalt'))

save(BING_KEY,  WebMercator, PSwatershed, zoom, tiling_main, tiling_alt, file = "mapValues")
