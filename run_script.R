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
library(remotes)
library(plyr)
library(Rcpp)
library(data.table)
library(magrittr)
library(rprojroot)

rootdir <- find_root(has_dir("src"))
if (getwd() != file.path(rootdir, 'src/traffic')) {
  setwd(file.path(rootdir, 'src/traffic'))}
source('coord_conversion.R')
source('map_api_edit.R')
sourceCpp("getbboxes_spatDataManagement.cpp")
resdir <- file.path(rootdir, 'results/airdata')
#resdir <- file.path(rootdir, 'results')


#Set constants
BING_KEY = Sys.getenv("BING_KEY") #Access API key(s) (see https://cran.r-project.org/web/packages/httr/vignettes/secrets.html)
BING_KEY2 = Sys.getenv("BING_KEY2")
BING_KEY3 = Sys.getenv("BING_KEY3")
WebMercator <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs") #Define Bing map projection
zoom <- 15
Xpx <- 2000 # length of the image in pixels (maximum that Bing/Google will output)
Ypx <- 1500

#==================================================================
# Get pixel position of Bing logo in each tile
#==================================================================
#Define function
Mode <- function(x) {
  "Purpose: Find values that occurs the most in vector"
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

getlogo <- function(Xpx, Ypx, zoom, BING_KEY) {
  "Purpose: identify which pixels the Bing logo occupies in each tile"
  
  #Get a single image in the middle of the ocean
  ll <- c(47, -126) #lower left coordinate
  bbox_ocean <- c(ll, 
                  unlist(pixelcoords_to_latlong(
                    latlong_to_pixelcoords(ll[1],ll[2],zoom)[['Y']] - Ypx, 
                    latlong_to_pixelcoords(ll[1],ll[2],zoom)[['X']] + Xpx,
                    zoom))) #Create bounding box for a single tile
  
  map.logob <- GetBingMap2(
    mapArea= bbox_ocean,
    maptype="CanvasDark", 
    zoom=zoom,
    apiKey= BING_KEY,
    extraURL="&mapLayer=TrafficFlow",
    destfile="logoremove_b.png",
    verbose=0,
    size=c(Xpx,Ypx),
    DISK=T,
    MEMORY=TRUE,
    labels = FALSE
  )
  
  map.logow <- GetBingMap2(
    mapArea= bbox_ocean,
    maptype="CanvasLight",
    zoom=zoom,
    apiKey= BING_KEY,
    extraURL="&mapLayer=TrafficFlow",
    destfile="logoremove_w.png",
    verbose=0,
    size=c(Xpx,Ypx),
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
  return(list(logopix=logopix, logoshift=logoshift))
}

#==================================================================
# Set map parameters (these will be called in the traffic_api.R script)
#==================================================================
#Determine number and bounding box of every tile to get
bbox_tile <- function(bbox, zoom, Xpx, Ypx, poly, polywgs, polybound) {
  #Compute pixel coordinate X and Y for the lower left and upper right corner of every row and column 
  #within the bounding box of the entire shapefile.
  #(keep in mind that in pixel coordinates X increase west to east and Y increased north to south,so 
  # ur.x > ll.x but ur.y < ll.y)
  bbox.list.ll.x <- seq(latlong_to_pixelcoords(bbox[1],bbox[2],zoom)[['X']],
                        latlong_to_pixelcoords(bbox[3],bbox[4],zoom)[['X']],Xpx)
  bbox.list.ur.x <- bbox.list.ll.x + Xpx
  
  bbox.list.ur.y <- seq(latlong_to_pixelcoords(bbox[3],bbox[4],zoom)[['Y']],
                        latlong_to_pixelcoords(bbox[1],bbox[2],zoom)[['Y']],Ypx)
  bbox.list.ll.y <- bbox.list.ur.y + Ypx
  
  imgs.w <- length(bbox.list.ll.x) #Number of columns
  imgs.h <- length(bbox.list.ll.y) #Number of rows
  
  #---- Create smallest clusters of tiles that enclose each shape's bounding box ----
  #Get bounding box of each shape
  sitebox <- data.table(GetBBoxes(polywgs), ID=seq_len(nrow(polywgs))) %>% 
    #Convert it them to pixel coordinates
    .[, (c('llx','lly')) := latlong_to_pixelcoords(minY, minX, zoom)] %>%
    .[, (c('urx','ury')) := latlong_to_pixelcoords(maxY, maxX, zoom)] %>%
    .[, c('llx.site', 'lly.site', 'urx.site', 'ury.site') := .(llx,lly, urx, ury)]
  
  #Get bounding box of the smallest tile set that encloses each shape's bounding box
  setkey(sitebox, llx)
  llx <- data.table(llx = bbox.list.ll.x, llx.tile= bbox.list.ll.x, 
                    jmin = seq_along(bbox.list.ll.x), key='llx')[sitebox, roll=Inf][,.(ID,jmin)]
  setkey(sitebox, urx)
  urx <- data.table(urx= bbox.list.ur.x, urx.tile =bbox.list.ur.x, 
                    jmax = seq_along(bbox.list.ur.x), key='urx')[sitebox, roll=-Inf][,.(ID,jmax)]
  setkey(sitebox, lly)
  lly <- data.table(lly= bbox.list.ll.y, lly.tile=bbox.list.ll.y, 
                    imin = seq_along(bbox.list.ll.y), key='lly')[sitebox, roll=-Inf][,.(ID,imin)]
  setkey(sitebox, ury)
  ury <- data.table(ury= bbox.list.ur.y, ury.tile=bbox.list.ur.y, 
                    imax = seq_along(bbox.list.ur.y), key='ury')[sitebox, roll=Inf][,.(ID,imax)]
  tilesets <- unique(llx[urx, on='ID'][lly, on='ID'][ury, on='ID'])
  
  #---- Get unique tiles contained in tile clusters ----
  #Get row and col of all unique tiles that are included within these tile sets 
  alltiles <- tilesets[, expand.grid(j=seq(jmin, jmax), i=seq(imin,imax)), by=seq_len(nrow(tilesets))][
    ,.(i,j)] %>%
    unique
  
  #Get extent coordinates of every tile
  alltiles[, (c('lly','llx','ury','urx')) := 
             c(pixelcoords_to_latlong(bbox.list.ll.y[i], bbox.list.ll.x[j], zoom),
               pixelcoords_to_latlong(bbox.list.ur.y[i], bbox.list.ur.x[j], zoom))]
  
  #---- For each tile, convert coordinates, and extract only if overlaps a shape  ----
  coordset <- apply(alltiles, 1, function(tile) {
    envelope <- as(raster::extent(tile[c('llx', 'urx', 'lly', 'ury')]),"SpatialPolygons")
    sp::proj4string(envelope) <- sp::CRS("+proj=longlat +datum=WGS84") 
    if (polybound==TRUE) {
      if (any(!is.na(over(
        spTransform(envelope, CRSobj=sp::CRS(sp::proj4string(poly))), 
        poly))))  {  
        envelope <- spTransform(envelope, CRSobj=WebMercator)
        list(coords_wgs = as.vector(tile),
             coords_mercator = c(raster::xmin(envelope), raster::xmax(envelope),
                                 raster::ymin(envelope),raster::ymax(envelope)))
      }
    }
  })
  
  #---- Format output  ----
  coords_mercator <- as.data.frame(do.call('rbind',
                                           plyr::compact(lapply(coordset, `[[`, 'coords_mercator'))))
  coords_wgs <- as.data.frame(do.call('rbind',
                                      plyr::compact(lapply(coordset, `[[`, 'coords_wgs'))))
  
  
  names(coords_wgs) <- c('row','col','yll','xll','yur','xur')
  names(coords_mercator) <- c('xmin', 'xmax','ymin','ymax')
  imgs.h <- max(coords_wgs[,'row'])
  imgs.w <- max(coords_wgs[,'col'])
  size <- c(Xpx, Ypx)
  
  return(list(coords_wgs=coords_wgs, coords_mercator=coords_mercator, 
              imgs.h=imgs.h, imgs.w=imgs.w, size=size))
}

#Get shape's bounding box then run main tiling and shifted tiling 
setparams <- function(shp, Xpx, Ypx, zoom, BING_KEY, logopars) {
  shptrans <- spTransform(shp, CRSobj=CRS("+proj=longlat +datum=WGS84"))
  shpbbox <- shptrans@bbox
  
  "For testing function"
  #rect <- c(47,-122.7,47.7,-122) # Coordinates are lower left and upper right lat/long (in that order) test extent
  rect <- c(shpbbox[2,1], shpbbox[1,1], shpbbox[2,2], shpbbox[1,2])  # Coordinates are lower left and upper right lat/long (in that order)
  
  #Create an alternate bounding box by extending it North by the height of the Bing logo bounding box
  rect_alt <- c(rect[1], rect[2],
                unlist(pixelcoords_to_latlong(
                  latlong_to_pixelcoords(rect[3],rect[4],zoom)[['Y']] - logopars[['logoshift']], 
                  latlong_to_pixelcoords(rect[3],rect[4],zoom)[['X']],
                  zoom)))
  
  #Determine number and bounding box of every tile to get
  print('Getting main tiling')
  tiling_main <- bbox_tile(bbox = rect, zoom=zoom, Xpx=Xpx, Ypx=Ypx,
                           poly = shp, polywgs = shptrans, polybound=TRUE)
  print('Getting shifted tiling')
  tiling_alt <- bbox_tile(bbox = rect_alt, zoom=zoom, Xpx=Xpx, Ypx=Ypx, 
                          poly = shp,polywgs = shptrans, polybound=TRUE)
  
  return(list(tiling_main=tiling_main, tiling_alt=tiling_alt))
}

#Create mask for logos across all tiles
logo_bool <- function(tiling_list, logoids, outraster) {
  llen <- nrow(tiling_list$coords_mercator)
  lapply(1:llen, function(i) {
    print(100*i/llen)
    bbox <- tiling_list$coords_mercator[i,]
    r <- raster(logoids)
    xmin(r) <- bbox[,1]
    xmax(r) <- bbox[,2]
    ymin(r) <- bbox[,3] 
    ymax(r) <- bbox[,4]
    crs(r) <- WebMercator # Define coordinate system
    writeRaster(r, paste0(outraster, i, '.tif'), format='GTiff', datatype='INT1U', overwrite=T)
  })
  print('Done downloading')
  mosaic_rasters(paste0(outraster, 1:llen, '.tif'), paste0(outraster, '.tif'), output_Raster=F, co="COMPRESS=LZW")
  print('Done mosaicking, now deleting')
  do.call(file.remove, list(paste0(outraster, 1:llen, '.tif')))
}

#==================================================================
# Run functions
#==================================================================
#Get logo mask characteristics
logopars <- getlogo(Xpx, Ypx, zoom, BING_KEY)

# Full extent of area to be covered
"For processing PS watershed"
# PSwatershed <- readOGR(file.path(resdir, 'PSwtshd_OSMroads_dissolve.shp'))
# PStiles <- setparams(PSwatershed, Xpx, Ypx, zoom, BING_KEY, logopars)
# save(BING_KEY,  WebMercator, PSwatershed, zoom,
#      PStiles[['tiling_main']], PStiles[['tiling_alt']],
#      file = "mapValues")

"For processing US AQ monitoring stations"
AQsites <- readOGR(file.path(resdir, 'airsites_550bufunion.shp'))
AQtiles <- setparams(shp=AQsites, Xpx, Ypx, zoom, BING_KEY, logopars)
tiling_main <- AQtiles$tiling_main
tiling_alt <-  AQtiles$tiling_alt
save(BING_KEY, BING_KEY2, BING_KEY3, WebMercator, AQsites,  zoom, tiling_main, tiling_alt, logopars,
     file = "mapValues_AQsites")
