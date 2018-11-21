library(raster) # for classification and extraction to road network
library(rgdal) # for geographic transformations and projections
library(magick)
library(RgoogleMaps)
library(httr) #for in-memory download of API image
library(RStoolbox) 
library(png)
library(stringr)
library(gdalUtils)

#rootdir <- 'F:/Levin_Lab/stormwater' #UPDATE
rootdir <- 'C:/Mathis/ICSL/stormwater'
src <- file.path(rootdir, '/src/traffic')
res <- file.path(rootdir, 'results/bing')
setwd(src)
source('coord_conversion.R')
source("map_api_edit.R") # edited function GetBingMaps from package `RGoogleMaps`
setwd(file.path(rootdir,'data'))
BING_KEY <- Sys.getenv("BING_KEY")


#Define function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Define image size
zoom <- 15
Xpx = 2000 # length of the image in pixels (maximum that Bing/Google will output)
Ypx = 1500

#Get a single image in the middle of the ocean
ll <- c(47, -126) #lower left coordinate
bbox <- c(ll, 
          pixelcoords_to_latlong(latlong_to_pixelcoords(ll[1],ll[2],zoom)[2] - Ypx, 
                                 latlong_to_pixelcoords(ll[1],ll[2],zoom)[1] + Xpx,
                                 zoom)) #Create bounding box

map.logob <- GetBingMap2( # aerial image
  mapArea= bbox,
  maptype="CanvasDark", # use aerial for final because it doesn't have labels
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

map.logow <- GetBingMap2( # aerial image
  mapArea= bbox,
  maptype="CanvasLight", # use aerial for final because it doesn't have labels
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

#Try changing them to NA
# map.logob[c(logopix, logopix+length(map.logob)/3, logopix+2*length(map.logob)/3)] <- NA
# writePNG(map.logob, 'logoremove_bedit.png')

######################################################################################################################################
#-------------------------------------------------------------------------------
#Try removing on larger image
#---------------------------------------------------------------------------------
bbox <- c(47.490, -122.420, 47.737, -122.251)
polybound <- F

# calculate optimal number of images to fetch
zoom <- 15
Xpx = 2000 # length of the image in pixels (maximum that Bing/Google will output)
Ypx = 1500
WebMercator <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs") #Define Bing map projection

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
    if (polybound==TRUE) {
      if (all(is.na(over(spTransform(envelope,CRSobj=CRS(proj4string(PSwatershed))), PSwatershed)))) {
        #Make sure that tile falls within boundaries of Puget Sound watershed
        print(paste0('Row ',i,', column ',j,' does not intersect with polygon boundaries'))
      }
    }
    else {
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

map.params <- list(maptype="CanvasDark", # parameters needed to construct URL for API call
                   zoom = zoom,
                   apiKey= BING_KEY,
                   extraURL="&mapLayer=TrafficFlow",
                   verbose=0,
                   size=size,
                   DISK=F,
                   MEMORY=TRUE,
                   labels=FALSE) #Setting labels=FALSE removes all features and labels of the map but roads and traffic. 

time.stamp <- format(Sys.time(), "%y%m%d_%H_%M_") # want all images taken in an instance to have same timestamp

imgs <- c() # holds images
ntiles <- nrow(coords)
for (i in 1:ntiles) {
  print(100*i/ntiles)
  filename <- file.path(res,
                        paste(time.stamp, # time stamp
                              str_pad(coords[i,'row'], nchar(imgs.h), pad = "0"), "_", # pad img number with leading zeros and row number
                              str_pad(coords[i,'col'], nchar(imgs.w), pad = "0"), # pad img number with leading zeros and column number
                              ".tif", sep=""))
  map <- brick(255L*do.call(GetBingMap2, c(list(mapArea=coords[i,c('yll','xll','yur','xur')],destfile=filename), map.params))-1L)
  
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
mosaic <- mosaic_rasters(imgs, file.path(res, paste0(time.stamp, "mosaic.tif")), output_Raster=T, co="COMPRESS=LZW")


######################################################################################################################################
logopix2 <- raster(
  do.call(cbind, replicate(imgs.w, 
                           do.call(rbind, replicate(imgs.h, 
                                                    logopix, simplify=FALSE)), simplify=FALSE)),
)
xmin(logopix2) <- xmin(mosaic)
xmax(logopix2) <- xmax(mosaic)
ymin(logopix2) <- ymin(mosaic)
ymax(logopix2) <- ymax(mosaic)
crs(logopix2) <- WebMercator # Define coordinate system
writeRaster(logopix2, file.path(res, 'boolean_logorm.tif'), format='GTiff', overwrite=T)
#181121_10_10_mosaic.tif
