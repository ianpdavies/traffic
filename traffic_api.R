
library(RgoogleMaps) # for accessing Bing Maps API
library(stringr) # if using str_pad for file naming
library(geosphere) # for spherical geometry
library(raster) # for mosaicking 
library(rgdal)
library(magick) # for mosaicking
library(RStoolbox)

#==================================================================
# notes
#==================================================================



#==================================================================
# Downloading static maps 
#==================================================================

setwd("C:/Users/ipdavies/Documents/traffic/bing")
source("map_api_edit.R")

apiKey = scan("bing_key.txt",what="") # text file with Bing Maps API key
setwd(paste(getwd(), "/images", sep=""))

# prior full extent from user
bbox <- c(47.614086, -122.235819, 47.637687, -122.128015) # lower left , upper right lat/long
# extents = matrix(bbox[c(3,2)]) # getting all 4 corners, for georectifying later 

# calculate optimal number of images
zoom <- 15
radius = 6378137 # radius of earth used in Bing Maps
px = 640 # number of pixels in image
groundres <- (cos(bbox[1] * pi/180) * 2 * pi * radius) / (256 * 2^zoom) # size of pixel in meters for zoom level at latitude
img.size <- px*groundres # number of pixels times ground res = image height/width in meters
bbox.w <- distRhumb(bbox[c(2,3)], bbox[c(4,3)], r=radius) # ground distance of bounding box (east to west), top left corner to top right
bbox.h <- distRhumb(bbox[c(2,3)], bbox[c(2,1)], r=radius) # ground distance of bounding box (north to south), top left corner to bottom left corner
imgs.w <- ceiling(bbox.w/img.size) # optimal number of images east to west
imgs.h <- ceiling(bbox.h/img.size) # optimal number of images north to south

# loop through images
map.params <- list(maptype="Aerial", # use aerial because it doesn't have labels
                   zoom = zoom,
                   apiKey=apiKey,
                   extraURL="&mapLayer=TrafficFlow",
                   verbose=1)

# loops through and retrieves spetatic images
time.stamp <- format(Sys.time(), "%a%d%b%y_%H_%M_") # want all images taken in a set to have same timestamp
extent.img1 <- c(destPointRhumb(c(bbox[2],bbox[3]), 180, d=img.size, r=radius)[2:1], # first img starts near the northwestern corner (but need to do math to get the lower left and upper right coordinates of that img, which may be smaller than coords of initial bounding box)
                destPointRhumb(c(bbox[2],bbox[3]), 90, d=img.size, r=radius)[2:1]) # upper right corner of first img
  
if(!exists("temp")) { # if temp file already exists, don't retrieve duplicate static images. When we parameterize the function, remove this
  z <- 0 # what is this for? might not need it anymore
  imgs <- list() # holds images
  coords <- NULL # holds coordinates ... might not need it later
  for(i in 1:imgs.h){ # loops over rows
    extent.img <- extent.img1 # reset back to first image and navigate down to the ith row
    di <- ifelse(i < 2, 0, img.size*2) # only starts moving after the first image
    extent.img <- c(destPointRhumb(c(extent.img[2],extent.img[1]), 180, d=di, r=radius)[2:1], # move south one image to new row
                    destPointRhumb(c(extent.img[4],extent.img[3]), 180, d=di, r=radius)[2:1])
    for(j in 1:imgs.w){ # loops over columns
      dj <- ifelse(j < 2, 0, img.size*2)
      extent.img <- c(destPointRhumb(c(extent.img[2],extent.img[1]), 90, d=dj, r=radius)[2:1], # move east one image to new column
                      destPointRhumb(c(extent.img[4],extent.img[3]), 90, d=dj, r=radius)[2:1])
      filename <- paste(time.stamp, # time stamp
                        str_pad(j, nchar(imgs.h), pad = "0"), "_", # pad img number with leading zeros, row number
                        str_pad(i, nchar(imgs.w), pad = "0"), # pad img number with leading zeros, column number
                        ".png", sep="")
      map <- do.call(GetBingMap2, c(list(mapArea=extent.img, 
                                         destfile=filename), map.params))
      imgs <- c(imgs, filename) # list of filenames
      coords.tmp <- c(XY2LatLon(map, -px, px, zoom), # lat/long of corners
                      XY2LatLon(map, -px, -px, zoom), 
                      XY2LatLon(map, px, -px, zoom),
                      XY2LatLon(map, px, px, zoom))
      coords <- rbind(coords, coords.tmp) # upper left, lower left, lower right, upper right
    }
  }
  rm(i,j,z,coords.tmp, map, extent.img1) # remove temp objects
  
  save(imgs, coords, file="temp") # save for testing, not needed for final function
} else {
  
  load("temp")
}


#==================================================================
# mosaic images into one raster
#==================================================================

q<-list(image_read(paste(getwd(), "/", imgs, sep=""))) # get all images from files

p<-list() # empty list to hold appended rows of images
for(i in 1:imgs.h){ # for each row of images, create an element in list p of east-west appended images
  p[[i]] <- image_append(q[[1]][(imgs.w*(i-1)+1):(imgs.w*i)])
}

w <- p[[1]] # create mosaicked image starting with first row 
if(imgs.h > 1){ # if there's more than one row of images, append the other rows
  for(j in 2:imgs.h){
    w <- image_append(c(w, p[[j]]), stack=TRUE)
  }
}
rm(i,j)

# save mosaic raster
image_write(w, path="test_append.png", format="png")

#==================================================================
# georeference the raster
#==================================================================

# get raster
r<-brick("test_append.png")

# add coordinate system
crs(r) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"

# georeference corners
xmax(r) <- coords[imgs.w, 8] # max lon
xmin(r) <- coords[1,2] # min lon
ymax(r) <- coords[1,1] # max lat
ymin(r) <- coords[(((imgs.h-1) * imgs.w) + 1), 3] # min lat

#==================================================================
# Image processing
#==================================================================

#===================================
# get test images

# smaller image to test, 1
map.test=GetBingMap2( # aerial image
    mapArea=c(47.591640, -122.324618, 47.598470, -122.315305),
    maptype="CanvasDark", # use aerial for final because it doesn't have labels
    # zoom=15,
    apiKey=apiKey,
    extraURL="&mapLayer=TrafficFlow",
    destfile="test.png",
    verbose=1,
    labels=FALSE
  )

image_write(image_read("test.png"), path="test.png", format="png") # not sure why we have to do this. otherwise, just using brick("test.png") results in 1 band rasterbrick

# smaller image to test, 2
map.test=GetBingMap2( # aerial image
  mapArea=c(47.556256, -122.296838, 47.565523, -122.281560),
  maptype="CanvasDark", # use aerial for final because it doesn't have labels
  # zoom=15,
  apiKey=apiKey,
  extraURL="&mapLayer=TrafficFlow",
  destfile="test2.png",
  verbose=1,
  labels=FALSE
)

image_write(image_read("test2.png"), path="test2.png", format="png") # not sure why we have to do this. otherwise, just using brick("test.png") results in 1 band rasterbrick

# smaller image to test, 3, but this time at the minimum map tile size

z4 <- c(-122.324618, 47.598470) # top left corner
z1<-destPointRhumb(z4, b=90, r=radius, d=img.size) # find distance to top right corner using the size of map tile at zoom 15, 640 px
z2 <- destPointRhumb(z1, b=180, r=radius, d=img.size)# bottom right corner
z3 <- destPointRhumb(z2, b=-90, r=radius, d=img.size)# bottom left corner


map.test=GetBingMap2( 
  mapArea=c(z3[2], z3[1], z1[2], z1[1]),
  maptype="CanvasDark", 
  # zoom=15,
  apiKey=apiKey,
  extraURL="&mapLayer=TrafficFlow",
  destfile="test3.png",
  verbose=1,
  labels=FALSE
)

image_write(image_read("test3.png"), path="test3.png", format="png") 

# read png as rasterbricks
r<-brick("test.png")
r2<-brick("test2.png")
r3<-brick("test3.png")
crs(r) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
crs(r2) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
crs(r3) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"

save(r,r2, r3, file="test_imgs")
load("test_imgs")

#===================================
# unsupervised classification

# using RStoolbox unsupervised classification (k-means)
library(RStoolbox)
set.seed(6)
p<-unsuperClass(r,nClasses=7)
layout(c(1,2))
plotRGB(r)
plot(p$map)
set.seed(0)
p2<-unsuperClass(r2)

# applying k-means model from one raster to another
library(FNN)

p3<-get.knnx(p$model$centers, getValues(r2), 1)$nn.index[,1]

m <- rbind(c(1, 2), c(3, 4))
layout(m)
plotRGB(r)
plotRGB(r2)
plot(p$map)
plot(raster(matrix(p3, nrow=640, byrow=TRUE)))

# This is still not distinguishing orange from yellow!

#===================================
# supervised classification

# Idea: use a supervised classification, then apply the classification model to other images


# # get training points and values
# pts<-click(r,n=21,id=TRUE,xy=TRUE,cell=TRUE)
# pts$class=c("G","Y","O","R","B","A","W") # add text labels

# pts2<-click(r,n=21,id=TRUE,xy=TRUE,cell=TRUE)
# pts2$class=c("G","G","G",
#              "Y","Y","Y",
#              "O","O","O",
#              "R","R","R",
#              "B","B","B",
#              "A","A","A",
#              "W","W","W") # add text labels

# load("training_pts")
load("training_pts2")

pts <- pts2
# create training data
train <- pts[c("x","y")] # extract lat/long of training points
vals <- data.frame(class=pts$class) # create a dataframe with class labels. 
vals$class <- factor(vals$class) # convert to factor
crs(r) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs" # assign CRS used by bing maps
train <- SpatialPointsDataFrame(train, vals, proj4string=crs("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")) # create spatial dataframe with training points

sclass <- superClass(r, trainData=train, responseCol="class",model = "rf", tuneLength = 1) # supervised classification


# plot classified image vs. original image to check
m <- rbind(c(1, 2))
layout(m)
plotRGB(r)
plot(sclass$map)

# now try using classification on another image
names(r2) <- names(r) # give dataframe columns (i.e. image bands) the same names
sclass2 <- predict(r2, sclass$model) # classify using model generated from training points

# now try using classification on another image
names(r3) <- names(r) # give dataframe columns (i.e. image bands) the same names
sclass3 <- predict(r3, sclass$model) # classify using model generated from training points

#===================================
# extract raster values to road vector

# fgdb <- "C:\\Users\\ipdavies\\Documents\\traffic\\bing\\24KLRS_2016\\24k.gdb"
# roads <- readOGR(dsn=fgdb, layer="sr24kLines_20161231")
# roads <- readOGR(dsn="C:\\Users\\ipdavies\\Documents\\traffic\\bing\\StreetMap_Data\\data", layer="mroads")
# save(roads, file="roads")
load("roads")

# clip road feature to raster extent
r.class <- sclass3
projection(r.class) = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")
xmin(r.class) <-  z3[1]
xmax(r.class) <- z1[1] 
ymin(r.class) <- z3[2]
ymax(r.class) <- z1[2] 


# project and add extent in meters to raster
r.class <- projectRaster(r.class,"+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")
# matrix with extent coordinates
coords <- matrix(c(z3[1],z3[2], # xmin, ymin
                 z2[1], z2[2], # xmax, ymin
                 z1[1], z1[2], # xmax, ymax
                 z4[1], z4[2]), # xmin, ymax
                 nrow=4, byrow=TRUE)
# reproject coords from lat/long to meter
rproj <- project(coords, "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")
rproj <- list(x=rproj[,1], y=rproj[,2]) # separate east/west and north/south coordinates
extent(r.class) <- rproj # set extent of raster

# reproject and clip road to raster extent
# roads <- spTransform(roads,crs("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"))
# roads_clip = crop(roads, r.class)
# save(roads_clip, file="roads_clip")

plot(r.class)
plot(roads_clip, add=TRUE)

writeRaster(r.class, filename="rclass3.tif", options=c('TFW=YES'), overwrite=TRUE)
writeOGR(obj=roads_clip, dsn="C:\\Users\\ipdavies\\Documents\\traffic\\bing\\images", layer="road_clip", driver="ESRI Shapefile")
