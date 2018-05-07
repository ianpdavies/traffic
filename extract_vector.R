#===================================
# This extracts the values from the raster to a shapefile of a road network

# the reason why we do this is because georectifying the vector to the raster won't work if the image is smaller than the minimum tile size
# given a starting point z4, this computes an image that is the minimum tile size
z4 <- c(-122.324618, 47.598470) # top left corner of image
z1<-destPointRhumb(z4, b=90, r=radius, d=img.size) # find distance to top right corner using the size of map tile at zoom 15, 640 px
z2 <- destPointRhumb(z1, b=180, r=radius, d=img.size)# bottom right corner
z3 <- destPointRhumb(z2, b=-90, r=radius, d=img.size)# bottom left corner


#===================================
# clip road vector to raster extent

# fgdb <- "C:\\Users\\ipdavies\\Documents\\traffic\\bing\\24KLRS_2016\\24k.gdb"
# roads <- readOGR(dsn=fgdb, layer="sr24kLines_20161231")
# roads <- readOGR(dsn="C:\\Users\\ipdavies\\Documents\\traffic\\bing\\StreetMap_Data\\data", layer="mroads")
# save(roads, file="roads")
# load("roads")

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

load("roads_clip")
plot(r.class)
plot(roads_clip, add=TRUE)

writeRaster(r.class, filename="rclass3.tif", options=c('TFW=YES'), overwrite=TRUE)
writeOGR(obj=roads_clip, dsn="C:\\Users\\ipdavies\\Documents\\traffic\\bing\\images", layer="road_clip", driver="ESRI Shapefile")

#===================================
# extract raster values to road vector

library(smoothr)
library(sf)
library(spatstat) # don't need

load("roads_clip") # load road line vectors
load("imgs")
load("sclasses")

roads_clip@data$ID = seq(1, length(roads_clip), 1) # add unique road identifier for each road obs in spatial df

roads.clip.sf <- st_as_sf(roads_clip) # convert sp to sf object so can use st_cast function

roads_dense <- densify(roads.clip.sf$geometry, n=10) # densify lines by adding equidistant vertices along paths (play around with max_distance or n parameters)

roads_dense <- roads_dense %>% st_cast("POINT") # convert line vertices (?) to points

roads_dense <- as(roads_dense, "Spatial") # convert sf object to sp object so we can use extract()

conds <-extract(r.class, roads_dense) # extract classified raster values to a vector

roads_class <-SpatialPointsDataFrame(roads_dense@coords, data.frame(conds)) # create a spatialpointsdataframe with point coordinates and traffic classes

spplot(roads_class, "conds")

p <- spLines(matrix(roads_class@coords,ncol=2)) # use points as vertices to create line segments. also need this to be a lines df so it keeps conditions


# using spLines connects unconnected roads to each other ... maybe giving each segment a road identifier, then looping through to create individual line segments, and adding them all into one lines dataframe will work

# https://gis.stackexchange.com/questions/242664/merge-a-list-of-spatiallines



# =================================
# this function densifies each line individually, extracts traffic conditions, and creates line segments, then rejoins all segments into one road file
for(i in 1:length(roads_clip)){
  road.conds = list()
  roads.clip.sf <- st_as_sf(roads_clip[1,]) # create sf object so can use st_cast function
  d <- lapply(slot(roads_clip[1,], "lines"), # extracts the coordinates of the given line
              function(x) lapply(slot(x, "Lines"), 
                                 function(y) slot(y, "coords")))
  d <- matrix(d[[1]][[1]], ncol=2) # get coords into matrix form for LineLength function
  d <- LineLength(d) # computes length of line using coordinates
  roads.dense <- densify(roads.clip.sf$geometry, n = ceiling(d/100)) # densify lines by adding equidistant vertices every ~100 meters
  roads.dense <- roads.dense %>% st_cast("POINT") # convert line vertices to points
  roads.dense <- as(roads.dense, "Spatial") # convert sf object to sp object so we can use extract()
  conds <-extract(r.class, roads.dense) # extract classified raster values to a vector
  # roads.class <-SpatialPointsDataFrame(roads.dense@coords, data.frame(conds)) # create a spatialpointsdataframe with point coordinates and traffic classes
  # create list with line coords and traffic values
  # create spatiallinesdataframe from list
  # join original road attribute data using column ID
  road.conds[[1]] <- SpatialLines(list(Lines(list(Line(roads.dense@coords)), ID=i)))
  road.conds <- do.call(rbind, road.conds)
  conds <- data.frame(conds=conds)
  road.conds <- SpatialLinesDataFrame(road.conds, data=conds)
  # roads.conds <- spLines(matrix(roads_class@coords,ncol=2)) # connects points to create one line
  # probably want to create the eventual line shapefile from sf, not sp to save time but that can be done later
  
  # okay problem here is that the above code only gives us 1 feature!
  # we need each line SEGMENT to be a feature, so we can assign a traffic condition
  # may need to nest another loop to create spatiallines from segments
  
  # if that results in too many segments, we can merge adjacent segments that have the same values
  # this will result in long unchanging segments and short dynamic segments at busy intersections and such
}


# this function densifies each line individually, extracts traffic conditions, and creates line segments, then rejoins all segments into one road file
road.segs = list() # maybe list(list()) if we want to have first level be lines, second level be line segments.
road.conds= c()
for(i in 1:length(roads_clip)){
  roads.clip.sf <- st_as_sf(roads_clip[i,]) # create sf object so can use st_cast function
  d <- lapply(slot(roads_clip[i,], "lines"), # extracts the coordinates of the given line
              function(x) lapply(slot(x, "Lines"), 
                                 function(y) slot(y, "coords")))
  d <- matrix(d[[1]][[1]], ncol=2) # get coords into matrix form for LineLength function
  d <- LineLength(d) # computes length of line using coordinates
  roads.dense <- densify(roads.clip.sf$geometry, n = ceiling(d/100)) # densify lines by adding equidistant vertices every ~100 meters
  roads.dense <- roads.dense %>% st_cast("POINT") # convert line vertices to points
  roads.dense <- as(roads.dense, "Spatial") # convert sf object to sp object so we can use extract()
  road.conds = c(road.conds, extract(r.class, roads.dense)) # extract classified raster values to a vector
  # roads.class <-SpatialPointsDataFrame(roads.dense@coords, data.frame(conds)) # create a spatialpointsdataframe with point coordinates and traffic classes
  # create list with line coords and traffic values
  # create spatiallinesdataframe from list
  # join original road attribute data using column ID
  for(j in seq(1, length(roads.dense@coords[,1])-1, 1)){
    q = length(road.segs) + j # can't use j to index road.segs because it will overwrite previous entries
    road.segs[[q]] <- SpatialLines(list(Lines(list(Line(roads.dense@coords[j:(j+1),])), ID=i)))
  }
}

road.segs <- do.call(rbind, road.segs) # combines all lines in road.conds into one spatial object, I think
road.conds <- data.frame(conditions=road.conds)
road.segs <- SpatialLinesDataFrame(road.segs, data=road.conds)
# roads.conds <- spLines(matrix(roads_class@coords,ncol=2)) # connects points to create one line
# probably want to create the eventual line shapefile from sf, not sp to save time but that can be done later

# okay problem here is that the above code only gives us 1 feature!
# we need each line SEGMENT to be a feature, so we can assign a traffic condition
# may need to nest another loop to create spatiallines from segments

# if that results in too many segments, we can merge adjacent segments that have the same values
# this will result in long unchanging segments and short dynamic segments at busy intersections and such



plot(SpatialLines(list(Lines(list(Line(d)), ID="1"))))
SpatialLines(list(Lines(list(Line(cbind(c(1,2,3),c(3,2,2)))),ID="a"))) 

sp.lines <- list()
sp.lines[[1]] <- SpatialLines(list(Lines(list(Line(cbind(c(1,2,3),c(3,2,2)))),ID="a"))) 
sp.lines[[2]] <- SpatialLines(list(Lines(list(Line(cbind(c(1,2,3)+0.5,c(3,2,2)+0.5))),ID="b"))) 
sp.lines[[3]] <- SpatialLines(list(Lines(list(Line(cbind(c(1,2,3),c(1,1.5,1)))),ID="c"))) 

merged.lines <- do.call(rbind, sp.lines)
class(merged.lines)
length(merged.lines)
plot(merged.lines, col=1:3)

merged.lines <- SpatialLinesDataFrame(merged.lines, data=data.frame("Z" = c("r","y","t","r","y","t")))


#====================================

## from the sp vignette:
l1 <- cbind(c(1, 2, 3), c(3, 2, 2))
l2 <- cbind(c(1, 2, 3), c(1, 1.5, 1))

Sl1 <- Line(l1)
Sl2 <- Line(l2)

S1 <- Lines(list(Sl1), ID = "a")
S2 <- Lines(list(Sl2), ID = "b")

Sl <- SpatialLines(list(S1, S2))

## sample data: line lengths
library(rgeos)
df <- data.frame(len = sapply(1:length(Sl), function(i) gLength(Sl[i, ])))
rownames(df) <- sapply(1:length(Sl), function(i) Sl@lines[[i]]@ID)


## SpatialLines to SpatialLinesDataFrame
Sldf <- SpatialLinesDataFrame(Sl, data = df)



x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))
