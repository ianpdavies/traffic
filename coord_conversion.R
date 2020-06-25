#Utility functions for converting back and forth betwene pixel coordinates and lat long coordinates
library(RgoogleMaps) # for accessing Bing Maps API # Need it for XY2LatLon

#Convert lat-long coordinates to Bing map to raw pixel coordinates
latlong_to_pixelcoords <- function(lat, lon, zoom) {
  rawXY <- LatLon2XY(lat,lon,zoom) #Lat Long to tile # + pixel coordinate within tile
  pixX <- rawXY$Tile[,1]*256+floor(rawXY$Coords[,1]) #Pixel coordinate = tile number*256+pixel coordinate within tile 
  pixY <- rawXY$Tile[,2]*256+floor(rawXY$Coords[,2]) #There are always 256 pixels/tile and 2^zoom tiles in the world, so 256*2^zoom pixels in the world
  return(list(X=pixX, Y=pixY))
}

#Convert pixel coordinates to lat-long coordinates
pixelcoords_to_latlong <- function(pixY, pixX, zoom) {
  mapSize <- 256*2^zoom
  dx = (pixX/mapSize) - 0.5
  dy = 0.5 - (pixY/mapSize)
  latitude = 90-360*atan(exp(-dy * 2 * pi))/pi
  longitude = 360*dx
  return(list(lat=latitude,lon=longitude))
}