# REST API parameters here
# https://msdn.microsoft.com/en-us/library/ff701724.aspx
# custom parameters here
# https://msdn.microsoft.com/en-us/library/mt823636.aspx


GetBingMap2 <- function (center = c(lat = 42, lon = -76), mapArea = c(45.219, -122.325, 47.61, -122.107), size = c(1500, 1500), destfile, 
                         zoom = 12, markers, path = "", 
                         maptype = c("Road", "Aerial", "CanvasDark", "CanvasGray","CanvasLight","AerialWithLabels")[1], 
                         format = c("png", "gif", "jpg")[1], #In effect, only png, gif, and jpg are accepted
                         extraURL = "", GRAYSCALE = FALSE, NEWMAP = TRUE, DISK=FALSE, MEMORY=TRUE, SCALE = 1, apiKey = NULL, 
                         verbose = 0, labels=TRUE) 
{
  if (!(maptype %in% c("Road", "Aerial", "CanvasDark", "CanvasGray","CanvasLight", "AerialWithLabels"))) # got rid of extra space in "Aerial ", added other styles
    maptype = "Road"
  if (missing(destfile)) 
    destfile = file.path(tempdir(), "mapTile.png")
  if (is.character(center)) {
    if (verbose) 
      cat("geocoding ", center, "\n")
    center = getGeoCode(center, verbose)
  }
  if (all(c("lat", "lon") %in% names(center))) 
    center = center[c("lat", "lon")]
  stopifnot(size[1] <= 2000)
  stopifnot(size[2] <= 1500)
  
  fileBase <- substring(destfile, 1, nchar(destfile) - 4)
  fileExt <- substring(destfile, nchar(destfile) - 2, nchar(destfile))
  if (is.null(center)) {
    if (verbose) 
      print("Note that when center and zoom are not specified, no meta information on the map tile can be stored. This basically means that R cannot compute proper coordinates. You can still download the map tile and view it in R but overlays are not possible.")
    MetaInfo <- list(lat.center = NULL, lon.center = NULL, 
                     zoom = zoom, url = "bing", BBOX = NULL, size = size, 
                     SCALE = SCALE)
    save(MetaInfo, file = paste(destfile, "rda", sep = "."))
  }
  else if (is.numeric(center) & !missing(zoom)) {
    MyMap <- list(lat.center = center[1], lon.center = center[2], 
                  zoom = zoom, SCALE = SCALE)
    BBOX <- list(ll = XY2LatLon(MyMap, -size[1]/2 + 0.5, 
                                -size[2]/2 - 0.5), ur = XY2LatLon(MyMap, size[1]/2 + 
                                                                    0.5, size[2]/2 - 0.5))
    MetaInfo <- list(lat.center = center[1], lon.center = center[2], 
                     zoom = zoom, url = "bing", BBOX = BBOX, size = size, 
                     SCALE = SCALE)
    save(MetaInfo, file = paste(destfile, "rda", sep = "."))
  }
  if (length(size) < 2) {
    s <- paste(size, size, sep = ",")
  }
  else {
    s <- paste(size, collapse = ",")
  }
  if (!is.null(center)) 
    center <- paste(center, collapse = ",")
  bingURL = paste0("http://dev.virtualearth.net/REST/v1/Imagery/Map/", 
                   maptype, "/")
  if (is.null(center) & missing(zoom)) {
    stopifnot(!missing(markers) | path != "")
    url <- paste0(bingURL, "size=", s, "&maptype=", maptype, 
                  "&format=", format)
  }
  else if (missing(mapArea)) {
    stopifnot(!is.null(center), !missing(zoom))
    url <- paste0(bingURL, center, "/", zoom, "?mapSize=", 
                  s, "&format=", format)
  }
  else if (!missing(mapArea)) {
    latR = range(mapArea[c(1, 3)])
    lonR = range(mapArea[c(2, 4)])
    # zoom <- min(MaxZoom(latR, lonR, c(size,size)))
    lat.center <- mean(latR)
    lon.center <- mean(lonR)
    center = c(lat.center, lon.center)
    BBOX = list(ll = mapArea[1:2], ur = mapArea[3:4])
    names(BBOX$ll) = c("lat", "lon")
    names(BBOX$ur) = c("lat", "lon")
    MetaInfo <- list(lat.center = center[1], lon.center = center[2], 
                     zoom = zoom, url = "bing", BBOX = BBOX, size = size, 
                     SCALE = SCALE)
    save(MetaInfo, file = paste(destfile, "rda", sep = "."))
    bingURL = paste0("http://dev.virtualearth.net/REST/v1/Imagery/Map/", 
                     maptype, "?mapArea=")
    url <- paste0(bingURL, paste0(mapArea, collapse = ","), 
                  "&mapSize=", s, "&format=", format, "&zoomLevel=", zoom)
  }
  url <- paste(url, path, sep = "")
  url <- paste(url, extraURL, sep = "")
  if (!missing(markers)) {
    if (is.matrix(markers) | is.data.frame(markers)) {
      stopifnot(all(c("lat", "lon") %in% colnames(markers)))
      latlon = which(colnames(markers) %in% c("lat", "lon"))
      for (i in 1:nrow(markers)) {
        m1 <- paste(markers[i, c("lat", "lon")], collapse = ",")
        if (any(c("size", "color", "label") %in% colnames(markers))) {
          m2 <- paste(colnames(markers)[-latlon], markers[i, 
                                                          -latlon], collapse = "|", sep = ":")
          m <- paste(m2, m1, sep = "|")
        }
        else {
          m <- m1
        }
        if (i == 1) {
          markers.string <- paste0("&markers=", m)
        }
        else {
          markers.string <- paste(markers.string, paste0("&markers=", 
                                                         m), sep = "")
        }
      }
    }
    else if (is.character(markers)) {
      markers.string <- markers
    }
    url <- paste0(url, markers.string)
  }
  url <- paste0(url, "&key=", apiKey)
  if(labels==FALSE & maptype=="Aerial"){ # added this to catch error
    stop("Can't retrieve Aerial map with labels=FALSE")
  }
  if(maptype=='CanvasDark' & labels==FALSE){ # added custom map style without labels to URL
    #url <- paste0(url, "&st=rd|lv:FALSE;fc=000000_;strokeColor=000000")
    #For reference, see https://www.bing.com/api/maps/sdk/mapcontrol/isdk/custommaptilestylesandhexcolor#JS
    #To try stuff out, see https://www.bing.com/api/maps/sdk/mapcontrol/isdk/custommaptilestylesandhexcolor#JS
    url <- paste0(url, "&st=road|sc:000000;fc:000000;lv:0_rl|v:0;lv:0_trl|v:0;lv:0_wr|v:0;lv:0_pp|v:0;lv:0_pl|v:0;lv:0_wt|v:0;lv:0_ar|v:0;lv:0")
  }
  if (verbose) 
    print(url)
  if (verbose == -1) 
    browser()
  if (verbose < 2 & NEWMAP & DISK) {
    suppressWarnings(download.file(url, destfile, mode = "wb", 
                                   quiet = TRUE)) #First write
    if (MEMORY) {
    myMap <- ReadMapTile(destfile) #Then read
    return(myMap)
    }
  }
  if (verbose < 2 & NEWMAP & DISK==F & MEMORY) {
    req <- GET(url) #httr package
    if (req$status_code != 200) print(req$status_code) #Check whether error in request
    return(content(req, "parsed")) #Directly parse as PNG
  }
  if (GRAYSCALE) {
    myTile <- readPNG(destfile, native = FALSE)
    myTile <- RGB2GRAY(myTile)
    writePNG(myTile, destfile)
  }
  invisible(url)
}
