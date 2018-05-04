# Traffic 2 Vector
R script to summarize traffic conditions into a road vector from Bing static maps API

## Getting Started
--

### Files
#### traffic_api.R
This is the main script that takes a bounding box for ROI and a road vector shapefile and returns a road vector with relative traffic conditions averaged over a given period of time. This works by mosaicking static maps from Bing Maps in ROI, classifying traffic conditions, averaging those conditions over the desired time period, and extracting the averaged traffic conditions to the underlying road vector.

#### map_api_edit.R
Edited functions of `RGoogleMaps` package to allow for more map types (including fixing a typo that incorrectly called aerial maps) and added a parameter to turn map labels on and off. 

## Running the Script
--

```
example
```

## Authors

**Ian Davies**
