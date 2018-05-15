# Traffic 2 Vector
R script to summarize traffic conditions into a road vector from Bing static maps API.

## Getting Started
--

## Files

#### run_script.R
Holds user parameters for the map and sets task scheduler to automatically run the `traffic_api.R` script. Can't figure out how to get Windows Task Scheduler to run though.

#### traffic_api.R
This is the main script that takes a bounding box for the ROI and a road vector shapefile and returns a road vector with relative traffic conditions averaged over a given period of time. This works by mosaicking static maps from Bing Maps in the ROI, classifying traffic conditions, and averaging those conditions over the desired time period.

#### raster_math.R
Script to compute averages of all the classified rasters.

#### extract_vector.R
This script takes the final averaged raster created in `traffic_api.R` and extracts the averaged traffic conditions to an underlying road vector that the user provides as a shapefile. This works (potentially) by densifying the road vector with vertices every 100 meters, converting the densified road geometry to points, extracting traffic conditions to the points, then reconnecting the points to line segments. Contiguous line segments with the same traffic value should be merged to reduce file size. This isn't quite done yet - I'm stuck on reconnecting the line segments while retaining original road shape, but I think `extract` might work for polylines by simply extracting to the densified vertices. Will check.

### model_training.R
Supervised classification. Not necessary to run this as the random forest model will is already included in the file `sclass` which `traffic_api.R` calls, but for including background. 

#### map_api_edit.R
Edited functions of `RGoogleMaps` package to allow for more map types (including fixing a typo that incorrectly called aerial maps) and added a parameter to turn map labels on and off. 

#### demo.R
This is a script used for testing the whole shebang. Basically the files above, merged to run independently. It does require multiple save files included in the `Test` folder.


## Running the Script


--

```
example
```

## Authors

**Ian Davies**
