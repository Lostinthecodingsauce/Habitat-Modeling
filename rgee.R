library(tidyverse)
library(rgee)
library(sf)
#https://r-spatial.github.io/rgee/
# https://csaybar.github.io/rgee-examples/#Reducer
#  https://stackoverflow.com/questions/63040059/write-a-for-loop-in-google-earth-engine
  
## It is necessary just once
ee_install()

# Initialize Earth Engine!
ee_Initialize()


library(raster)

dat <- structure(list(ID = 758432:758443, 
                      lat = c(24.875, 24.875, 24.625, 24.625, 24.875, 24.875, 24.625, 24.625, 24.375, 24.375, 24.125, 24.125), 
                      lon = c(72.875, 72.625, 72.625, 72.875, 72.375, 72.125, 72.125, 72.375, 72.375, 72.125, 72.125, 72.375)), 
                 class = "data.frame", row.names = c(NA, -12L))


dat_rast <- rasterFromXYZ(dat[, c('lon', 'lat', 'ID')], crs = '+proj=longlat +datum=WGS84 +no_defs')
dat_poly <- rasterToPolygons(dat_rast, fun=NULL, na.rm=TRUE, dissolve=FALSE)


# A few days for test
startDate = ee$Date('2020-01-01');
endDate = ee$Date('2020-01-10');


# Open dataset
ImageCollection = ee$ImageCollection('NASA/NEX-GDDP')$filter(ee$Filter$date(startDate, endDate))#$filterBounds(polygonsCollection)

# Polygons collection
coords <- as.data.frame(raster::geom(dat_poly))
polygonsFeatures <- coords %>% split(.$object) %>% purrr::map(~{  
  ee$Feature(ee$Geometry$Polygon(mapply( function(x,y){list(x,y)} ,.x$x,.x$y,SIMPLIFY=F)))
})

polygonsCollection = ee$FeatureCollection(unname(polygonsFeatures))
Map$addLayer(polygonsCollection)


# Get list of images (1 per day)
ListOfImages = ImageCollection$toList(ImageCollection$size());

# first image
image <- ee$Image(ListOfImages$get(0))

# Add the mean of each band as new properties of each polygon
Means = image$reduceRegions(collection = polygonsCollection,reducer= ee$Reducer$mean())
Means$getInfo()


# The polygons data can be downloaded on Google Drive:

task_vector <- ee_table_to_drive(
  collection = Means,
  fileFormat = "CSV",
  fileNamePrefix = "test"
)
task_vector$start()
ee_monitoring(task_vector)

### Map data over all days
# Calculate means for all dates
calcMean <- function(image) {
  image$reduceRegions(collection = polygonsCollection,reducer= ee$Reducer$mean())
}

DayMeans <- ImageCollection$map(calcMean)$flatten()

task_vector <- ee_table_to_drive(
  collection = DayMeans,
  fileFormat = "CSV",
  fileNamePrefix = "DayMeans"
)
task_vector$start()