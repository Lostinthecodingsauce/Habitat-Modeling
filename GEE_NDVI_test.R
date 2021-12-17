#GEE NDVI test 

library(cptcity)
library(raster)
library(stars)
library(rgee)
library(sf)
#library(geoaxe)
#library(gdistance)
library(googledrive)
library(googleCloudStorageR)
library(maps)
library(maptools)
#library(mapview)
library(geojsonio)
library(tiff)
library(stars)
library(rgee)


ee_Initialize(drive = TRUE, gcs = TRUE, email = 'bfscott1906@gmail.com')

setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital/Tiered_Shapefile_Outputs")


#################################################################
## Functions to use
 # ee.Reducer.percentile
 # ee.Reducer.sampleVariance
 # ee.Reducer.skew
 # ee.Reducer.splitWeights
 # ee.Reducer.stdDev

## This is the function that I need to use
# ee.ImageCollection.aggregate_stats

 ## Working final

 ee_roi1 <- st_read("null.polys/Passerina_cyanea.shp") %>%
   st_geometry() %>%
   sf_as_ee()
ee_roi2 <- st_read("null.polys/Passerina_caerulea.shp") %>%
  st_geometry() %>%
  sf_as_ee()
# Search into the Earth Engine's public data archive
#ee_search_dataset() %>%
 # ee_search_title("mod13") %>%
 # ee_search_title("250km") %>%
 # ee_search_display()

#GLDAS <- ee$ImageCollection("NASA/GLDAS/V021/NOAH/G025/T3H")


###########################################3

# modis_ndvi <- ee$ImageCollection("MODIS/006/MOD13A2") #original 
modis_ndvi <- ee$ImageCollection("MODIS/006/MOD13Q1")# ours 

# Filter out poor quality pixels
getQABits <- function(image, qa) {
  # Convert binary (character) to decimal (little endian)
  qa <- sum(2^(which(rev(unlist(strsplit(as.character(qa), "")) == 1))-1))
  # Return a mask band image, giving the qa value.
  image$bitwiseAnd(qa)$lt(1)
}

# Using getQABits we construct a single-argument function 'mod13A2_clean'
mod13A2_clean <- function(img) {
  # Extract the NDVI band
  ndvi_values <- img$select("NDVI")
  
  # Extract the quality band
  ndvi_qa <- img$select("SummaryQA")
  
  # Select pixels to mask
  quality_mask <- getQABits(ndvi_qa, "11")
  
  # Mask pixels with value zero.
  ndvi_values$updateMask(quality_mask)
}

# Create a monthly composite
ndvi_composite <- modis_ndvi$
  filter(ee$Filter$date('2001-01-01', '2019-12-31'))$
  #filter(ee$Filter$calendarRange(6, field = "month"))$
  map(mod13A2_clean)$
  #median()
  mean()
  #variance()

# Display results
scale <- 0.0001
Map$setCenter(lon = -114,lat = 40,zoom = 4)
Map$addLayer(
  eeObject = ndvi_composite,
  visParams = list(
    min = -.2 / scale, # 0.2 is min (-2000 unscaled)
    max = 1 / scale, # 1 is max (10000 unscaled)
    palette = cpt("grass_ndvi", 10)
  )
) + Map$addLayer(ee_roi1)

Map$addLayer(
  eeObject = ndvi_composite,
  visParams = list(
    min = -.2 / scale, # 0.2 is min (-2000 unscaled)
    max = 1 / scale, # 1 is max (10000 unscaled)
    palette = cpt("grass_ndvi", 10)
  )
) + Map$addLayer(ee_roi2)


###################################################################################
 ## Outputing results
#############################################################
# Download raster
ee_raster <- ee_as_raster(
  image = ndvi_composite,
  region = ee_roi$geometry(),
  dsn = "WestTanJune.tif",
  scale = 3000,
  via = "drive"
)
# write csv for local, drop last colum with polygon descriptions
df <- data.frame(ndvi_mean_sf)
#df = subset(ndvi_var_sf, select = c(1,2))
df <- df[-c(3)]
df <- transform(df, NDVI.scaled = NDVI * 0.0001)
write.csv(df, file = "test.meanNDVI.csv")

# Write csv file to drive 
task_vector <- ee_table_to_drive(
  collection = Means,
  fileFormat = "CSV",
  fileNamePrefix = "test"
)
task_vector$start()
ee_monitoring(task_vector)

# ee_manage_cancel_all_running_task()
ndvi_mean_sf <- ee_extract(
  x = ndvi_composite,
  y = ee_roi,
  fun = ee$Reducer$mean(),
  scale = 2000,
  sf = TRUE
)
# Variance
ndvi_var_sf <- ee_extract(
  x = ndvi_composite,
  y = ee_roi,
  fun = ee$Reducer$variance(),
  scale = 2000,
  sf = TRUE
)
############################################################
### NIR example
###########################################################

dataset <- ee$ImageCollection('LANDSAT/LE07/C01/T1_SR')


# Filter out poor quality pixels
getQABits <- function(image, qa) {
  # Convert binary (character) to decimal (little endian)
  qa <- sum(2^(which(rev(unlist(strsplit(as.character(qa), "")) == 1))-1))
  # Return a mask band image, giving the qa value.
  image$bitwiseAnd(qa)$lt(1)
}

# Using getQABits we construct a single-argument function 'mod13A2_clean'
l8_clean <- function(img) {
  # Calculate the NDVI
  ndvi_values <- img$normalizedDifference(c("B4"))
  
  # Extract the quality band
  ndvi_qa <- img$select("pixel_qa")
  
  # Select pixels to mask
  quality_mask <- getQABits(ndvi_qa, "00000100001")
  
  # Mask pixels with value zero.
  ndvi_values %>%
    ee$Image$updateMask(quality_mask) %>%
    ee$Image$copyProperties(img, list("system:time_start"))
}


# Create a monthly composite
ndvi_composite <- dataset$
  filterDate('2010-01-01', '2016-12-31')$
  filter(ee$Filter$calendarRange(1, field = "month"))$
  map(l8_clean)$
  median()

# Display results
scale <- 0.0001
Map$addLayer(
  eeObject = ndvi_composite,
  visParams = list(
    min = 0.2 ,
    max = 0.7 ,
    palette = cpt("grass_ndvi", 10)
  )
) + Map$addLayer(ee_roi1)



#############################################################
### Creating variance in output file rather than mean
#############################################################

# This is a working document, very messy
ee.ImageCollection.aggregate_stats

ndvi <- ee$ImageCollection("MODIS/006/MOD13Q1")$ # ours 
  filter(ee$Filter$date('2019-01-01', '2019-12-31'))

ee.ImageCollection.aggregate_stats





# Compute the weighted mean of the NDWI image clipped to the region.


# Compute the median in each band, each pixel.
# Band names are B1_median, B2_median, etc.
median <- collection$reduce(ee$Reducer$median())
var <- collection$reduce(ee$Reducer$variance())
# The output is an Image.  Add it to the map.
vis_paramV <- list(bands = c("B4_variance", "B3_variance", "B2_variance"), gamma = 1.6)
vis_paramM <- list(bands = c("B4_median", "B3_median", "B2_median"), gamma = 1.6)
Map$setCenter(-122.3355, 37.7924, 4)
Map$addLayer(
  eeObject = var,
  visParams = vis_paramV,
  name = "Variance"
)
##############################################
#More complex reductions are also possible using reduce(). 
#For example, to compute the long term linear trend over a 
#collection, use one of the linear regression reducers. 
#The following code computes the linear trend of MODIS Enhanced 
#Vegetation Index (EVI):
 


################################################
# Get list of images (1 per day)
ListOfImages = ImageCollection$toList(ImageCollection$size());
# Get list of images (1 per day)
ListOfImages = ImageCollection$toList(ImageCollection$size());

# first image
image <- ee$Image(ListOfImages$get(0))

# Add the mean of each band as new properties of each polygon
Means = image$reduceRegions(collection = polygonsCollection,reducer= ee$Reducer$variance())
Means$getInfo()

#############################################################
# Download raster
ee_raster <- ee_as_raster(
  image = ndvi_composite,
  region = ee_roi$geometry(),
  dsn = "WestTanJune.tif",
  scale = 3000,
  via = "drive"
)
# Write csv file to drive 
task_vector <- ee_table_to_drive(
  collection = Means,
  fileFormat = "CSV",
  fileNamePrefix = "test"
)
task_vector$start()
ee_monitoring(task_vector)

# ee_manage_cancel_all_running_task()
ndvi_mean_sf <- ee_extract(
  x = ndvi_composite,
  y = ee_roi,
  fun = ee$Reducer$mean(),
  scale = 2000,
  sf = TRUE
)
# write csv, drop last colum with polygon descriptions
df <- data.frame(ndvi_mean_sf)
#df = subset(ndvi_var_sf, select = c(1,2))
df <- df[-c(3)]
df <- transform(df, NDVI.scaled = NDVI * 0.0001)
write.csv(df, file = "test.meanNDVI.csv")

########################################
## Same analysis but using TIFF. Currently does not work due to python do.call issues
library(rgee)
library(raster)
#ee_Initialize("csaybar",gcs = TRUE)

Cardsin <- raster("card_sin_raster/card_sin_test.tif")
ee_Cardsin <- sprintf("%s/card_sin_test", ee_get_assethome())
ee_stars_02 <- raster_as_ee(
  x = car_data,
  overwrite = TRUE,
  assetId = ee_Cardsin,
  bucket = "rgee_dev"
)
Map$centerObject(ee_stars_02)
Map$addLayer(ee_stars_02)
###
ee_stars_02 <- raster_as_ee(
  x = Cardsin,
  overwrite = TRUE,
  assetId = ee_Cardsin,
  bucket = "cardndvi"
)

######
tif <- system.file("card_sin_raster/card_sin_test.tif", package = "stars")
x <- stack(tif)
ee_roi <- sprintf("%s/%s",ee_get_assethome(),'card_sin_test')

## THis works??S
library(raster)
str_name<-"card_sin_raster/card_sin_test.tif"
imported_raster=raster(str_name)
Cardsin <- imported_raster
raster::plot(Cardsin)

ee_roi <- stars_as_ee(Cardsin)
ee_roi <- raster_as_ee(Cardsin, card_sin_test, bucket = "cardndvi")

setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital/card_sin_raster")
rlist=list.files(getwd(), pattern="tif$", full.names=FALSE)
for(i in rlist) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) } 

##############################################################
ee_roi <- raster_as_ee(Cardsin, card_sin_test, bucket = "cardndvi")
ee_stars_01 <- ee$Image()
# Map$centerObject(ee_stars_01)
# Map$addLayer(ee_stars_01, list(min = 0, max = 255))
ee_stars_02 <- raster_as_ee(
  x = Cardsin,
  overwrite = TRUE,
  assetId = card_sin_test,
  bucket = "cardndvi"
)
Map$addLayer(ee_stars_02)

############################################

## commands to explore
# ee table to assest 
##########################################################################
ee_roi <- st_read("TestTan/TestTan_Aggregate.shp") %>%
  st_geometry() %>%
  sf_as_ee()
region <- ee_roi$geometry()$bounds()

# Retrieve the MODIS Terra Vegetation Indices 16-Day Global 1km dataset as an ee.ImageCollection and select the NDVI band.
col <- ee$ImageCollection('MODIS/006/MOD13A2')$select('NDVI')

# Group images by composite date
col <- col$map(function(img) {
  doy <- ee$Date(img$get('system:time_start'))$getRelative('day', 'year')
  img$set('doy', doy)
})
distinctDOY <- col$filterDate('2013-01-01', '2014-01-01')

#Define a filter that identifies which images from the complete collection match the DOY from the distinct DOY collection.
filter <- ee$Filter$equals(leftField = 'doy', rightField = 'doy');

# Define a join; convert the resulting FeatureCollection to an ImageCollection.
join <- ee$Join$saveAll('doy_matches')
joinCol <- ee$ImageCollection(join$apply(distinctDOY, col, filter))

#Apply median reduction among matching DOY collections.
comp <- joinCol$map(function(img) {
  doyCol = ee$ImageCollection$fromImages(
    img$get('doy_matches')
  )
  doyCol$reduce(ee$Reducer$median())
})

Map$addLayer(
  eeObject = col,
  visParams = list(
    min = -.2 / scale, # 0.2 is min (-2000 unscaled)
    max = 1 / scale, # 1 is max (10000 unscaled)
    palette = cpt("grass_ndvi", 10)
  )
) + Map$addLayer(ee_roi)


#Define RGB visualization parameters.
visParams = list(
  min = 0.0,
  max = 9000.0,
  bands = "NDVI_median",
  palette = c(
    'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
    '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
    '012E01', '011D01', '011301'
  )
)

# gganimate
colnames(ee_nc_rain) <- sprintf("%02d", 1:12)
ee_nc_rain$name <- nc$NAME

ee_nc_rain %>%
  pivot_longer(-name, names_to = "month", values_to = "pr") %>%
  ggplot(aes(x = as.integer(month), y = pr, color = pr)) +
  geom_line(alpha = 0.8, size = 2) +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  theme_minimal() +
  transition_states(name) +
  shadow_mark(size = 0.4, colour = "grey")


## Does not work, python error on thier end with do.call
#Create RGB visualization images for use as animation frames.
rgbVis <- comp$map(function(img) {
  do.call(comp$ img$visualize, visParams) %>% 
    ee$Image$clip(mask)
})

#Define GIF visualization parameters.

gifParams <- list(
  region = region,
  dimensions = 600,
  crs = 'EPSG:3857',
  framesPerSecond = 10
)
# Render the GIF animation in the console.

print(rgbVis$getVideoThumbURL(gifParams))
browseURL(rgbVis$getVideoThumbURL(gifParams))



#### forloop 
### Set MODIS Data using functions defined above ###
# https://stackoverflow.com/questions/63040059/write-a-for-loop-in-google-earth-engine
# Get mean and SD in every band by combining reducers.
stats <- ndvi_composite$reduceRegion(
  reducer = ee$Reducer$mean()$combine(
    reducer2 = ee$Reducer$stdDev(),
    sharedInputs = TRUE
  ),
  geometry = ee$Geometry$Rectangle(c(-2.15, 48.55, -1.83, 48.72)),
  scale = 10,
  bestEffort = TRUE # Use maxPixels if you care about scale.
)

print(stats$getInfo())

# Extract means and SDs to images.
meansImage <- stats$toImage()$select('.*_mean')
sdsImage <- stats$toImage()$select('.*_stdDev')


# Set parameters
setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital/Shapefiles")
filenames <- list.files(path = getwd(), pattern = "*shp")
numfiles <- length(filenames)
dataset = list()

display_l8ts <- list()
for (l8 in l8_ts) {
  ee_l8 <- ee$Image(l8)
  display_l8ts[[l8]] <- Map$addLayer(ee_l8)
}

Map$centerObject(ee_l8)
Reduce('+', display_l8ts)


# Generate for loop





##########################################################################
ee_search_dataset() %>%
  ee_search_title("mod13") %>%
  ee_search_title("1km") %>%
  ee_search_display()               

# s2 <- ee$ImageCollection("COPERNICUS/S2_SR") # example
s2 <- ee$ImageCollection("MODIS/006/MOD13Q1")# ours 

getQABits <- function(image, qa) {
  # Convert decimal (character) to decimal (little endian)
  qa <- sum(2^(which(rev(unlist(strsplit(as.character(qa), "")) == 1))-1))
  # Return a single band image of the extracted QA bits, giving the qa value.
  image$bitwiseAnd(qa)$lt(1)
}

mod13A2_clean <- function(img) {
    # Extract the NDVI band
    ndvi_values <- img$select("NDVI")
    
    # Extract the quality band
    ndvi_qa <- img$select("SummaryQA")
    
    # Select pixels to mask
    quality_mask <- getQABits(ndvi_qa, "11")
    
    # Mask pixels with value zero.
    ndvi_values$updateMask(quality_mask)
  }

#####

s2_tan <- s2$
  filterBounds(ee_roi)$ # Select S2 images just for the Ocoña Valley region.
  filter(ee$Filter$lte("CLOUDY_PIXEL_PERCENTAGE", 20))$ # Remove images with a cloudy-scene pixel percentage greater than 20.
  filter(ee$Filter$date('2001-03-01', '2019-08-31'))$ #Select images from 2017-01-01 to the present day.
 # filter(ee$Filter$calendarRange(6, field = "month"))$ #Select images only for June.
  map(mod13A2_clean) # Map over the collection to remove cloud pixels.

nimages <- s2_tan$size()$getInfo()
ic_date <- ee_get_date_ic(s2_tan)


Map$setCenter(lon = -114,lat = 40,zoom = 4)
s2_img_list <- list() 
for (index in seq_len(nimages)) {
  py_index <- index - 1
  s2_img <- ee$Image(s2_tan$toList(1, py_index)$get(0))
  s2_img_list[[index]] <- Map$addLayer(
    eeObject = s2_img,
    visParams = list(min = -0.1, max = 0.8, palette = cpt("grass_ndvi", 10)),
    name = ic_date$id[index]
  )
}
Reduce('+', s2_img_list)
