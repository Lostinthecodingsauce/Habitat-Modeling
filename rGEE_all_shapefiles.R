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


ee_Initialize(drive = TRUE,gcs = TRUE, email = 'bfscott1906@gmail.com')

setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital/Tiered_Shapefile_Outputs")
# We genereated minimum convvex polygons based on aggregation distance of points. THese are teired based on how many samples each species has.
# Polygons are created around clusters of three or more points with the aggregation distance. 
# So with a large aggregation distance, further apart points can be considered part of a cluster.
# Distance was chosen based of file size, following the scheme in the excel document "Aggregation distance logic:
# Distance metric for each groups are as follows:

# species with the fewest occurrences
# 1) Tier A -150km
# 2) Tier B -100km
# 3) Tier C -150km
# 4) Tier D -150km
# 5) Tier E -150km
# Species with the most occurrences 

## Breeding season = season = 2
# nonbreeding = 4
# Passage = 3

#################################################################
## Functions to use
# ee.Reducer.percentile
# ee.Reducer.sampleVariance
# ee.Reducer.skew
# ee.Reducer.splitWeights
# ee.Reducer.stdDev

## This is the function that I need to use, onloy avaliable on GEE
# ee.ImageCollection.aggregate_stats


## I need to run analysis for NDVI, ndvi, and near-infared
# sur_refl_b02 = NIR 

##############################################################
## Test file 
setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital/Tiered_Shapefile_Outputs/Tier A - 150km")

ee_roi <- st_read("Tier A-150km/Chlorothraupis_frenata/Chlorothraupis_frenata_Agg.shp") %>%
  st_geometry() %>%
  sf_as_ee()

# Search into the Earth Engine's public data archive
ee_search_dataset() %>%
 ee_search_title("mod13") %>%
 ee_search_title("250km") %>%
 ee_search_display()

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
  #ndvi_values <- img$select("NDVI")
  ndvi_values <- img$select("NDVI")
  
  # Extract the quality band
  #ndvi_qa <- img$select("SummaryQA")
  ndvi_qa <- img$select("SummaryQA")
  
  # Select pixels to mask
  #quality_mask <- getQABits(ndvi_qa, "11")
   quality_mask <- getQABits(ndvi_qa, "11")
  
  # Mask pixels with value zero.
  #ndvi_values$updateMask(quality_mask)
  ndvi_values$updateMask(quality_mask)
}

# Create a monthly composite
ndvi_composite <- modis_ndvi$
  filter(ee$Filter$date('2010-01-01', '2019-12-31'))$
  #filter(ee$Filter$calendarRange(6, field = "month"))$
  map(mod13A2_clean)$
  #median()
  mean()


# Display results
scale <- 0.0001
Map$setCenter(lon = -81,lat = 9,zoom = 2)
Map$addLayer(
  eeObject = ndvi_composite,
  visParams = list(
    min = -.2 / scale, # 0.2 is min (-2000 unscaled)
    max = 1 / scale, # 1 is max (10000 unscaled)
    palette = cpt("grass_ndvi", 10)
  )
) + Map$addLayer(ee_roi)

##############################################################













# For each shapefile. Since this is impssible to make as a for-loop, I have 
## subsetted all polygons by species, and ran each one independently
### RGEE is not built for forloops (yet)
#### woof
library(rgdal)
setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital")
Shapes <- readOGR("CardShapes.shp") #import shape files 
names <- read.csv("Cardinalidae-names.csv") # import species names
Offical <- readOGR("Official range maps/Card_maps.shp") #import shape files 
species <- names$Species # save as object
## Subset data
# Okay so I gave up on subsetting in for loop and did it by hand. Took 30 secs in excel
# This is gonna be better to do in sections to reduce memory usage 
#######################################################3
#set 1
Amaurospiza_carrizalensis<- subset(Shapes, Shapes$species == "Amaurospiza carrizalensis") #good 
Amaurospiza_concolor<- subset(Shapes, Shapes$species == "Amaurospiza concolor") #good 
Amaurospiza_moesta<- subset(Shapes, Shapes$species == "Amaurospiza moesta") #bad 
#Cardinalis_cardinalis<- subset(Shapes, Shapes$species == "Cardinalis cardinalis") #bad 
Cardinalis_cardinalis1<- subset(Offical, Offical$OBJECTID == "16181") # Use 
Cardinalis_phoeniceus<- subset(Shapes, Shapes$species == "Cardinalis phoeniceus")
Cardinalis_sinuatus<- subset(Shapes, Shapes$species == "Cardinalis sinuatus") #good 
Caryothraustes_canadensis<- subset(Shapes, Shapes$species == "Caryothraustes canadensis") #good 
Caryothraustes_poliogaster<- subset(Shapes, Shapes$species == "Caryothraustes poliogaster") #good 
writeOGR(Cardinalis_cardinalis1, "Official range maps/")
##############################################################
a <- st_as_sf(Amaurospiza_concolor)

ee_roi <- sf_as_ee(a)
ndvi_mean_sf <- ee_extract(
  x = ndvi_composite,
  y = ee_roi,
  fun = ee$Reducer$mean(),
  scale = 2000,
  sf = TRUE)
ndvi_var_sf <- ee_extract(
  x = ndvi_composite,
  y = ee_roi,
  fun = ee$Reducer$variance(),
  scale = 2000,
  sf = TRUE)
# write csv for local, drop last colum with polygon descriptions
dfM <- data.frame(ndvi_mean_sf)
dfV <- data.frame(ndvi_var_sf)
dfM <- transform(dfM, MeanNDVI.scaled = NDVI * 0.0001)
dfV <- transform(dfV, VarNDVI.scaled = NDVI * 0.0001)
Alldf <- cbind.data.frame(dfM,dfV)
df = subset(Alldf, select = c(2,5,10))
write.csv(df, file = "NDVI.test/Amaurospiza_concolor.NDVI.csv")
#################
a <- st_as_sf(Amaurospiza_moesta)
ee_roi <- sf_as_ee(a)

ndvi_mean_sf <- ee_extract(
  x = ndvi_composite,
  y = ee_roi,
  fun = ee$Reducer$mean(),
  scale = 2000,
  sf = TRUE)
ndvi_var_sf <- ee_extract(
  x = ndvi_composite,
  y = ee_roi,
  fun = ee$Reducer$variance(),
  scale = 2000,
  sf = TRUE)
# write csv for local, drop last colum with polygon descriptions
dfM <- data.frame(ndvi_mean_sf)
dfV <- data.frame(ndvi_var_sf)
dfM <- transform(dfM, MeanNDVI.scaled = NDVI * 0.0001)
dfV <- transform(dfV, VarNDVI.scaled = NDVI * 0.0001)
Alldf <- cbind.data.frame(dfM,dfV)
df2 = subset(Alldf, select = c(2,5,10))

#############
results <- rbind(df2, df)
 results

################################################################
################################################################
################################################################
################################################################
################################################################
#set 2
Chlorothraupis_carmioli<- subset(Shapes, Shapes$species == "Chlorothraupis carmioli")
Chlorothraupis_olivacea<- subset(Shapes, Shapes$species == "Chlorothraupis olivacea")
Chlorothraupis_stolzmanni<- subset(Shapes, Shapes$species == "Chlorothraupis stolzmanni")
Cyanocompsa_parellina<- subset(Shapes, Shapes$species == "Cyanocompsa parellina")
Cyanoloxia_brissonii<- subset(Shapes, Shapes$species == "Cyanoloxia brissonii")
Cyanoloxia_cyanoides<- subset(Shapes, Shapes$species == "Cyanoloxia cyanoides")
Cyanoloxia_glaucocaerulea<- subset(Shapes, Shapes$species == "Cyanoloxia glaucocaerulea")
Cyanoloxia_rothschildii<- subset(Shapes, Shapes$species == "Cyanocompsa rothschildii")
#############################################################

##############################################################
a <- st_as_sf(Chlorothraupis_carmioli)
b <- st_as_sf(Chlorothraupis_olivacea)
c <- st_as_sf(Chlorothraupis_stolzmanni)
d <- st_as_sf(Cyanocompsa_parellina)
e <- st_as_sf(Cyanoloxia_brissonii)
f <- st_as_sf(Cyanoloxia_cyanoides)
g <- st_as_sf(Cyanoloxia_glaucocaerulea)
h <- st_as_sf(Cyanoloxia_rothschildii)

##########
ee_roi <- sf_as_ee(h)
Map$addLayer(ee_roi)

ndvi_mean_sf <- ee_extract(
  x = ndvi_composite,
  y = ee_roi,
  fun = ee$Reducer$mean(),
  scale = 2000,
  sf = TRUE)
ndvi_var_sf <- ee_extract(
  x = ndvi_composite,
  y = ee_roi,
  fun = ee$Reducer$variance(),
  scale = 2000,
  sf = TRUE)
# write csv for local, drop last colum with polygon descriptions
dfM <- data.frame(ndvi_mean_sf)
dfV <- data.frame(ndvi_var_sf)
dfM <- transform(dfM, MeanNDVI.scaled = NDVI * 0.0001)
dfV <- transform(dfV, VarNDVI.scaled = NDVI * 0.0001)
Alldf <- cbind.data.frame(dfM,dfV)
df = subset(Alldf, select = c(2,5,10))

results <- rbind(results, df)
results



#set 3
Granatellus_francescae<- subset(Shapes, Shapes$species == "Granatellus francescae")
Granatellus_paraensis<- subset(Shapes, Shapes$species == "Granatellus paraensis")
Granatellus_pelzelni<- subset(Shapes, Shapes$species == "Granatellus pelzelni")
Granatellus_sallaei<- subset(Shapes, Shapes$species == "Granatellus sallaei")
Granatellus_venustus<- subset(Shapes, Shapes$species == "Granatellus venustus")
Habia_atrimaxillaris<- subset(Shapes, Shapes$species == "Habia atrimaxillaris")
Habia_cristata<- subset(Shapes, Shapes$species == "Habia cristata")
Habia_fuscicauda<- subset(Shapes, Shapes$species == "Habia fuscicauda")
Habia_gutturalis<- subset(Shapes, Shapes$species == "Habia gutturalis")
Habia_rubica<- subset(Shapes, Shapes$species == "Habia rubica")
##
i <- st_as_sf(Chlorothraupis_carmioli)
j <- st_as_sf(Granatellus_francescae)# absent from ebird data
k <- st_as_sf(Granatellus_paraensis) # absent from ebird data
l <- st_as_sf(Granatellus_pelzelni)
m <- st_as_sf(Granatellus_sallaei)
n <- st_as_sf(Granatellus_venustus)
o <- st_as_sf(Habia_atrimaxillaris)
p <- st_as_sf(Habia_cristata)
q <- st_as_sf(Habia_fuscicauda)
r <- st_as_sf(Habia_rubica)

##########
ee_roi <- sf_as_ee(r)
Map$addLayer(ee_roi)

ndvi_mean_sf <- ee_extract(
  x = ndvi_composite,
  y = ee_roi,
  fun = ee$Reducer$mean(),
  scale = 2000,
  sf = TRUE)
ndvi_var_sf <- ee_extract(
  x = ndvi_composite,
  y = ee_roi,
  fun = ee$Reducer$variance(),
  scale = 2000,
  sf = TRUE)
# write csv for local, drop last colum with polygon descriptions
dfM <- data.frame(ndvi_mean_sf)
dfV <- data.frame(ndvi_var_sf)
dfM <- transform(dfM, MeanNDVI.scaled = NDVI * 0.0001)
dfV <- transform(dfV, VarNDVI.scaled = NDVI * 0.0001)
Alldf <- cbind.data.frame(dfM,dfV)
df = subset(Alldf, select = c(2,5,10))

results <- rbind(results, df)
results

#set 4

Passerina_amoena<- subset(Shapes, Shapes$species == "Passerina amoena")
Passerina_caerulea <- subset(Shapes, Shapes$species == "Passerina caerulea")
Passerina_ciris<- subset(Shapes, Shapes$species == "Passerina ciris")
Passerina_cyanea<- subset(Shapes, Shapes$species == "Passerina cyanea")
Passerina_leclancherii<- subset(Shapes, Shapes$species == "Passerina leclancherii")
Passerina_rositae<- subset(Shapes, Shapes$species == "Passerina rositae")
Passerina_versicolor<- subset(Shapes, Shapes$species == "Passerina versicolor")
Periporphyrus_erythromelas<- subset(Shapes, Shapes$species == "Periporphyrus erythromelas")

##
s <- st_as_sf(Passerina_amoena)
t <- st_as_sf(Passerina_caerulea) 
u <- st_as_sf(Passerina_ciris)
v <- st_as_sf(Passerina_cyanea)
w <- st_as_sf(Passerina_leclancherii)
x <- st_as_sf(Passerina_rositae)
y <- st_as_sf(Passerina_versicolor)
z <- st_as_sf(Periporphyrus_erythromelas)

##########
ee_roi <- sf_as_ee(z)
Map$addLayer(ee_roi)

ndvi_mean_sf <- ee_extract(
  x = ndvi_composite,
  y = ee_roi,
  fun = ee$Reducer$mean(),
  scale = 2000,
  sf = TRUE)
ndvi_var_sf <- ee_extract(
  x = ndvi_composite,
  y = ee_roi,
  fun = ee$Reducer$variance(),
  scale = 2000,
  sf = TRUE)
# write csv for local, drop last colum with polygon descriptions
dfM <- data.frame(ndvi_mean_sf)
dfV <- data.frame(ndvi_var_sf)
dfM <- transform(dfM, MeanNDVI.scaled = NDVI * 0.0001)
dfV <- transform(dfV, VarNDVI.scaled = NDVI * 0.0001)
Alldf <- cbind.data.frame(dfM,dfV)
df = subset(Alldf, select = c(2,5,10))

results <- rbind(results, df)
results

########################################
#set 5
Pheucticus_aureoventris<- subset(Shapes, Shapes$species == "Pheucticus aureoventris")
Pheucticus_chrysogaster<- subset(Shapes, Shapes$species == "Pheucticus chrysogaster")
Pheucticus_chrysopeplus<- subset(Shapes, Shapes$species == "Pheucticus chrysopeplus")
Pheucticus_ludovicianus<- subset(Shapes, Shapes$species == "Pheucticus ludovicianus")
Pheucticus_melanocephalus<- subset(Shapes, Shapes$species == "Pheucticus melanocephalus")
Pheucticus_tibialis<- subset(Shapes, Shapes$species == "Pheucticus tibialis")
Piranga_bidentata<- subset(Offical, Offical$SCINAME == "Piranga bidentata") #keep 


Piranga_erythrocephalaA<- subset(Shapes, Shapes$species == "Piranga erythrocephala")
Piranga_erythrocephalaB<- subset(Offical, Offical$SCINAME == "Piranga erythrocephala") #

Piranga_flava<- subset(Shapes, Shapes$species == "Piranga flava")
#######
a <- st_as_sf(Pheucticus_aureoventris)
b <- st_as_sf(Pheucticus_chrysogaster)
c <- st_as_sf(Pheucticus_chrysopeplus)
d <- st_as_sf(Pheucticus_ludovicianus)
e <- st_as_sf(Pheucticus_melanocephalus)
f <- st_as_sf(Pheucticus_tibialis)
g <- st_as_sf(Piranga_bidentata)

h <- st_as_sf(Piranga_erythrocephalaA)
b <- st_as_sf(Piranga_erythrocephalaB)

i <- st_as_sf(Piranga_flava)

######
ee_roi <- sf_as_ee(h)
Map$addLayer(ee_roi)

ndvi_mean_sf <- ee_extract(
  x = ndvi_composite,
  y = ee_roi,
  fun = ee$Reducer$mean(),
  scale = 2000,
  sf = TRUE)
ndvi_var_sf <- ee_extract(
  x = ndvi_composite,
  y = ee_roi,
  fun = ee$Reducer$variance(),
  scale = 2000,
  sf = TRUE)
# write csv for local, drop last colum with polygon descriptions
dfM <- data.frame(ndvi_mean_sf)
dfV <- data.frame(ndvi_var_sf)
dfM <- transform(dfM, MeanNDVI.scaled = NDVI * 0.0001)
dfV <- transform(dfV, VarNDVI.scaled = NDVI * 0.0001)
Alldf <- cbind.data.frame(dfM,dfV)
df = subset(Alldf, select = c(2,5,10))

results <- rbind(results, df)
results

#set 6
Piranga_hepaticaa<- subset(Shapes, Shapes$species == "Piranga hepatica") #keep 
Piranga_leucopteraB<- subset(Offical, Offical$SCINAME == "Piranga leucoptera") #kee[ ]


Piranga_ludoviciana<- subset(Shapes, Shapes$species == "Piranga ludoviciana")
Piranga_lutea<- subset(Shapes, Shapes$species == "Piranga lutea")
Piranga_olivaceaA<- subset(Shapes, Shapes$species == "Piranga olivacea")

Piranga_roseogularis<- subset(Offical, Offical$SCINAME == "Piranga roseogularis") #keep

Piranga_rubra<- subset(Shapes, Shapes$species == "Piranga rubra")
Piranga_rubriceps<- subset(Shapes, Shapes$species == "Piranga rubriceps")
Rhodothraupis_celaeno<- subset(Shapes, Shapes$species == "Rhodothraupis celaeno")
Spiza_americana<- subset(Shapes, Shapes$species == "Spiza americana")

a <- st_as_sf(Piranga_leucopteraA)
b <- st_as_sf(Piranga_leucopteraB)

ee_roi <- sf_as_ee(a)
Map$addLayer(ee_roi)

ee_roib <- sf_as_ee(b)
Map$addLayer(ee_roib)


c <- st_as_sf(Pheucticus_chrysopeplus)
d <- st_as_sf(Pheucticus_ludovicianus)
e <- st_as_sf(Pheucticus_melanocephalus)
f <- st_as_sf(Pheucticus_tibialis)
g <- st_as_sf(Piranga_bidentata)
h <- st_as_sf(Piranga_erythrocephala)
i <- st_as_sf(Piranga_flava)
