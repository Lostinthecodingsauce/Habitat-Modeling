#Clip migratory species to breeding range, generate SEMS for species 

setwd("C:/Users/bfsco/Desktop/Masters Research/Geospaital/test.SEM")
library(rgdal)
library(dplyr)
library(raster)
library(sf)
library(smoothr)
library(rnaturalearth)
library(tidyr)
library(stringr)
library(ggplot2)
require(spatstat)
require(tidyverse)
library("dismo")
library("maptools")
library("rgdal")
library("sp")
library(spatstat)
library(tidyverse)
library(dplyr)
library(SSDM)

bioclimatic.data <- raster::getData(name = "worldclim",
                                    var = "bio",
                                    res = 5,
                                    path = "wc10/")

breeding_range <- read.csv("All_summer_range.csv")


Offical <- readOGR("../Official_range_maps/Card_maps.shp") #import shape files 
# Just breeding shapefiles 
Passerina_amoena<- subset(Offical, Offical$OBJECTID == "11278")###
Passerina_ciris<- subset(Offical, Offical$OBJECTID == "11266") ###
Passerina_cyanea<- subset(Offical, Offical$OBJECTID == "16173")
Passerina_versicolor<- subset(Offical, Offical$OBJECTID == "16516")

Passerina_caerulea <- subset(Offical, Offical$OBJECTID == "16870") 
####
Pheucticus_melanocephalus<- subset(Offical, Offical$OBJECTID == "11395")
Pheucticus_ludovicianus<- subset(Offical, Offical$OBJECTID == "16527")
Piranga_rubra<- subset(Offical, Offical$OBJECTID == "11573")
Piranga_ludoviciana<- subset(Offical, Offical$OBJECTID == "11568")
Spiza_americana<- subset(Offical, Offical$OBJECTID == "12269")
Piranga_olivacea<- subset(Offical, Offical$OBJECTID ==  "11581")
Cardinalis_cardinalis<- subset(Offical, Offical$OBJECTID ==  "16181")


##################3
species<- subset(breeding_range, breeding_range$species == "Cardinalis cardinalis")
species <- species %>% 
  rename(
    latitude = decimallatitude,
    longitude = decimallongitude)


coordinates(species) <- c("longitude","latitude") #make dataset spatial dataset
crs.geo <- CRS("+proj=longlat +ellps+WGS84 +datum=WGS84") #Add coordiantes 
proj4string(species) <- crs.geo #assign te coordiante system


stations_subset <- species[Cardinalis_cardinalis, ]
plot(Cardinalis_cardinalis)
plot(stations_subset, add = TRUE)
DF <- as.data.frame(stations_subset)
#DF <- DF[c(4:7)]
write.csv(DF, file = "NewBreeding/Cardinalis_cardinalis_breeding.csv")

####################################
Cyanoloxia_brissonii
Cyanoloxia_cyanoides
Cyanoloxia_glaucocaerulea

#species <- read.csv(file = "KDE/Cyanoloxia_glaucocaerulea.csv")
DF <- species %>% 
  rename(
  latitude = Y, 
   longitude = X) 

DF <- read.csv(file = "NewBreeding/Cardinalis_cardinalis_breeding.csv")

# Determine geographic extent of our data
max.lat <- ceiling(max(DF$latitude))
min.lat <- floor(min(DF$latitude))
max.lon <- ceiling(max(DF$longitude))
min.lon <- floor(min(DF$longitude))
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

# Load the data to use for our base map
data(wrld_simpl)

# Plot the base map
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95",
     main = 'DF\nKDE thinned Records')

# Add the points for individual observation
points(x = DF$longitude,
       y = DF$latitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)
# And draw a little box around the graph
box()

# Crop bioclim data to geographic extent of saguaro
bioclim.data <- crop(x = bioclimatic.data, y = geographic.extent)

# Reverse order of columns
#DF <- DF[, c("longitude", "latitude")]


#SDM <- modelling('MAXENT',  DF,
     #            bioclim.data, Xcol = 'longitude', Ycol = 'latitude', verbose = FALSE)
#plot(SDM@projection, main = 'SDM\nfor Cardinalis_sinuatus\nwith MAXENT algorithm')

ESDM <- ensemble_modelling(c('CTA', 'MARS'), DF,
                           bioclim.data, rep = 1, Xcol = 'longitude', Ycol = 'latitude',
                           ensemble.thresh = 0, verbose = FALSE)
plot(ESDM@projection, main = 'ESDM\nfor Cardinalis_cardinalis\nwith CTA and MARS algorithms')


#save.esdm(ESDM, name = "Passerina_caerulea.csv",path = "test/KDE_SEMs/breedingSEMs", verbose = TRUE, GUI = FALSE)
HighProp <- ESDM@projection >= .7
plot(HighProp,main = 'Binary ESDM\nfor Cardinalis_cardinalis\n70% Occurence Probability')
save.esdm(ESDM, name = "Cardinalis_cardinalis",path = "test/KDE_SEMs/", verbose = TRUE, GUI = FALSE)
#save.esdm(ESDM, name = "Cyanoloxia_brissonii",path = "test/Breeding/", verbose = TRUE, GUI = FALSE)

polys1 = rasterToPolygons(HighProp, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
polys1 <- subset(polys1, polys1$layer == "1")
plot(polys1)

cols = rev(terrain.colors(255))
spplot(polys1, "layer", col.regions=cols, lwd=0)

writeOGR(polys1, layer = "Cardinalis_cardinalis", "results",driver="ESRI Shapefile")

############################################



# For loop - Generate SEMs
setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital/test.SEM/NewBreeding")
bioclimatic.data <- raster::getData(name = "worldclim",
                                    var = "bio",
                                    res = 5,
                                    path = "../wc10/")

filenames <- list.files(path = getwd(), pattern = "*csv")
numfiles <- length(filenames)


for (i in (1:numfiles)){
  print(filenames[i])
  df <- read.csv(filenames[i], header = TRUE)
  max.lat <- ceiling(max(df$latitude))
  min.lat <- floor(min(df$latitude))
  max.lon <- ceiling(max(df$longitude))
  min.lon <- floor(min(df$longitude))
  geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))
  bioclim.data <- crop(x = bioclimatic.data, y = geographic.extent)
  ESDM <- ensemble_modelling(c('CTA', 'MARS'), df,
                             bioclim.data, rep = 1, Xcol = 'longitude', Ycol = 'latitude',
                             ensemble.thresh = 0, verbose = FALSE)
  plot(ESDM@projection, main = 'ESDM\nfor test\nwith CTA and MARS algorithms')
  file_name <- paste(gsub(" ","_",filenames[[i]]), sep="")
  save.esdm(ESDM, name = file_name,path = "../test/Breeding/", verbose = TRUE, GUI = FALSE)
}

#################
# For loop to load SEMs, filter them by >.7, and export them as shapefile
# For loop 
setwd("C:/Users/bfsco/Desktop/Masters Research/Geospaital/test.SEM/test/Breeding")
spp <- load_esdm("Pheucticus_ludovicianus_breeding.csv")
plot(spp@projection, main = 'ESDM\nfor Pheucticus_ludovicianus\nwith CTA and MARS algorithms')


filenames <- list.files(path = getwd())
numfiles <- length(filenames)
cols = rev(terrain.colors(255))
names <- as.data.frame(filenames)

for (i in (1:numfiles)){
  print(filenames[i])
  spp <- load_esdm(filenames[i])
  plot(spp@projection, main = filenames[i])
  HighProp <- spp@projection >= .70 # Filter data set to only include probability above .7
  plot(HighProp)
  poly = rasterToPolygons(HighProp, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
  polys1 <- subset(poly, poly$layer == "1")
  Pero <- subset.data.frame(names, names$filenames == filenames[i])
  polys1@data$species <- Pero$filenames
  spplot(polys1, "layer", col.regions=cols, lwd=0, main = filenames[i])
  file_name <- paste(gsub(" ","_",filenames[[i]]), sep="")
  writeOGR(polys1, layer = file_name, "../Filtered-breeding",driver="ESRI Shapefile")
}
##################

# Climate Hypervolume centroids from breeding ranges
setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital/species.occurences")
################### KDe forloop ##############
filenames <- list.files(path = getwd(), pattern = "*csv")
numfiles <- length(filenames)


crs.geo <- CRS("+proj=longlat +ellps+WGS84 +datum=WGS84") #Add coordiantes 
datalist = list()

for (i in (1:numfiles)){
  print(filenames[i])
  df <- read.csv(filenames[i], header = TRUE)
  #coordinates(df) <- c("decimallongitude","decimallatitude") #make dataset spatial dataset
  # proj4string(df) <- crs.geo #assign te coordiante system
  df.wrs = hypervolume(data=df[,1:2],method='box')
  cen <- get_centroid(df.wrs)
  centroid <- data.frame(cen)
  # dat <- data.frame(df.wrs)
  file_name <- paste("Centroids/", gsub(" ","_",filenames[[i]]), sep="")
  write.csv(centroid,file = file_name)
  datalist[[i]] <- centroid}

# create one dataframe from results 
data <- do.call(rbind, datalist)












#Combine all filtered shapesiles into one file
setwd("C:/Users/bfsco/Desktop/Masters Research/Geospaital/test.SEM/test/Filtered-breeding")

file_list <- list.files(path = getwd(), pattern = "*shp")

library(sf)
shapefile_list <- lapply(file_list, read_sf)


test <- sf::rbind.SpatialPolygonsDataFrame(shapefile_list)

breeding <- rbind(Passerina_amoena,Passerina_caerulea,Passerina_ciris,Passerina_cyanea,Passerina_versicolor,Pheucticus_melanocephalus,
                  Pheucticus_ludovicianus,Piranga_rubra,Piranga_ludoviciana,Spiza_americana,Piranga_olivacea)

a <- readOGR("Passerina_amoena_breeding.csv.shp") #import shape files 
plot(a)
cen <- st_centroid(a)
Passerina_versicolor_breeding.csv

writeOGR(Wintering, layer = "All-wintering", "Editted_shapefiles",driver="ESRI Shapefile")


writeOGR(shapefile_list, layer = "All-shapes", "Editted_shapefiles",driver="ESRI Shapefile")

