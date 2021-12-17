##### Merging shapefiles to proper range of GBIF DATA


setwd("C:/Users/bfsco/Desktop/Masters Research/Geospaital")
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

breeding_range <- read.csv("test.SEM/All_summer_range.csv")

species<- subset(breeding_range, breeding_range$species == "Passerina amoena")


species <- species %>% 
  rename(
    latitude = decimallatitude,
    longitude = decimallongitude)


########################

Amao <- readOGR("Official_range_maps/Amaurospiza_moesta.shp") #import shape files 
Offical <- readOGR("Official_range_maps/Card_maps.shp") #import shape files 
#Shapes <- readOGR("CardShapes.shp") #import shape files 
# modis_ndvi <- ee$ImageCollection("MODIS/006/MOD13A2") #original 

Piranga_hepatica <- readOGR("Official_range_maps/data_0.shp")


writeOGR(spp, layer = "Habia_fuscicauda-All", "Editted_shapefiles",driver="ESRI Shapefile")


off<- subset(Piranga_hepatica, row.names(Piranga_hepatica) == 5)

writeOGR(off, layer = "Piranga lutea", "Editted_shapefiles",driver="ESRI Shapefile")
#################################################################
libs <- c("rgdal", "maptools", "gridExtra")
lapply(libs, require, character.only = TRUE)

# I need two shape files, on for breeding range and one for entire range. 
# Need minimum convex polygons for these 
#################################################################
# For every species?

XXX<- subset(Offical, Offical$SCINAME == "Piranga hepatica")
bind <- rbind(XXX)
spp <-SpatialPolygonsDataFrame(bind,data=as.data.frame(XXX@data))


hopes <- aggregate(rbind.SpatialPolygonsDataFrame(XXX@polygons))
spp <-SpatialPolygonsDataFrame(hopes,data=as.data.frame("yourData"))
plot(spp)

writeOGR(spp, layer = "Habia_fuscicauda-All", "Editted_shapefiles",driver="ESRI Shapefile")
## Combined entire range 
##### Files that are huge
Passerina_amoena<- subset(Offical, Offical$SCINAME == "Passerina amoena")
Passerina_caerulea <- subset(Offical, Offical$SCINAME == "Passerina caerulea")
Passerina_ciris<- subset(Offical, Offical$SCINAME == "Passerina ciris")
Passerina_cyanea<- subset(Offical, Offical$SCINAME == "Passerina cyanea")
Passerina_versicolor<- subset(Offical, Offical$SCINAME == "Passerina versicolor")
####
Pheucticus_melanocephalus<- subset(Offical, Offical$SCINAME == "Pheucticus melanocephalus")
Pheucticus_ludovicianus<- subset(Offical, Offical$SCINAME == "Pheucticus ludovicianus")
Piranga_rubra<- subset(Offical, Offical$SCINAME == "Piranga rubra")
Piranga_ludoviciana<- subset(Offical, Offical$SCINAME == "Piranga ludoviciana")
Spiza_americana<- subset(Offical, Offical$SCINAME == "Spiza americana")
Piranga_olivacea<- subset(Offical, Offical$SCINAME ==  "Piranga olivacea")
Cardinalis_cardinalis<- subset(Offical, Offical$SCINAME ==  "Cardinalis cardinalis")
#######
hopes <- aggregate(rbind.SpatialPolygonsDataFrame(Cardinalis_cardinalis))
spp <-SpatialPolygonsDataFrame(hopes,data=as.data.frame("yourData"))
plot(spp)

writeOGR(Cardinalis_cardinalis, layer = "Cardinalis_cardinalis-All", "Editted_shapefiles",driver="ESRI Shapefile")

##########################################################
# Just breeding shapefiles 
Passerina_amoena<- subset(Offical, Offical$OBJECTID == "11278")
Passerina_caerulea <- subset(Offical, Offical$OBJECTID == "16870") ###
Passerina_ciris<- subset(Offical, Offical$OBJECTID == "11266")
Passerina_cyanea<- subset(Offical, Offical$OBJECTID == "16173")
Passerina_versicolor<- subset(Offical, Offical$OBJECTID == "16516")
####
Pheucticus_melanocephalus<- subset(Offical, Offical$OBJECTID == "11395")
Pheucticus_ludovicianus<- subset(Offical, Offical$OBJECTID == "16527")
Piranga_rubra<- subset(Offical, Offical$OBJECTID == "11573")
Piranga_ludoviciana<- subset(Offical, Offical$OBJECTID == "11568")
Spiza_americana<- subset(Offical, Offical$OBJECTID == "12269")
Piranga_olivacea<- subset(Offical, Offical$OBJECTID ==  "11581")
Cardinalis_cardinalis<- subset(Offical, Offical$OBJECTID ==  "16181")

16181

plot(Cardinalis_cardinalis)
# Generate IDs for grouping
oregon.id <- cut(Passerina_caerulea@data[,3], include.lowest=TRUE)
IDS <- Offical@data$SCINAME

breeding <- rbind(Passerina_amoena,Passerina_caerulea,Passerina_ciris,Passerina_cyanea,Passerina_versicolor,Pheucticus_melanocephalus,
            Pheucticus_ludovicianus,Piranga_rubra,Piranga_ludoviciana,Spiza_americana,Piranga_olivacea)



# Plotting
plot(oregon)
plot(oregon.union, add = TRUE, border = "red", lwd = 2)


test <- spatialjoin

#######
hopes <- aggregate(rbind.SpatialPolygonsDataFrame(Passerina_caerulea))
spp <-SpatialPolygonsDataFrame(hopes,data=as.data.frame("yourData"))
plot(spp)


writeOGR(breeding, layer = "All-breeding", "Editted_shapefiles",driver="ESRI Shapefile")


plot(Passerina_amoena)
plot(Passerina_caerulea)
plot(Passerina_ciris)
plot(Passerina_cyanea)
plot(Passerina_versicolor)
plot(Pheucticus_melanocephalus)
plot(Pheucticus_ludovicianus)
plot(Piranga_rubra)
plot(Piranga_ludoviciana)
plot(Spiza_americana)
plot(Piranga_olivacea)#

writeOGR(Passerina_amoena, layer = "Passerina_amoena-breeding", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Passerina_caerulea, layer = "Passerina_caerulea-breeding", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Passerina_ciris, layer = "Passerina_ciris-breeding", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Passerina_cyanea, layer = "Passerina_cyanea-breeding", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Passerina_versicolor, layer = "Passerina_versicolor-breeding", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Pheucticus_melanocephalus, layer = "Pheucticus_melanocephalus-breeding", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Pheucticus_ludovicianus, layer = "Pheucticus_ludovicianus-breeding", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Piranga_rubra, layer = "Piranga_rubra-breeding", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Piranga_ludoviciana , layer = "Piranga_ludoviciana-breeding", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Spiza_americana, layer = "Spiza_americana-breeding", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Piranga_olivacea, layer = "Piranga_olivacea-breeding", "Editted_shapefiles",driver="ESRI Shapefile")

##########################################################
# Just wintering shapefiles 

Passerina_amoena<- subset(Offical, Offical$OBJECTID == "11279")
Passerina_caerulea <- subset(Offical, Offical$OBJECTID == "16868") ###
Passerina_ciris<- subset(Offical, Offical$OBJECTID == "11265")
Passerina_cyanea<- subset(Offical, Offical$OBJECTID == "16508")
Passerina_versicolor<- subset(Offical, Offical$OBJECTID == "16518")
####
Pheucticus_melanocephalus<- subset(Offical, Offical$OBJECTID == "11397")
Pheucticus_ludovicianus<- subset(Offical, Offical$OBJECTID == "16525")
Piranga_rubra<- subset(Offical, Offical$OBJECTID == "11588")
Piranga_ludoviciana <- subset(Offical, Offical$OBJECTID == "11567")
Spiza_americana<- subset(Offical, Offical$OBJECTID == "12268")
Piranga_olivacea<- subset(Offical, Offical$OBJECTID ==  "11582")
#######

plot(Passerina_amoena)
plot(Passerina_caerulea)
plot(Passerina_ciris)
plot(Passerina_cyanea)
plot(Passerina_versicolor)
plot(Pheucticus_melanocephalus)
plot(Pheucticus_ludovicianus)
plot(Piranga_rubra)
plot(Piranga_ludoviciana)
plot(Spiza_americana)
plot(Piranga_olivacea)

Wintering <- rbind(Passerina_amoena,Passerina_caerulea,Passerina_ciris,Passerina_cyanea,Passerina_versicolor,Pheucticus_melanocephalus,
                  Pheucticus_ludovicianus,Piranga_rubra,Piranga_ludoviciana,Spiza_americana,Piranga_olivacea)
writeOGR(Wintering, layer = "All-wintering", "Editted_shapefiles",driver="ESRI Shapefile")


writeOGR(Passerina_amoena, layer = "Passerina_amoena-wintering", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Passerina_caerulea, layer = "Passerina_caerulea-wintering", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Passerina_ciris, layer = "Passerina_ciris-wintering", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Passerina_cyanea, layer = "Passerina_cyanea-wintering", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Passerina_versicolor, layer = "Passerina_versicolor-wintering", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Pheucticus_melanocephalus, layer = "Pheucticus_melanocephalus-wintering", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Pheucticus_ludovicianus, layer = "Pheucticus_ludovicianus-wintering", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Piranga_rubra, layer = "Piranga_rubra-wintering", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Piranga_ludoviciana , layer = "Piranga_ludoviciana-wintering", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Spiza_americana, layer = "Spiza_americana-wintering", "Editted_shapefiles",driver="ESRI Shapefile")
writeOGR(Piranga_olivacea, layer = "Piranga_olivacea-wintering", "Editted_shapefiles",driver="ESRI Shapefile")

##########################################################
