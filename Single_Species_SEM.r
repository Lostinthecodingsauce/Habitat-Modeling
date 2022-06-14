## This is a script to generate Ensamble species distribution models. It takes a previous spatially thinned dataset and generates a climate envolope for the species in question. 

setwd("C:/Users/bfsco/Desktop/Masters Research/Geospaital/test.SEM")

library("dismo")
library("maptools")
library("rgdal")
library("raster")
library("sp")
library(spatstat)
library(tidyverse)
library(dplyr)
library(SSDM)

### Import climate variables. If you have not done this yet, you will need to download a dataset from raster first
bioclimatic.data <- raster::getData(name = "worldclim",
                        var = "bio",
                        res = 5,
                        path = "wc10/")

Env <- load_var(system.file('extdata',  package = 'SSDM'), categorical = 'SUBSTRATE', verbose = FALSE)
Env

#####################
#launch graphic interface
#gui()
# Read in example species
species <- read.csv(file = "KDE/Cardinalis_sinuatus.csv")


# Rename lat-long
species <- species %>% 
  rename(
    latitude = Y,
    longitude = X)

# Create window for modeling extent. This is created by taking the max and mins for latitude and longitude
max.lat <- ceiling(max(species$latitude))
min.lat <- floor(min(species$latitude))
max.lon <- ceiling(max(species$longitude))
min.lon <- floor(min(species$longitude))
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

# Load the data to use for our base map
data(wrld_simpl)

# Plot the base map
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95",
     main = 'Species\nKDE thinned Records')

# Add the points for individual observation
points(x = species$longitude, 
       y = species$latitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)
# And draw a little box around the graph
box()

# Crop bioclim data to geographic extent of saguaro
bioclim.data <- crop(x = bioclimatic.data, y = geographic.extent)

# Reverse order of columns
#species <- species[, c("longitude", "latitude")]

# generate single ESM using different modeling parameters. This is using Maxent, but there are many other models
SDM <- modelling('MAXENT',  species,
                 bioclim.data, Xcol = 'longitude', Ycol = 'latitude', verbose = FALSE)
plot(SDM@projection, main = 'SDM\nfor Cardinalis_sinuatus\nwith MAXENT algorithm')

# Generate ESM using multiple alogirthums
ESDM <- ensemble_modelling(c('CTA', 'MARS'), species,
                           bioclim.data, rep = 1, Xcol = 'longitude', Ycol = 'latitude',
                           ensemble.thresh = 0, verbose = FALSE)
plot(ESDM@projection, main = 'ESDM\nfor Cardinalis_sinuatus\nwith CTA and MARS algorithms')


#save.esdm(ESDM, name = "Passerina_caerulea.csv",path = "test/KDE_SEMs/", verbose = TRUE, GUI = FALSE)
HighProp <- ESDM@projection >= .70
plot(HighProp,main = 'Binary ESDM\nfor Cardinalis_sinuatus\n70% Occurence Probability')

