## SEMs forloop 
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

bioclimatic.data <- raster::getData(name = "worldclim",
                        var = "bio",
                        res = 5,
                        path = "wc10/")

Env <- load_var(system.file('extdata',  package = 'SSDM'), categorical = 'SUBSTRATE', verbose = FALSE)
Env
####################################################################

#####################
#launch graphic interface
#gui()
species <- read.csv(file = "KDE/Cardinalis_sinuatus.csv")
species <- read.csv(file = "KDE/Cyanocompsa_parellina.csv")




# Rename
#species <- species %>% 
#  rename(
#    latitude = decimallatitude,
#    longitude = decimallongitude)
# For KDE species (DO NOT SWITCH X and Y for these! )
species <- species %>% 
  rename(
    latitude = Y,
    longitude = X)

#species <- read.csv(file = "test/test.combined.csv")
# Determine geographic extent of our data
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


SDM <- modelling('MAXENT',  species,
                 bioclim.data, Xcol = 'longitude', Ycol = 'latitude', verbose = FALSE)
plot(SDM@projection, main = 'SDM\nfor Cardinalis_sinuatus\nwith MAXENT algorithm')

ESDM <- ensemble_modelling(c('CTA', 'MARS'), species,
                           bioclim.data, rep = 1, Xcol = 'longitude', Ycol = 'latitude',
                           ensemble.thresh = 0, verbose = FALSE)
plot(ESDM@projection, main = 'ESDM\nfor Cardinalis_sinuatus\nwith CTA and MARS algorithms')


#save.esdm(ESDM, name = "Passerina_caerulea.csv",path = "test/KDE_SEMs/", verbose = TRUE, GUI = FALSE)
HighProp <- ESDM@projection >= .70
plot(HighProp,main = 'Binary ESDM\nfor Cardinalis_sinuatus\n70% Occurence Probability')
############################################

# test elevation
Env <- load_var(system.file('extdata',  package = 'SSDM'), categorical = 'SUBSTRATE', verbose = FALSE)
Env

#####################################################
   #### Exploration ###
#######################################################

#Reload one file
spp <- load_esdm("Piranga_lutea.csv", path = "../KDE_55_SEMs/")
plot(spp@projection, main = 'ESDM\nfor test\nwith CTA and MARS algorithms')
# Filter data set to only include probability above .7
HighProp <- spp@projection >= .585
plot(HighProp)

polys1 = rasterToPolygons(HighProp, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
polys1 <- subset(polys1, polys1$layer == "1")
plot(polys1)

cols = rev(terrain.colors(255))
spplot(polys1, "layer", col.regions=cols, lwd=0)

writeOGR(polys1, layer = "Piranga_lutea.csv", "results",driver="ESRI Shapefile")


# Comparing KDE vs regu SEMs
spp <- load_esdm("Cardinalis sinuatus",path = "Shapefiles.SEMs/")
spp.kde <- load_esdm("Cardinalis_sinuatus.KDE",path = "KDE_SEMs/")

plot(spp@projection, main = 'ESDM\nfor Passerina_ciris_reg\nwith CTA and MARS algorithms')
plot(spp.kde@projection, main = 'ESDM\nfor Passerina_ciris_KDE\nwith CTA and MARS algorithms')


##############################################################################
###############################################################################

# For loop - Generate SEMs
setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital/test.SEM/test")

filenames <- list.files(path = getwd(), pattern = "*csv")
numfiles <- length(filenames)


for (i in (1:numfiles)){
  print(filenames[i])
  df <- read.csv(filenames[i], header = TRUE)
  #df <- df %>% 
  #  rename(
  #    latitude = decimallatitude,
  #    longitude = decimallongitude)
  df <- df %>% 
    rename(
      latitude = Y,
      longitude = X)
 # df <- df[, c("longitude", "latitude")] # turn off for KDE species 
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
  save.esdm(ESDM, name = file_name,path = "KDE_SEMs/", verbose = TRUE, GUI = FALSE)
}
#################
# For loop to load SEMs, filter them by >.7, and export them as shapefile
# For loop 
setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital/test.SEM/test/KDE_55_SEMs")

filenames <- list.files(path = getwd())
numfiles <- length(filenames)
cols = rev(terrain.colors(255))
names <- as.data.frame(filenames)

for (i in (1:numfiles)){
  print(filenames[i])
  spp <- load_esdm(filenames[i])
  plot(spp@projection, main = filenames[i])
  HighProp <- spp@projection >= .55 # Filter data set to only include probability above .7
  plot(HighProp)
  poly = rasterToPolygons(HighProp, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
  polys1 <- subset(poly, poly$layer == "1")
  Pero <- subset.data.frame(names, names$filenames == filenames[i])
  polys1@data$species <- Pero$filenames
  spplot(polys1, "layer", col.regions=cols, lwd=0, main = filenames[i])
  file_name <- paste(gsub(" ","_",filenames[[i]]), sep="")
  writeOGR(polys1, layer = file_name, "../Filtered-55",driver="ESRI Shapefile")
}
###############################################

# Comparing KDE vs regu SEMs
spp <- load_esdm("Cardinalis sinuatus",path = "Shapefiles.SEMs/")
spp.kde <- load_esdm("Cardinalis_sinuatus.KDE",path = "KDE_SEMs/")

plot(spp@projection, main = 'ESDM\nfor Passerina_ciris_reg\nwith CTA and MARS algorithms')
plot(spp.kde@projection, main = 'ESDM\nfor Passerina_ciris_KDE\nwith CTA and MARS algorithms')



# Problem Children- too small ~ reran for loop with only these species using 55 filter
"Amaurospiza_concolor.csv"
"Amaurospiza_moesta.csv"
Caryothraustes_canadensis
"Chlorothraupis_carmioli.csv"
"Chlorothraupis_stolzmanni.csv"
"Chlorothraupis_olivacea.csv"
"Cyanocompsa_rothschildii.csv"
"Granatellus_pelzelni.csv"
"Piranga_erythrocephala.csv"
"Piranga_hepatica.csv"
"Piranga_lutea.csv"
"Piranga_roseogularis.csv"
"Piranga_rubriceps.csv"
 "Rhodothraupis_celaeno.csv"

# Problem Children- too big ~ reran for loop with only these species using 85 filter
"Cyanocompsa_brissonii.csv"
"Piranga_rubra.KDE"
"Passerina_amoena.KDE"
"Passerina_ciris.KDE"
"Passerina_cyanea.KDE"
"Pheucticus_melanocephalus.KDE"

#################### For loop 3 ##################3

#Combine all filtered shapesiles into one file
setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital/test.SEM/test/AllShapefiles")

file_list <- list.files(path = getwd(), pattern = "*shp")

library(sf)
shapefile_list <- lapply(file_list, read_sf)

test <- rbind.SpatialPolygonsDataFrame(shapefile_list)

writeOGR(shapefile_list, layer = "All-shapes", "Editted_shapefiles",driver="ESRI Shapefile")

#writeRaster(test,'TIFFs/test.tif',options=c('TFW=YES'))





#########################################################################

library(EcoGenetics)



misc.parse.filter(data, "filter == 1", filter, byrow = TRUE)
####################################################################

