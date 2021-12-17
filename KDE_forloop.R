## For loop for generating species kernel densities based on GBIF records 
#### Hypervolumes 

### creating 2-D polygons from species occurance
##### GBIF DATA for Cardinaldiae
setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital")
names <- read.csv("Cardinalidae-names.csv")
species <- names$Species
# GBIF info
user <- "peucedramus95" # your gbif.org username
pwd <- "Birdzmaps69" # your gbif.org password
email <- "bscott9441@sdsu.edu" # your email

### I have used so many different packages I have lost which ones I need and which ones I dont.
library(speciesgeocodeR)
library(exactextractr)
library(hyperoverlap)
library(KernSmooth)
library(sf)
library(raster)
library(readr)
library(countrycode)
library(rgbif)
library(rgeos)
library(dplyr)
library(spatstat)
library(rgdal)
library(maps)
library(corrplot)
library(qgraph)
library(psych)
library("rgeos")
library(GISTools)
library(tidyverse)
library(sp)
library(mapdata)
library(spData)
library(data.table)
library(proj4)
library(RStoolbox)
library(spDataLarge)
library(adehabitatHR)
library(CoordinateCleaner)
library(taxize)
library(ggplot2)
library(vegan)
library(gridExtra)
library(corrplot)
library(qgraph)
library(psych)
library(dismo)
library(data.table) ## for BigPCA to file
library(bigmemory)## for BigPCA to file
library(bigpca)## for BigPCA to file
########################################################33
library(ebirdst)
library(raster)
library(sf)
library(smoothr)
library(rnaturalearth)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
require(spatstat)
require(tidyverse)
require(raster)
library(tmap)
library(adehabitatHR)
library(spThin)
library(ALA4R)

######################
# Read in data 
###################################
#cleaned.gbif <- read.csv("final.cleaned.gbif.csv") # Cleaned Cardinal REcords
df <- read.csv("cleaned.records.csv")
# Remove any saltators 
df <- df %>%
  filter(genus !="Saltator")
source("cleaning_gbif.R") #Dillions code for cleaning records/
DATA <- df # have two datasets, one that has corrdinate pursession and one raw

coordinates(df) <- c("decimallongitude","decimallatitude") #make dataset spatial dataset
crs.geo <- CRS("+proj=longlat +ellps+WGS84 +datum=WGS84") #Add coordiantes 
proj4string(df) <- crs.geo #assign te coordiante system
Shapes <- readOGR("CardShapes.shp") #import shape files

setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital/Official range maps")
Shapes <- readOGR("Card_maps.shp") #import shape files 
##############################################################################

# Combine all shapefiles into one file to make a mask 
hopes <- aggregate(rbind.SpatialPolygonsDataFrame(Shapes))
mask <- raster(hopes)
#####################################
names(output_df) <- names(df)
output_df$species <- names
###################################################
### Raster forlooop ##################
###################################################

####################################################################################
####################################################################################
# 1) Calculate hypervolumes
#### Start, part 1
West <- dat %>% 
  filter(species %in% c("Chlorothraupis olivacea","Granatellus pelzelni")) 
 # filter(species == "Granatellus pelzelni")            


West <- dat %>% 
  filter(species == "Granatellus pelzelni")
Grana <- West[c(4,5,13)]
All <- dat[c(4,5,13)]

#Subset 
coordinates(Grana) <- c("decimallongitude","decimallatitude") #make dataset spatial dataset
crs.geo <- CRS("+proj=longlat +ellps+WGS84 +datum=WGS84") #Add coordiantes
proj4string(Grana) <- crs.geo #assign te coordiante system
str(Grana)
# All individuals 

#dat <- dat %>% group_by(species) %>% filter(n()>= 20) %>% ungroup()


#Create window 
kernel.ref <- kernelUD(df, h = "href", grid = 500, extent= .5)
image(kernel.ref)

kernel.poly <- getverticeshr(kernel.ref, percent = 95) 
plot(getverticeshr(kernel.ref, percent = 75), add = TRUE, col="red")
print(kernel.poly)

# Get volume
vud <- getvolumeUD(kernel.ref)

#image(kernel.ref, get)
title(main="grid=500, extent=0.5")
kernel.ref[[1]]@h
kernel.area(kernel.ref, percent = c(50,75,90),unout = "km2")

# Get volume
vud <- getvolumeUD(kernel.ref)
image(vud)
plot(vud[[1]]@coords, add = TRUE)
title(main="Volume")


r <- raster(as(kernel.ref[[1]], "SpatialPixelsDataFrame"))
proj4string(r) <- crs.geo
plot(r)

wack <- writeRaster(r, "kernel.ref.tif")
#writeRaster(r, filename = file.path(tempdir(), "kernel.ref.tif"))


# Plot points 
data(wrld_simpl)
# Plot the base map
win <- extent(matrix(c(df$decimallongitude,df$decimallatitude), nrow = nrow(df)))
win <- data.frame(c(win@xmin, win@xmax),
                  c(win@ymin, win@ymax))
win <- owin(c(win[1,1],win[2,1]),c(win[1,2],win[2,2]))


wack <-plot(wrld_simpl, 
            xlim = win$xrange,
            ylim = win$yrange,
            col = "grey95")
#plot(kernel.ref[[1]], add = TRUE)
#plot(Grana.kernel.poly)
points(x = df$decimallongitude, y = df$decimallatitude,
       col = "olivedrab", 
       pch = 20, 
       cex = 1)

sub_mask <- subset(Shapes,species == "Granatellus pelzelni")
rast <- raster::raster(ext = raster::extent(sub_mask), res = 1)



####################################################################################
####################################################################################

# Map for KDE with cant be used for anything
#######################################################
kernel.ref <- kernelUD(df, h = "href", grid = 500) #extent = win
plot(kernel.ref)
kde <- raster(kernel.ref) #converts to raster
projection(kde) <- CRS("+proj=longlat +ellps+WGS84 +datum=WGS84") #Set prjection to global(?)
library(tmap)
#map the taster in tmap, "___" is density variable 
tm_shape(kde) + tm_raster("ud")

### Bounding 
# use polygons generated from species occurences 
#bounding <- hopes@bbox
bounding <- mapsWest@bbox
#Maps the raste within the bounding box
tm_shape(kde, bbox = bounding) + tm_raster("ud")

# mask the raster by the output area polygon
masked_kde <- mask(kde, mapsWest)
# maps the masked raster, also maps white output area boundries
tm_shape(masked_kde, bbox = bounding) + tm_raster("ud", style = "quantile", n = 50, palette = "YlGnBu") + tm_shape(mapsWest) + tm_borders(alpha=.2, col = "white")
##
# make a brick
brick <- brick(masked_kde, sqrt(masked_kde))
plot(brick)
#write tiff
jkjk <- writeRaster(brick, filename = "tanager4.tif", format ="GTiff")
writeRaster(masked_kde, filename = "tanager.tif", format ="GTiff")
#####
pw.all.rast <- (raster(as(brick, "SpatialPixelsDataFrame")))
plot(pw.all.rast)
writeRaster(pw.all.rast, filename = "tan.tif", format ="GTiff")
################# Other species

######
Cap <- West[c(6,7)]
# Need spatialPIXELSdataframe, not spatialpoints 
coordinates(coords) <- c("West_ppp.x","West_ppp.y")
coordinates(Cap) <- c("decimallongitude","decimallatitude") 
####
#grid
whelp <- extent(matrix(c(Rhodo$decimallongitude,Rhodo$decimallatitude), nrow = nrow(Rhodo)))
xmin <- whelp@xmin
xmax <- whelp@xmax
ymin <- whelp@ymin
ymax <- whelp@ymax
xGrid <- seq(xmin,xmax, by = 1)
yGrid <- seq(ymin,ymax, by = 1)
xGrid <- seq(-101,-96, by = 1)
yGrid <- seq(19,28, by = 1)
xyGRID <- expand.grid(xGrid=xGrid,yGrid=yGrid)
coordinates(xyGRID) <- ~xGrid+yGrid
gridded(whelp) <- TRUE
######  

###################################################################
###################################################################

################################################
############# For loop##########################
################################################
DUF <- DATA[,c("gbifID","basisOfRecord","decimallatitude","decimallongitude",
               "scientificName",
               "class","order","family","genus","species","month","year")]



filenames <- species
numfiles <- length(filenames)

output_df <- data.frame()#
############
Cardinals <- c("Amaurospiza carrizalensis","Amaurospiza concolor ","Amaurospiza moesta","Cardinalis cardinalis ","Cardinalis phoeniceus","Cardinalis sinuatus","Caryothraustes canadensis ","Caryothraustes poliogaster","Chlorothraupis carmioli frenata","Chlorothraupis olivacea","Chlorothraupis stolzmanni","Cyanocompsa parellina ","Cyanoloxia brissonii","Cyanoloxia cyanoides","Cyanoloxia glaucocaerulea","Cyanoloxia rothschildii","Granatellus paraensis","Granatellus pelzelni","Granatellus sallaei","Granatellus venustus ","Granatellus francescae","Habia atrimaxillaris","Habia cristata","Habia fuscicauda ","Habia gutturalis","Habia rubica affinis ","Habia rubica bahiae ","Habia rubica peruviana ","Passerina amoena","Passerina caerulea","Passerina ciris","Passerina cyanea","Passerina leclancherii","Passerina rositae","Passerina versicolor","Periporphyrus erythromelas","Pheucticus aureoventris","Pheucticus chrysogaster","Pheucticus chrysopeplus ","Pheucticus ludovicianus ","Pheucticus melanocephalus","Pheucticus tibialis","Rhodothraupis celaeno","Spiza americana","Piranga rubriceps","Piranga rubra","Piranga roseogularis","Piranga olivacea","Piranga lutea","Piranga ludoviciana","Piranga leucoptera","Piranga hepatica","Piranga flava","Piranga erythrocephala","Piranga bidentata")

############################
for (i in 1:length(filenames)){
  
  df_sub <- subset(DUF,DUF$species == filenames[i])

  for (c in 1:ncol(df_sub)){
    
    write.csv(df_sub[c])
    paste(df_sub[c],".csv",sep = "")
  }
}
names(output_df) <- names(df)
output_df$species <- names



for (i in df){
  filter == "species"
  list.files()
  filename <- paste("", ".csv", sep="")
}
df %>%
  select(species)


################### KDe forloop ##############
filenames <- list.files(path = getwd(), pattern = "*csv")
numfiles <- length(filenames)
library(tmap)
dataset = list()

for (i in c(1:numfiles)){
  tryCatch({
    print(filenames[i])
    refs <- read.csv(filenames[i], header = TRUE)
    Cap <- refs[c(4,3)]
    #Cap <- Capa[c(-1)]
    coordinates(Cap) <- c("decimallongitude","decimallatitude") 
    huf <- kernelUD(Cap, h = "href", grid = 500) #extent = win
    plot(huf)
    kde <- raster(huf) #converts to raster
    projection(kde) <- CRS("+proj=longlat +ellps+WGS84 +datum=WGS84") #Set prjection to global(?)
    #map the taster in tmap, "___" is density variable 
    tm_shape(kde) + tm_raster("ud")
    # use polygons generated from species occurences 
    mapRefs <- CalcRange(refs, method = "pseudospherical", terrestrial = T,rare = "buffer", buffer.width = 10000)
    
    #bounding <- hopes@bbox
    bounding <- mapRefs@bbox
    
    #Maps the raste within the bounding box
    tm_shape(kde, bbox = bounding) + tm_raster("ud")
    
    # mask the raster by the output area polygon
    masked_kde <- mask(kde, mapPass)
    
    # maps the masked raster, also maps white output area boundries
    tm_shape(masked_kde, bbox = bounding) + tm_raster("ud", style = "quantile", n = 50, palette = "YlGnBu") + tm_shape(mapRefs) + tm_borders(alpha=.3, col = "white")
  
    # make a brick
   # brick <- brick(masked_kde, sqrt(masked_kde))
   # plot(brick)
    
    #write tiff
   # filename <- paste("test/", card, "_output.tif", sep="")
  #  writeRaster(brick, filename = filename, format ="GTiff")
  })
}
