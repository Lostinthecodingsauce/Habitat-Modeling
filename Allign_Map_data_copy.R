#### Allign raster and polygons layers
library(raster)
library("rgdal")
library("rgeos")
library(GISTools)
library(tidyverse)
library(phytools)
library(sp)
library(maps)
library(mapdata)
library(RStoolbox)
library(rgdal)
library(sf)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)
library(data.table)

# Great walkthrough for processing a single individual
# https://cmerow.github.io/YaleBGCCourses/111_SDM_Transfer.html
# https://rpubs.com/StatGirl302/PAClimateAnalysisUsingPCAandClustering

setwd("~/Desktop/Ben/Rangemaps")
rawmap <-readOGR("Card_maps.shp")
map <- readOGR("cleaned.shapefiles.qml")
Cardinal.tree <- read.nexus("Cardinalidae.nexus")

centroids <- read.csv("centroids.csv", header = TRUE)
rownames(centroids) <- centroids[,6]

#read in biolclimatic data
r <- getData("worldclim",var="bio",res=10)

#####
# Calculate centroids
centr <- gCentroid(maps, byid = TRUE)

# create SpatialPointsDataFrame to export via writeOGR
# positive side effect: All data from landuse@data joined to centr@data
centr <- SpatialPointsDataFrame(centr, data= map@data) 

writeOGR(centr, ".", "Cardinal_centroids", driver = "ESRI Shapefile")
####


#match projctions between map and raster data
projection(map) = projection(r)
projection(centr) = projection(r)
# Create small interation zone in this particular raster region
#yeet <- clip(map,r)
### Concatenated raster data with poloygon
#wow <- extract(fuck,yeet)
Cen <- rownames(centr$SCINAME)
names <- centr$SCINAME



bam<-raster::extract(r,centr)
### Need to drop Biocline 16 and 17, then run PCA
boot <- cbind(bam, names)
boot <- data.frame(bam)
row.names(boot) <- boot$names
boots = subset(boot, select = -c(names) )
wackman <- data.frame(df)
wackPCA <- princomp(df)



# Use list apply to calculate mean for each polygon
r.mean <- lapply(bam, FUN=mean)

# Join mean values to polygon data
sdata@data <- data.frame(sdata@data, m2012=r.mean)

# Write results
writeOGR(sdata, getwd(), outshp, driver="ESRI Shapefile", check_exists=TRUE,
         overwrite_layer=TRUE)

#############
climate.pca <- prcomp(aa)
summary(climate.pca)

#############

dat<-data.frame(dat)
str(dat)


#grab bioclim climate variables and add to dataset
w <- getData('worldclim', var='bio', res=2.5) # downloading data

dbio1 <- extract(w, dat[,5:6]) #grabbing bioclim variables based on coordinates
full.dat <- data.frame(cbind(dat, dbio1[,1:19]))

#look at range of lat and lon
range(full.dat[,5]) #lon range
## [1] -83.94955 -68.51740
range(full.dat[,6]) # lat range
## [1] 33.55605 44.98180
#designating limits
lims<-c(-85,-65,30,50)

submap<-crop(w,lims)
plot(submap,1)#plot MAT

map("worldHires",c("USA","Canada"),add=TRUE)








############ Below script is not working ##############

pcamap<-rasterPCA(aa,spca=TRUE)
#check loadings and eigenvalues
knitr::kable(round(pcamap$model$loadings[,1:3],3)) # top 3 loadings

# way to add spatial data into

dsn <- system.file("vectors", package = "rgdal")[1]
rgdal::readOGR(dsn,layer = "Card_maps.shp")
### Get species centroids of lat long

sids@data %>% remove_rownames %>% column_to_rownames(var="OBJECTID")

map$rownames <- as.character(row.names(nc_4267))

rgdal::readOGR()

sids <- readShapePoly(system.file("Card_maps.shp", package="maptools")[1],
                      proj4string=CRS("+proj=longlat +ellps=clrk66"))
class(sids)
plot(sids)
writeSpatialShape(sids, "sids")
cents <- coordinates(sids)
cents <- SpatialPointsDataFrame(coords=cents, data=sids@data,
                                proj4string=CRS("+proj=longlat +ellps=clrk66"))
points(cents, col = "Blue")
rgdal::writeOGR(cents, "cents")

writeSpatialShape(cents, "cents")

centroids <- getSpPPolygonsLabptSlots(sids)
points(centroids, pch = 3, col = "Red")



mydf   <- read.csv("centroids.csv")
myspdf <- readOGR("Card_maps.shp")





mask <- as(map, 'SpatialPolygons')

## variables to be used in the analysis
variables <- crop(r, mask)




# ranges on evironmental factor maps
ranges_emaps(ranges = map, variables = variables)

## mask variables to the region of interest
WGS84 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
w_map <- map(database = "world", regions = c("Ecuador", "Peru", "Bolivia", "Colombia","Mexico", "USA","Canada","Costa Rica","Panama","El Salvador", "Guyana", "French Guyana", "Brazil", "Argentina","Chile"),
             fill = TRUE, plot = FALSE) # map of the world

w_po <- sapply(strsplit(w_map$names, ":"), function(x) x[1]) # preparing data to create polygon
reg <- map2SpatialPolygons(w_map, IDs = w_po, proj4string = WGS84) # map to polygon

e <- extent(reg)
mask <- as(e, 'SpatialPolygons')

## variables to be used in the analysis
variables <- crop(vars, mask)

# ranges on evironmental factor maps
ranges_emaps(ranges = ranges, variables = variables)



## then merge using sp's merge function
mynewspdf <- merge(myspdf, mydf)

#### Example
# https://cran.r-project.org/web/packages/dismo/vignettes/sdm.pdf
# 4.2
presvals <- extract(predictors, bradypus)
 # setting random seed to always create the same
 # random set of points for this example
 set.seed(0)
 backgr <- randomPoints(predictors, 500)
 absvals <- extract(predictors, backgr)
 pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
 sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
 sdmdata[,'biome'] = as.factor(sdmdata[,'biome'])
 head(sdmdata)



#After querying species' climatic data, we bound the resulting files together
#and ran a single large correlation matrix PCA across all climate variables except bio7,
#which is simply the difference between bio5 and bio6. We retained species' scores along the different PC axes
#and used scores along the first two PC axes to calculate species-level hypervolumes in climate space.


 ## Plot midpoints onto map
 lat.long<-read.csv("midpoints.csv",row.names=1)
 tip <- c("Amaurospiza_concolor","Chlorothraupis_carmioli","Chlorothraupis_olivacea",
          "Chlorothraupis_stolzmanni","Cyanocompsa_brissonii","Cyanocompsa_cyanoides",
          "Cyanocompsa_parellina","Periporphyrus_erythromelas","Rhodothraupis_celaeno")
 Card.tree <- drop.tip(Cardinal.tree,tip)
 lat.long<-lat.long[Card.tree$tip.label, ]


 obj<-phylo.to.map(Cardinal.tree,lat.long,plot=FALSE)


 plot(obj,type="phylogram",asp=1.3,mar=c(0.1,0.5,3.1,0.1))













##### Importing large spatial data
raster <- raster("aaa.tif")
fuck<- raster("Fuck.tif")

# S4 method for Raster,SpatialLines
yes <- extract(raster, map)


plot(map)
plot(raster, add = TRUE)

#match projctions between map and raster data
projection(map) = projection(fuck)
# Create small interation zone in this particular raster region
yeet <- clip(map,fuck)
### Concatenated raster data with poloygon
wow <- extract(fuck,yeet)
