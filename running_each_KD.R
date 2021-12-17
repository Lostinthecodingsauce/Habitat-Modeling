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
DATA$decimallatitude <- as.numeric(DATA$decimallatitude)
DATA$decimallongitude <- as.numeric(DATA$decimallongitude)
#designating limits
lims<-c(-150,0,-35,45)
# Crop based on limits
dat <- DATA%>%
  filter(decimallongitude > -155, decimallongitude < -40) # remove records from outside range
dat <- dat%>%
  filter(decimallatitude > -45, decimallatitude < 75) # remove records from outside range

do <- dat[-c(1,2,3,6,7,8,9,10,11,12,14,15)]
# This is a function to write a csv file for each species
#by(do, do$species, FUN=function(i) write.csv(i, paste0(i$species[1], ".csv")))

coordinates(df) <- c("decimallongitude","decimallatitude") #make dataset spatial dataset
crs.geo <- CRS("+proj=longlat +ellps+WGS84 +datum=WGS84") #Add coordiantes 
proj4string(df) <- crs.geo #assign te coordiante system
Shapes <- readOGR("CardShapes.shp") #import shape files 
##############################################################################

# Combine all shapefiles into one file to make a mask 
hopes <- aggregate(rbind.SpatialPolygonsDataFrame(Shapes))
mask <- raster(hopes)
#####################################



by(dat, dat$species, FUN=function(i) write.csv(i, paste0(i$species[1], ".csv")))

####################################################################################
####################################################################################
# 1) Calculate hypervolumes
#### Start, part 1
Grana <- dat %>% 
  filter(species %in% c("Granatellus paraensis","Granatellus pelzelni","Granatellus sallaei","Granatellus venustus","Granatellus francescae")) 
# filter(species == "Granatellus pelzelni")            
Grana <- Grana[c(4,5,13)]

#### Start, part 1
Grana <- dat %>% 
  filter(species %in% c("Granatellus paraensis","Granatellus pelzelni","Granatellus sallaei","Granatellus venustus","Granatellus francescae")) 
# filter(species == "Granatellus pelzelni")            
Grana <- Grana[c(4,5,13)]


#Subset 
coordinates(Grana) <- c("decimallongitude","decimallatitude") #make dataset spatial dataset
crs.geo <- CRS("+proj=longlat +ellps+WGS84 +datum=WGS84") #Add coordiantes
proj4string(Grana) <- crs.geo #assign te coordiante system
str(Grana)

#Create window 
kernel.grana <- kernelUD(Grana, h = "href", grid = 500, extent= 1)
image(kernel.grana)

Grana.kernel.poly <- getverticeshr(kernel.grana, percent = 75) 
plot(getverticeshr(kernel.grana, percent = 75), add = TRUE, col="red")
print(Grana.kernel.poly)
# calculate cenbtroid of polygon 
trueCentroids = gCentroid(Grana.kernel.poly,byid=TRUE)
trueCentroids = gCentroid(a,byid=TRUE)
plot(trueCentroids, add = TRUE)

# Get volume
vud <- getvolumeUD(kernel.grana)
image(vud)
plot(vud[[1]]@coords, add = TRUE)
title(main="Volume")


R <- raster(as(kernel.grana[[1]], "SpatialPixelsDataFrame"))
proj4string(R) <- crs.geo
plot(R)

wack <- writeRaster(r, "kernel.ref.tif")
# Plot points 
data(wrld_simpl)
# Plot the base map
win <- extent(matrix(c(Grana$decimallongitude,Grana$decimallatitude), nrow = nrow(Grana)))
win <- data.frame(c(win@xmin, win@xmax),
                  c(win@ymin, win@ymax))
win <- owin(c(win[1,1],win[2,1]),c(win[1,2],win[2,2]))


wack <-plot(wrld_simpl, 
            xlim = win$xrange,
            ylim = win$yrange,
            col = "grey95")
plot(kernel.grana[[1]], add = TRUE)



#Subset 
coordinates(Grana) <- c("decimallongitude","decimallatitude") #make dataset spatial dataset
crs.geo <- CRS("+proj=longlat +ellps+WGS84 +datum=WGS84") #Add coordiantes
proj4string(Grana) <- crs.geo #assign te coordiante system
str(Grana)

xy <- expand.grid(x=win$xrange,y=win$yrange)

x <- seq(0, 100, by=1.) # resolution is the pixel size you desire 
y <- seq(0, 100, by=1.)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
class(xy)

x <- seq(win$xrange,by=1) # where resolution is the pixel size you desire 
y <- seq(ymin,ymax,by=resolution)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
class(xy)
kud=kernelUD(detections[,1],h=h, grid=xy, kern=c("bivnorm"))



kud_points <- kernelUD(Grana, h = "href", grid = xy)









###########################################################
### With all 
dat <- dat %>% group_by(species) %>% filter(n()>= 20) %>% ungroup()
All <- dat[c(4,5,13)]
coordinates(All) <- c("decimallongitude","decimallatitude") #make dataset spatial dataset
crs.geo <- CRS("+proj=longlat +datum=WGS84") #Add coordiantes 
proj4string(All) <- crs.geo #assign te coordiante system
str(All)


kernel.ref <- kernelUD(All, h = "href", grid = 500, extent= .1, same4all = FALSE)
image(kernel.ref)
#image(kernel.ref, get)
title(main="grid=500, extent=0.5")
kernel.ref[[1]]@h
kernel.area(kernel.ref, percent = c(50,75,90),unout = "km2")


All.kernel.poly <- getverticeshr(kernel.ref, percent = 95) 
plot(getverticeshr(kernel.ref, percent = 75), add = TRUE, col="red")
print(All.kernel.poly)


# Get volume
vud <- getvolumeUD(kernel.ref)
image(vud)
plot(vud[[1]]@coords, add = TRUE)
title(main="Volume")


r <- raster(as(kernel.ref[[1]], "SpatialPixelsDataFrame"))
proj4string(r) <- crs.geo
plot(r)




#writeRaster(r, filename = file.path(tempdir(), "kernel.ref.tif"))

kernel.poly <- getverticeshr(kernel.ref, percent = 95) 
print(Grana.kernel.poly) 


# Plot points 
data(wrld_simpl)
# Plot the base map
win <- extent(matrix(c(All$decimallongitude,All$decimallatitude), nrow = nrow(All)))
win <- data.frame(c(win@xmin, win@xmax),
                  c(win@ymin, win@ymax))
win <- owin(c(win[1,1],win[2,1]),c(win[1,2],win[2,2]))

wack <-plot(wrld_simpl, 
            xlim = win$xrange,
            ylim = win$yrange,
            col = "grey95")
plot(kernel.grana[[1]], add = TRUE)
#plot(Grana.kernel.poly)
points(x = Grana$decimallongitude, y = Grana$decimallatitude,
       # West$decimallongitude, y = West$decimallatitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 1)
##########################################################################
library(hypervolume)
# Hypervolume 
Grana <- dat %>% 
 filter(species == "Piranga ludoviciana")            
Grana <- Grana[c(4,5,13)]



#13
#Subset 
#coordinates(Grana) <- c("decimallongitude","decimallatitude") #make dataset spatial dataset
#crs.geo <- CRS("+proj=longlat +ellps+WGS84 +datum=WGS84") #Add coordiantes
#proj4string(Grana) <- crs.geo #assign te coordiante system
#str(Grana)

#estimate_bandwidth(Grana[1,2], method = "silverman")
hv = hypervolume(Grana[,1:2],method='box')
hypervolume::estimate_bandwidth(Grana[,1:2],method='silverman')
#hvAll = hypervolume(data=subset(All, species=="Granatellus pelzelni")[,1:2],method='box')
summary(hv)
plot.Hypervolume(hv)
cen <- get_centroid(hv)
probs <- hypervolume_estimate_probability(hv, points=Grana)




thinned <- hypervolume_thin(hv, num.points=1000)#  factor = 0.5,
plot.Hypervolume(thinned)
cenThin <- get_centroid(thinned)

 r <- raster(as(hv@RandomPoints, "SpatialPixelsDataFrame"))
proj4string(r) <- crs.geo
plot(r)

wack <- writeRaster(r, "kernel.ref.tif")

#################################################################
## for loop hypervolumes
setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital/species.occurences")
filenames <- list.files(path = getwd(), pattern = "*csv")
numfiles <- length(filenames)
#dataset = list()
hvs_joined = list()
library(hypervolume)
library(ebirdr)


for (i in c(1:numfiles)){
  tryCatch({
    print(filenames[i])
    refs <- read.csv(filenames[i], header = TRUE)
    Cap <- refs[c(4,3,13)]
    hv = hypervolume(Cap[,1:2],method='guassian')
    cen <- get_centroid(hv)
    hvs_joined[[i]] <- hypervolume_join(hv)},
   # datalist[[i]] <- hv},
    error = function(err) {
      
      print(paste("Didn't work:  ",filenames[i]))
      
    },
    warning = function(warn) {}, finally = {}
  )
}
#hv_i<-hypervolume_gaussian(data = All,name = paste(sp_i))
#saveRDS(hv_i,file = paste("hypervolumes/",sp_i,"_gaussian_",bw_multiplier,"_",quantile,sep = ""))
#hv_isolated <-  hypervolume_join(hv@HVList[results$Plot_type == "Isolated"])


dat <- dat %>% group_by(species) %>% filter(n()>= 20) %>% ungroup()
All <- dat[c(4,5,13)]
dir.name(hypervolumes)

for(b in 1:3){
  for(q in 1:3){
    for(i in 1:length(unique(All$species))){
      bw_multiplier<-c(.75,1,1.5)[b]
      quantile<-c(0.95,0.85,0.75)[q]
      hv_i<-NULL
      print(i)
      sp_i<-unique(All$species)[i]  
      #data_i<-raster::ext`ract(x = climate,y = (occurrences[,3:2][which(occurrences$scrubbed_species_binomial==sp_i),]))  
      #data_i<-na.omit(data_i)
      #data_i<-unique(data_i)
      bandwidth<-estimate_bandwidth(data = All$species)*bw_multiplier
      try(hv_i<-hypervolume_gaussian(data = All,name = paste(sp_i,"_gaussian_",bw_multiplier,"_",quantile,sep=""),kde.bandwidth = bandwidth,quantile.requested = quantile))
      saveRDS(hv_i,file = paste("hypervolumes/",sp_i,"_gaussian_",bw_multiplier,"_",quantile,sep = ""))
      
    }
    
  }}



data_split = split(Pass[,1:2],Pass$species)
hvs_split = lapply(data_split, hypervolume); 
hvs_joined = hypervolume_join(hvs_split)

  


#### Start, part 1
West <- DATA %>% 
  filter(species == "Piranga ludoviciana")
All <- dat %>% 
  filter(species == "Piranga ludoviciana")
coordinates(All) <- c("decimallongitude","decimallatitude") #make dataset spatial dataset
crs.geo <- CRS("+proj=longlat +datum=WGS84") #Add coordiantes 
proj4string(All) <- crs.geo #assign te coordiante system
#Create window 
win <- extent(matrix(c(All$decimallongitude,All$decimallatitude), nrow = nrow(All)))
win <- data.frame(c(win@xmin, win@xmax),
                  c(win@ymin, win@ymax))
win <- owin(c(win[1,1],win[2,1]),c(win[1,2],win[2,2]))

grid <- ebirdr::sample.grid.cell(All$decimallongitude,
              All$decimallatitude,xlim = c(-150,100), 
              ylim = c(69,7), nx=100,ny=100,size = 2000)

  
####################################################################
# get worldclim data from internet
climatelayers <- raster::getData("worldclim", var = "bio", res = 10)
climatelayers_ss_cropped = crop(climatelayers,cen)


# z-transform climate layers to make axes comparable
climatelayers_ss = climatelayers[[c(1,4,12,15)]]
for (i in 1:nlayers(climatelayers_ss)){
  climatelayers_ss[[i]] <- (climatelayers_ss[[i]] - cellStats(climatelayers_ss[[i]], 'mean')) / cellStats(climatelayers_ss[[i]], 'sd') 
}




######################################################
library(red)
West <- dat %>% 
  filter(species == "Piranga ludoviciana")            
West <- West[c(4,5)]
Pir <- subset(Shapes, species == "Piranga ludoviciana")

recs <- thin(West, 0.5)


graphics::plot(West)
records <- thin(West, 0.01)
graphics::plot(records)


# Create spatial grid
bb <- bbox(Pir)
cs <- c(30, 30)  # cell size 
cc <- bb[, 1] + (cs/2)  # cell offset
cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
sp_grd <- SpatialGridDataFrame(grd,
                               data=data.frame(id=1:prod(cd)),
                               proj4string=CRS(proj4string(Pir)))








#####
win <- extent(matrix(c(Grana$decimallongitude,Grana$decimallatitude), nrow = nrow(Grana)))
win <- data.frame(c(win@xmin, win@xmax),
                  c(win@ymin, win@ymax))
win <- owin(c(win[1,1],win[2,1]),c(win[1,2],win[2,2]))


# get worldclim data from internet
climatelayers <- raster::getData("worldclim", var = "bio", res = 10)

# z-transform climate layers to make axes comparable
climatelayers_ss = climatelayers[[c(1,4,12,15)]]
for (i in 1:nlayers(climatelayers_ss)){
  climatelayers_ss[[i]] <- (climatelayers_ss[[i]] - cellStats(climatelayers_ss[[i]], 'mean')) / cellStats(climatelayers_ss[[i]], 'sd') 
}

climatelayers_ss_cropped = crop(climatelayers,Gra)

# extract transformed climate values
climate_grana = crop(climatelayers, Grana)
#climate_rubra = extract(climatelayers_ss_cropped, data_rubra)

# compute hypervolumes with auto-bandwidth for both species
hv_alba = hypervolume_gaussian(climate_grana)
hv_rubra = hypervolume_gaussian(climate_rubra,name='rubra',samples.per.point=10)

# determine intersection and unique components of the overlap
hv_set = hypervolume_set(hv_alba, hv_rubra, check.memory=FALSE)

# put all the output volumes in one convenient place
volumes <- get_volume(hv)

# do species distribution modeling (reduce point density by factor of 10 for demo speed)
rubra_map = hypervolume_project(hv,climatelayers_ss_cropped,reduction.factor=0.1)
alba_map = hypervolume_project(hv_alba, climatelayers_ss_cropped,reduction.factor=0.1)

# then barplot of hypervolumes of each component
op=par(mar=c(3,10,1,1))
barplot(volumes,horiz=TRUE,las=2,main="Hypervolume",cex.names=0.5,col='lightblue')

# then pairs plot of the set operations
par(op)
plot(hv_set[[c(3,5,6)]]) # only the unique components of each + intersection

# plot the geographic projections of the ranges
plot(rubra_map,col=colorRampPalette(c(rgb(1,1,1),rgb(1,0,0)))(100),legend=FALSE,main='Quercus rubra')
map('world',add=TRUE)
points(Latitude~Longitude,data=data_rubra,pch=3,cex=0.1)

plot(alba_map,col=colorRampPalette(c(rgb(1,1,1),rgb(0,0,1)))(100),legend=FALSE,main='Quercus alba')
map('world',add=TRUE)
points(Latitude~Longitude,data=data_alba,pch=3,cex=0.1)



