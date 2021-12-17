#Hypervolume Forloop###################
library(ade4)
library(dismo)
library(grDevices)    
library(hypervolume)
library(lattice)
library(maps)
library(raster)
library(shapefiles)
library(ecospat)
library(dplyr)
library(maps)
library(rgeos)
library(maptools)
library(rgdal)
library(sf)
library(spatstat)



setwd("C:/Users/bfsco/Desktop/Masters Research/Geospaital/species.occurences/KDE")
# Example # 

Amaurospiza_concolor <- read.csv("Amaurospiza concolor.csv")
hv = hypervolume(data=subset(Amaurospiza_concolor, species=="Amaurospiza concolor")[,1:2],method='box')


Cardinalis_Cardinalis<- read.csv("Cardinalis_cardinalis_breeding.csv")
Passerina_ciris<- read.csv("Passerina_ciris_breeding.csv")
Passerina_cyanea<- read.csv("Passerina_cyanea_breeding.csv")

species <- read.csv("Chlorothraupis_stolzmanni.csv")
species <- species %>% 
  rename(
    latitude = Y, 
    longitude = X) 

#hv = hypervolume(data=subset(Cardinalis_phoeniceus, species=="Cardinalis phoeniceus")[,1:2],method='box')
hv = hypervolume(data=species[,3:2],method='box')

#Cardinalis_cardinalis <- read.csv("Cardinalis_cardinalis.csv")
#hv = hypervolume(data=Cardinalis_cardinalis[,1:2],method='box')

summary(hv)
plot(hv)
get_centroid(hv)
write.csv(centroid,file ="Centroids/Passerina_cyanea.csv")


########## for loop ###########

setwd("C:/Users/bfsco/Desktop/Masters Research/Geospaital/species.occurences")
setwd("C:/Users/bfsco/Desktop/Masters Research/Geospaital/test.SEM/NewBreeding")
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
  df.wrs = hypervolume(data=df[,7:8],method='box')
  cen <- get_centroid(df.wrs)
  centroid <- data.frame(cen)
 # dat <- data.frame(df.wrs)
  file_name <- paste("Centroids/", gsub(" ","_",filenames[[i]]), sep="")
  write.csv(centroid,file = file_name)
  datalist[[i]] <- centroid}

# create one dataframe from results 
data <- do.call(rbind, datalist)


#########################
# Plotting hypervolume 
########################
# create spatial dataframe 

dat <- Amaurospiza_concolor

coordinates(dat) <- c("decimallongitude","decimallatitude") #make dataset spatial dataset
crs.geo <- CRS("+proj=longlat +ellps+WGS84 +datum=WGS84") #Add coordiantes 
proj4string(dat) <- crs.geo #assign te coordiante system


# Plot the base map
win <- extent(matrix(c(df$decimallatitude,df$decimallongitude), nrow = nrow(df)))
win <- data.frame(c(win@xmin, win@xmax),
                  c(win@ymin, win@ymax))
win <- owin(c(win[1,1],win[2,1]),c(win[1,2],win[2,2]))

data(wrld_simpl)

wack <-plot(wrld_simpl, 
            xlim = win$xrange,
            ylim = win$yrange,
            col = "grey95")
points(x = dat$decimallatitude, y = dat$decimallongitude,
       # West$decimallongitude, y = West$decimallatitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 1)

plot(df, pch=20, col='black', cex=0.50, add = TRUE)
box()
title('Amaurospiza concolor')
legend('bottomleft', legend=c('Original sample', 'Subsample'), 
       col=c('black','green'),pch=c(20,20),bg="white")

#################################################################################
   ######## Creating radius around hypervolumes #####################
#################################################################################
setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital")
centroids <- read.csv("centroids.csv")

roids <-readOGR("Centroids.shp")

dat <- centroids

coordinates(dat) <- c("decimallongitude","decimallatitude") #make dataset spatial dataset
crs.geo <- CRS("+proj=longlat +ellps+WGS84 +datum=WGS84") #Add coordiantes 
proj4string(dat) <- crs.geo #assign te coordiante system




stores <- st_sfc(st_multipoint(centroids[c(2,3)]), crs = crs.geo)


map <- roids %>% 
  addCircles(lng = coordinates_point_two[1], lat = coordinates_point_two[2], 
             radius = distance_max_two * 1000, color = "red")


cart.earth <- circle.polygon(-117.24, 32.86, 40, poly.type = "cart.earth")

lat.range <- c(32, 34)
lon.range <- c(-118.5, -116)

op <- par(mar = c(3, 5, 5, 5) + 0.1, oma = c(1, 1, 1, 1))

map("worldHires", fill = TRUE, col = "wheat3", xlim = lon.range, ylim = lat.range)
points(-117.24, 32.86, pch = 19, col = "red")
polygon(cart.earth, border = "red", lwd = 3)
lat.lon.axes(n = 3)
box(lwd = 2)
mtext("poly.type = 'cart.earth'", line = 3)

par(op)





# Plot the base map
win <- extent(matrix(c(dat$decimallatitude,dat$decimallongitude), nrow = nrow(dat)))
win <- data.frame(c(win@xmin, win@xmax),
                  c(win@ymin, win@ymax))
win <- owin(c(win[1,1],win[2,1]),c(win[1,2],win[2,2]))

data(wrld_simpl)

wack <-plot(wrld_simpl, 
            xlim = win$xrange,
            ylim = win$yrange,
            col = "grey95")
points(x = dat$decimallatitude, y = dat$decimallongitude,
       # West$decimallongitude, y = West$decimallatitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 1)

