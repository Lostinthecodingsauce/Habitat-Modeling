### Evenly sample points

library(cptcity)
library(raster)
library(stars)
library(rgee)
library(sf)
library(rgdal)
library(googledrive)
library(googleCloudStorageR)
library(maps)
library(maptools)
library(dplyr)
library(geojsonio)
library(tiff)
library(stars)
library(rgdal)
require(spatialEco)
require(spatstat)
require(sp) 

ee_Initialize(drive = TRUE,gcs = TRUE, email = 'bfscott1906@gmail.com')

setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital/species.occurences")

######## Step 1 #############
#### Evenly sample points 
library(rgeos)
library(spatialEco)

plot(aaa)
points = sample.poly(aaa, n= 4000, type = "random")
points2 = sample.poly(aaa, n= 4000, type = "regular")
#############################################
#another type which may answer your problem
plot(points, col="red", add=T)


Amaurospiza_concolor <- read.csv("Amaurospiza concolor.csv")

Passerina_amoena <- read.csv("Passerina_amoena.csv")
Passerina_caerulea <- read.csv("Passerina_caerulea.csv")
Piranga_ludoviciana <- read.csv("Piranga_ludoviciana.csv") #done 
Pheucticus_melanocephalus <- read.csv("Pheucticus_melanocephalus.csv")
Spiza_americana <- read.csv("Spiza_americana.csv")
Piranga_olivacea <- read.csv("Piranga_olivacea.csv")
Passerina_cyanea <- read.csv("Passerina_cyanea.csv")
Cardinalis_sinuatus <- read.csv("Cardinalis sinuatus.csv")

df <- Cardinalis_sinuatus

coordinates(df) <- c("decimallongitude","decimallatitude") #make dataset spatial dataset
crs.geo <- CRS("+proj=longlat +ellps+WGS84 +datum=WGS84") #Add coordiantes 
proj4string(df) <- crs.geo #assign te coordiante system
##################################################################################
require(spatialEco)
require(spatstat)
require(sp) 
#https://gis.stackexchange.com/questions/262997/optimally-thin-or-subset-sample-points
#### THis function also outputs a KDE, and works better

plot(df, pch=20, col='black', cex=0.50)
box()
title("Observed population")

n = round(length(df) * 0.65, digits=0)
n = round(length(df) * 0.80, digits=0)   
df.wrs <- pp.subsample(df, n=n, window='extent') 

#plot(df, pch=20, col='black', cex=0.50)
plot(df.wrs, pch=20, col='red', cex=.30, add=TRUE) 
box()
title('Cardinalis sinuatus subsample')
legend('bottomright', legend=c('Original sample', 'Subsample'), 
       col=c('black','red'),pch=c(20,20),bg="white")


dat <- data.frame(df.wrs)
dat$species <- 'Amaurospiza_concolor'
write.csv(dat,"Thinned/Amaurospiza_concolor.csv")


##############################################################################################
###### For lOpp
setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital/species.occurences")
################### KDe forloop ##############
filenames <- list.files(path = getwd(), pattern = "*csv")
numfiles <- length(filenames)

n65 = round(length(df) * 0.65, digits=0) #Thin Each species by 65%
#n35 = round(length(df) * 0.35, digits=0) #Thin Each species by 35%
#n80 = round(length(df) * 0.80, digits=0) #Thin Each species by 65%


crs.geo <- CRS("+proj=longlat +ellps+WGS84 +datum=WGS84") #Add coordiantes 
datalist = list()
for (i in (1:numfiles)){
#  tryCatch({
    print(filenames[i])
    df <- read.csv(filenames[i], header = TRUE)
    coordinates(df) <- c("decimallongitude","decimallatitude") #make dataset spatial dataset
    proj4string(df) <- crs.geo #assign te coordiante system
    n = round(length(df) * 0.65, digits=0)
    df.wrs <- pp.subsample(df, n=n, window='extent')#, sigma = 'likelihood') 
    #plot(df.wrs, pch=20, col='red', cex=.50, add=TRUE) 
    dat <- data.frame(df.wrs)
    file_name <- paste("wack/", gsub(" ","_",filenames[[i]]), sep="")
    write.csv(dat,file = file_name)
    datalist[[i]] <- dat}


 b data <- do.call(rbind, datalist)

# No need, very small 
# [1] "Didn't work:   Chlorothraupis frenata.csv"
# [1] "Cyanocompsa brissonii.csv"
# [1] "Didn't work:   Cyanocompsa brissonii.csv"
# [1] "Cyanocompsa cyanea.csv"
# [1] "Didn't work:   Cyanocompsa cyanea.csv"
# "Periporphyru_erythromelas.csv"
# [1] "Didn't work:   Periporphyru_erythromelas.csv"
# [1] "Piranga_lutea.csv"
# [1] "Didn't work:   Piranga_lutea.csv"

### Species that did not work 
Chlorothraupis_frenata <- read.csv("Chlorothraupis frenata.csv")#too small 

Piranga_lutea <- read.csv("Piranga_lutea.csv") #too small 
Periporphyru_erythromelas <- read.csv("Periporphyru_erythromelas.csv") #too small 

 
 coordinates(df) <- c("decimallongitude","decimallatitude") #make dataset spatial dataset
 crs.geo <- CRS("+proj=longlat +ellps+WGS84 +datum=WGS84") #Add coordiantes 
 proj4string(df) <- crs.geo #assign te coordiante system
   
plot(df, pch=20, col='black', cex=0.50)
box()
title("Observed population")

n = round(length(df) * 0.65, digits=0)
n = round(length(df) * 0.80, digits=0)   
df.wrs <- pp.subsample(df, n=n, window='extent') 

#plot(df, pch=20, col='black', cex=0.50)
plot(df.wrs, pch=20, col='red', cex=.50, add=TRUE) 
box()
title('60% subsample')
legend('bottomright', legend=c('Original sample', 'Subsample'), 
       col=c('black','red'),pch=c(20,20),bg="white")


dat <- data.frame(df.wrs)
dat$species <- 'Cyanocompsa_brissonii'
write.csv(dat,"Thinned/Cyanocompsa_brissonii.csv")
#################################

### Creating geoTiff from KDEs###########  

##############################
setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital/species.occurences")
################### KDe forloop ##############
filenames <- list.files(path = getwd(), pattern = "*csv")
numfiles <- length(filenames)

n65 = round(length(df) * 0.65, digits=0) #Thin Each species by 65%
#n35 = round(length(df) * 0.35, digits=0) #Thin Each species by 35%
#n80 = round(length(df) * 0.80, digits=0) #Thin Each species by 65%


crs.geo <- CRS("+proj=longlat +ellps+WGS84 +datum=WGS84") #Add coordiantes 
datalist = list()











library(BiodiversityR)
library(dismo)
#########################


#################################################
#Simulation window parameters
xMin=-98.3561;xMax=-48.7800;
yMin=-12.5655;yMax=21.5234;
xDelta=xMax-xMin;yDelta=yMax-yMin; #rectangle dimensions
areaTotal=xDelta*yDelta;

#Point process parameters
lambda=100; #intensity (ie mean density) of the Poisson process

#Thinning probability parameters
sigma=0.5; #scale parameter for thinning probability function
#define thinning probability function
fun_p <- function(s,x,y) {
  exp(-(x^2 + y^2)/s^2);
}

#Simulate a Poisson point process
numbPoints=rpois(1,areaTotal*lambda);#Poisson number of points
xx=xDelta*runif(numbPoints)+xMin;#x coordinates of Poisson points
yy=xDelta*runif(numbPoints)+yMin;#y coordinates of Poisson points

#calculate spatially-dependent thinning probabilities
p=fun_p(sigma,xx,yy); 

#Generate Bernoulli variables (ie coin flips) for thinning
booleThinned=runif(numbPoints)<p; #points to be thinned
booleRetained=!booleThinned; #points to be retained

#x/y locations of thinned points
xxThinned=xx[booleThinned]; yyThinned=yy[booleThinned];
#x/y locations of retained points
xxRetained=xx[booleRetained]; yyRetained=yy[booleRetained];

#Plotting
par(pty="s")
plot(xxRetained,yyRetained,'p',xlab='x',ylab='y',col='blue'); #plot retained points
points(xxThinned,yyThinned,col='red'); #plot thinned points
#################################################

##################################################################
woot <- cbind.data.frame(xxRetained,yyRetained)
