### I have used so many different packages I have lost which ones I need and which ones I dont.
library(raster)
library(corrplot)
library(rgbif)
library(dplyr)
library(rgdal)
library(maps)
library(qgraph)
library(psych)
library(tidyverse)
library(sp)
library(mapdata)
library(spData)
library(ggplot2)
library(vegan)
library(factoextra)

setwd("C:/Users/bfsco/Desktop/Masters Research/Geospaital")

centroids <- read.csv("centroids.csv")
df <- centroids
rownames(df) <- df$ï..species
centroids$decimallongitude
coordinates(df) <- c("decimallongitude","decimallatitude") #make dataset spatial dataset
crs.geo <- CRS("+proj=longlat +ellps+WGS84 +datum=WGS84") #Add coordiantes 
proj4string(df) <- crs.geo #assign te coordiante system

max.lat <- ceiling(max(centroids$decimallatitude))
min.lat <- floor(min(centroids$decimallatitude))
max.lon <- ceiling(max(centroids$decimallongitude))
min.lon <- floor(min(centroids$decimallongitude))
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))


# Plot results
wm <- borders("world", colour="gray50", fill="gray50")
ggplot()+ coord_fixed()+ wm +
  geom_point(data = centroids, aes(x = decimallongitude, y = decimallatitude),
             colour = "darkred", size = 0.5)+
  theme_bw()

r <- raster::getData('worldclim', var='bio', res=5)


#look at range of lat and lon
lat <- range(centroids[,2]) #lat range
## [[1] -25.88800  41.21336
long <- range(centroids[,3]) # lon range
## [1] -105.82383  -49.18857
#designating limits
lims <- cbind(lat,long)

# Crop bioclim data to geographic extent of Cardinals
submap<-crop(r,geographic.extent)
plot(submap,1)#plot MAT


############## Works to extract raster values from point coutns
#coords<-dat_cl[c(5,4)]
#samps<-dat_cl[c(1,4:6)]
#points <- SpatialPoints(coords, proj4string = r@crs)
values <- raster::extract(r,df)
my.data <- cbind.data.frame(coordinates(df),values)
my.data<-cbind.data.frame(df, my.data)
write.csv(my.data,file = "climate.centroids.csv")

###################################################################

### Drop species that are not in MT tree
row.names.remove <- c("Chlorothraupis_frenata","Cyanocompsa_cyanoides","Cyanocompsa_brissonii",
                      "Cyanocompsa_cyanea","Cyanocompsa_rothschildii")

rownames(my.data) <- my.data[,1,]

# drop species 
df.c <- my.data[!(row.names(my.data) %in% row.names.remove), c(7:25) ]

#######################
### Climate PCA #######
######################
df <- read.csv("climate.centroids.csv")
rownames(df) <- df[,1,]
df.c <- df[c(8:26)]

# Drop Bio7, which is the difference between Bio5 and Bio6
df.c <- df.c[c(-7)]

pca.cor <- princomp(df.c,cor = TRUE, scores = TRUE)
summary(pca.cor)#Importance of Components
print(pca.cor) #Standard Deviations
pca.cor$loadings

fviz_eig(pca.cor)

fviz_pca_ind(pca.cor,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

## Get scores
scores.cor <- as.data.frame(pca.cor$scores)
sp2<-ggplot(scores.cor, aes(x=Comp.1, y=Comp.2, color=Comp.3)) + geom_text(label=rownames(scores.cor))  # + geom_point(size = 3)
sp2+scale_color_gradient(low="blue", high="red")

write.csv(scores.cor, file = "Climate.PCA.scores.csv")
