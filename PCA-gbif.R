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
library(raster)
library(readr)
library(corrplot)
library(countrycode)
library(rgbif)
library(dplyr)
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
library(RStoolbox)
library(spDataLarge)
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
source("cleaning_gbif.R") #Dillions code for cleaning records/

#cleaned.gbif <- read.csv("final.cleaned.gbif.csv") # Cleaned Cardinal REcords
df <- read.csv("cleaned.records.csv")
# Remove any saltators 
df <- df %>%
  filter(genus !="Saltator")
source("cleaning_gbif.R") #Dillions code for cleaning records/
DATA <- df # have two datasets, one that has corrdinate pursession and one raw
climate_records <- read.csv("fuck.csv")

coordinates(df) <- c("decimallongitude","decimallatitude") #make dataset spatial dataset
crs.geo <- CRS("+proj=longlat +ellps+WGS84 +datum=WGS84") #Add coordiantes 
proj4string(df) <- crs.geo #assign te coordiante system
Shapes <- readOGR("CardShapes.shp") #import shape files 

# read in csv "Cardinalidae-names.csv at top. Use this code to get list of species names
taxon <- names$Species


#designating limits
lims<-c(-150,0,-35,45)

# Crop based on limits
dat_cl <- DATA%>%
  filter(decimallongitude > -150, decimallongitude < -32) # remove records from outside range
# Plot results
wm <- borders("world", colour="gray50", fill="gray50")
ggplot()+ coord_fixed()+ wm +
  geom_point(data = dat_cl, aes(x = decimallongitude, y = decimallatitude),
             colour = "darkred", size = 0.5)+
  theme_bw()
# Crop bioclim data to geographic extent of Cardinals
submap<-crop(r,lims)
plot(submap)
#### Recover worldclim data and align to species range
library(raster)
library(sp)

r <- raster::getData("worldclim", var="bio", res=10)

#look at range of lat and lon
range(dat_cl[,4]) #lat range
## [[1] -45.80000  69.56828
range(avg.clim[,5]) # lon range
## [1] -149.967  -34.900
#designating limits
lims<-c(-150,-34,-46,69)

# Crop bioclim data to geographic extent of Cardinals
submap<-crop(r,lims)
plot(submap,1)#plot MAT


############## Works to extract raster values from point coutns
#coords<-dat_cl[c(5,4)]
#samps<-dat_cl[c(1,4:6)]
#points <- SpatialPoints(coords, proj4string = r@crs)
values <- raster::extract(r,df)
my.data <- cbind.data.frame(coordinates(df),values)
my.data<-cbind.data.frame(df, my.data)
write.csv(FUCKTRUMP,file = "fuck.csv")
# Need to save dataframe but its being weird 





###########################
max.lat <- ceiling(max(dat_cl$decimallatitude))
min.lat <- floor(min(dat_cl$decimallatitude)) 
max.lon <- ceiling(max(dat_cl$decimallongitude))
min.lon <- floor(min(dat_cl$decimallongitude))
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

# Plot the base map
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")

# Add the points for individual observation
points(x = obs.data$longitude, 
       y = obs.data$latitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)
# And draw a little box around the graph
box()

# Crop bioclim data to geographic extent of saguaro
bioclim.data <- crop(x = bioclim.data, y = geographic.extent)

### Create sample grid 
bebe <- gridSample()
###################################
# Dillions Code
####

df.1 <- read.csv("fuck.csv")

df.2 <- df.1[-c(3,5,6,7,8)]
output_df <- data.frame()
#df.3 <- na.omit(df.2) 

for (i in 1:length(species)){
  
  df_sub <- subset(df.2,df.2$species == species[i])
  
  for (c in 1:ncol(df_sub)){
    
    output_df[i,c] <- mean(na.omit(df_sub[,c]))
  }
}
names(output_df) <- names(df.2)
output_df$species <- species

write.csv(output_df,file = "mean.climate.csv")
###############

# subtract mean/ SD for each column 
output_SD <- data.frame()

for(var in 1:19){
  print(sd(df.2[var]))
}

# Take mean first
# transform 1) Every column has mean=0, SD=1
# Transform 2) Taking PCA of new matrix. Matrix stays same size, roating to explain most variance
# Transformation 3) average rows by species. take 1millx19 to  55x19. Every row = mean of all of entries in original matrix 

# End with 44x19 prcomp object. Get loadings with sdv, get cordinates for species with x 
# square list of squa, get variance for each PC. To get fraction by of how much each PC contributes to variance, add them up. 

### Centered at 0, magnitutde of 1 (0= mean, Sd=1)
#Take new dataframe, subtract original mean, divide by SD 
med.clim <- read.csv("med.climate.csv")
mean.clim <- read.csv("mean.climate.csv")

df.2 <- mean.clim
output_SD <- df.2

for(var in 5:23) {
  output_SD[,var] = df.2[,var] / sd(df.2[,var])  
  output_SD[,var] = output_SD[,var] - mean(output_SD[,var])
}

### COMBINE WITH SPECIES NAMES 
#basic <- df.2[c(2)]
#THEDATA <- cbind(basic,output_SD)
row.names(output_SD) <- output_SD$species
THEDATA <- output_SD[c(-1,-2,-3,-4)] # Remove Lat long
THEDATA <- output_SD[c(-1,-4)]

remove <- c("Amaurospiza carrizalensis","Amaurospiza moesta","Cyanocompsa cyanea","Cyanoloxia rothschildii", "Cyanoloxia cyanoides", "Habia cristata","Granatellus paraensis","Granatellus francescae","Parkerthraustes humeralis","Pheucticus chrysopeplus", 
            "Pheucticus chrysopeplus aurantiacus","Piranga hepatica","Piranga lutea","Habia rubica bahiae","Habia rubica peruviana")
finaldata = THEDATA[!row.names(THEDATA)%in%remove,]


#########################################################################
 #### Finally PCA! Run a) with all 19, and run b) after removing correlated variables 
#######################################################################
finaldata1 <-finaldata[c(-1,-2,-9)] # Drop Bio7
PCA <- prcomp(finaldata1) # PC1 and PC2 describe 90% of variance 
loadingsFULL <- PCA$rotation
load <- loadingsFULL^2
scoresFull <- PCA$x
scores <-data.frame(scoresFull)
#scores <- cbind(scores, row.names(finaldata))

PC1vPC2 <- ggplot(data = scores, aes(x = PC1, y = PC2)) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_point(colour = "Tomato", alpha = 0.8, size = 4) +
  ggtitle("PC1 vs PC2") #+ geom_point(aes(colour=All_WPTCS_score$Group, size = 4))
plot(PC1vPC2)
# repeat what we did, just use species by data matrix. 
# Take mean of matrix we just produce for each species

#### Using Bioclim varialbes that are not correlated. First round use Bio1, Bio2, Bio5, Bio 14. Use threshold of R < .6(+/-) 
babyset <- finaldata[c(3,4,7,16)]

# DOublecheck correlations 
cors<-cor(babyset) # evaluate correlations
corrplot(cors ,order = "AOE", addCoef.col = "grey",number.cex=.6) # plot
# correlogram with hclust reordering
corrplot(cors, type="upper", order="hclust", addCoef.col = "grey",number.cex=.6)


# Run PCA
PCA.baby <- prcomp(babyset)
loadingsBaby <- PCA.baby$rotation
loadbb <- loadingsBaby^2
scoresBaby <- PCA.baby$x

###################
# average row
# Standard devation row
#   subtract mean by ever row, / by SD 

#####


# name rows as species name, create ne datafram for experiments
output2_df <- avg.clim
output2_df <- data.frame(output2_df)
rownames(output2_df) <- output2_df$species
output2_df <- output2_df[c(-1)]
# Drop lat long
output2_df <- output2_df[c(1,4:22)]


### Centered at 0, magnitutde of 1 (0= mean, Sd=1)
#Take new dataframe, subtract original mean, divide by SD 


for(var in 1:19) {
  finaldata[,var] = df[,var] / sd(df[,var])  
  finaldata[,var] = finaldata[,var] - mean(finaldata[,var])
}
###  Run PCA

PCA.sum <- prcomp(finaldata)
loadings.avg <- PCA.sum$rotation
scores <- PCA.sum$x
write.csv(scores, file = "clim.pca.scores.csv")

# Get proportion of variance explained
load <- loadings.avg^2

# remove bio7, it is just bio5-bio6
finaldata1 <-finaldata1[ , -which(names(finaldata1) %in% c("bio7"))]


##################################################################################
# Test for autocorrelation amongst climatic varaibles 
cors<-cor(finaldata[c(4:22)]) # evaluate correlations
corrplot(cors ,order = "AOE", addCoef.col = "grey",number.cex=.6) # plot
# correlogram with hclust reordering
corrplot(cors, type="upper", order="hclust", addCoef.col = "grey",number.cex=.6)

# Test for autocorrelation amongst climatic varaibles 
cors<-cor(avg.clim[c(7:25)]) # evaluate correlations
corrplot(cors ,order = "AOE", addCoef.col = "grey",number.cex=.6) # plot
# correlogram with hclust reordering
corrplot(cors, type="upper", order="hclust", addCoef.col = "grey",number.cex=.6)


# Specialized the insignificnt value according to the significant level

### Compute matrix of p-values
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(cors)
head(p.mat[, 1:5])

corrplot(cors, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.05)


# Kepp only nocorrelated ones- test!
finaldata1 <-finaldata1[ , -which(names(finaldata1) %in% c("bio7"))]

finaldata2 <-finaldata1[ , which(names(finaldata1) %in% c("bio1","bio2","bio5","bio15"))]
#finaldata2=finaldata1[[c("bio1","bio2","bio13","bio14")]] # keep just reasonably uncorrelated ones

##### Climate PCA for all bioclin variables
# using prmcoomp
comb.clim.pca <- princomp(finaldata1, cor = TRUE) 
# Get loadings and scores
clim.pca.loadings <-comb.clim.pca$loadings
clim.pca.scores <-comb.clim.pca$scores
######
# Using RDA
comb.clim.rda <- rda(finaldata1)
clim

########################################################################

for(var in 2:20) {
  output_SD[,var] = df.2[,var] / sd(df.2[,var])  
  output_SD[,var] = output_SD[,var] - mean(output_SD[,var])
}
### COMBINE WITH SPECIES NAMES 
#basic <- df.2[c(2)]
#THEDATA <- cbind(basic,output_SD)
row.names(output_SD) <- output_SD$Species
THEDATA <- output_SD[c(-1,-21)]

finaldata <- THEDATA[-c(11),] # remove Cyanocompsa_cyanoide

######################################


############### Getting data fro Piranga rubra from San Diego
#Lat long SD
#32.7157° N, 117.1611° W
San_Diego<-  c("32.7175", "117.1611")
#########################################################################
#### Finally PCA! Run a) with all 19, and run b) after removing correlated variables 
#######################################################################
finaldata1 <-finaldata[c(-7)] # Drop Bio7
PCA <- prcomp(finaldata1)
loadingsFULL <- PCA$rotation
load <- loadingsFULL^2
scoresFull <- PCA$x
scores <-data.frame(scoresFull)
scores <- cbind(scores, row.names(finaldata1))

###############
library(ggbiplot)
ggbiplot::ggbiplot(PCA)


PC1vPC2 <- ggplot(data = scores, aes(x = PC1, y = PC2)) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_point(colour = "Tomato", alpha = 0.8, size = 4) +
  ggtitle("PC1 vs PC2") #+ geom_point(aes(colour=All_WPTCS_score$Group, size = 4))
plot(PC1vPC2)

