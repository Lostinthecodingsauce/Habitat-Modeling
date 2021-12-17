# Calculating surface area 
setwd("C:/Users/bfsco/Desktop/Masters Research/UCEs/Geospaital/Tiered_Shapefile_Outputs")

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
library(rgee)
library(tiff)

ee_Initialize(drive = TRUE, gcs = TRUE, email = 'bfscott1906@gmail.com')

Pass_a1 <- readOGR("Tier_D-25km/Passerina_amoena/Passerina_amoena_Agg.shp")
Pass_a <- readOGR("null.polys/Passerina_amoena.shp") #something wrong with these polygons 


AAA <- readOGR("Tier_B-100km/Cardinalis_phoeniceus/Cardinalis_phoeniceus_Agg.shp")
