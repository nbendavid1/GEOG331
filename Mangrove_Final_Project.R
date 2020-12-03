#load libraries
library(raster)
library(rgdal)
library(sf)

#load tif raster file
global2000 <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/a2000mfw.tif")

#use "countries" dataset built into raster package
world <- getData("countries") 

#make vector for the countries we are looking at in West Africa with their FIPS codes
wAf_FIPS <- c("SG", "GA", "PU", "GV", "SL", "LI", "IV", "GH", "TO", "BN", "NI", "CM", "EK", "GB", "CF", "CG", "AO")

#trim the world countries polygons data frame to include only the 17 countries we are looking at
#store this as a new spatial polygon data frame for only West Africa
wAfricaSpFrame <- world[match(wAf_FIPS,world$FIPS),]

#extract the 17 polygons from the SpatialPolygonsDataFrame and store as list
wAfPoly <- getSpPpolygonsSlot(wAfricaSpFrame)

#confirm the list is actually a list of 17 items
class(wAfPoly)
length(wAfPoly)
#confirm that the items in the list are Polygons
class(wAfPoly[[1]])

#convert the 17 Polygons to SpatialPolygons so they can be merged into one 
wAfSpPoly <- SpatialPolygons(wAfPoly)
#confirm this has worked
class(wAfSpPoly)

#crop global distribution to region
ext2000 <- crop(global2000,wAfSpPoly)

