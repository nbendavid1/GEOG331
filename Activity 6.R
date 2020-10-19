#download spatial data packages
install.packages(c("raster","sp","rgdal","rgeos","plyr"))
#load in packages
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

#read in vector data for glacier outlines contained in shapefile
#use readOGR function to do this for 4 glaciers
g1966 <- readOGR("/Users/nadav/Documents/GitHub/data_import_GEOG331/glacier data/GNPglaciers/GNPglaciers_1966.shp", stringsAsFactors = T)
g1998 <- readOGR("/Users/nadav/Documents/GitHub/data_import_GEOG331/glacier data/GNPglaciers/GNPglaciers_1998.shp", stringsAsFactors = T)
g2005 <- readOGR("/Users/nadav/Documents/GitHub/data_import_GEOG331/glacier data/GNPglaciers/GNPglaciers_2005.shp", stringsAsFactors = T)
g2015 <- readOGR("/Users/nadav/Documents/GitHub/data_import_GEOG331/glacier data/GNPglaciers/GNPglaciers_2015.shp", stringsAsFactors = T)

#look at data structure
head(g2015@data)

#store in polygons
g2015@polygons[[1]]

#look up projection info
g1966@proj4string

