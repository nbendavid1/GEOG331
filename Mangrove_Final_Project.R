library(raster)
library(rgdal)
library(sf)
library(spatial)
library(ggplot2)
library(RColorBrewer)

# create a vector of file names
f <- list.files(pattern = glob2rx("*mfw.tif"))

# create a vector of output filenames
fo <- paste("waf", f, sep = "_")

# get extent of W. Africa 
# note you can make this manually
# ex <- extent(-18,32, -18, 17)

#use "countries" dataset built into raster package
world <- getData("countries") 

#make vector for the countries we are looking at in West Africa with their FIPS codes
wAf_FIPS <- c("SG", "GA", "PU", "GV", "SL", "LI", "IV", "GH", "TO", "BN", "NI", "CM", "EK", "GB", "CF", "CG", "AO")

#trim the world countries polygons data frame to include only the 17 countries we are looking at
#store this as a new spatial polygon data frame for only West Africa
wAfricaSpFrame <- world[match(wAf_FIPS,world$FIPS),]

#extract the 17 polygons from the SpatialPolygonsDataFrame and store as list
wAfPoly <- getSpPpolygonsSlot(wAfricaSpFrame)

#convert the 17 Polygons to SpatialPolygons so they can be merged into one 
wAfSpPoly <- SpatialPolygons(wAfPoly)

# clean up the environment
rm(world,wAfricaSpFrame,wAfPoly)

# crop loop through each file and crop to W. Africa extent
for(i in 1:length(f))
{
  dat <- raster(f[i])
  crop(dat,wAfSpPoly,
       filename = fo[i], overwrite = T)
  rm(dat)
}

#import 2000 mangrove forest extent cropped
mang2000 <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/cropped/cropped2000.tif")

#import cropped rasters
crop2001 <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/cropped/cropped2001.tif")
crop2002 <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/cropped/cropped2002.tif")
crop2003 <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/cropped/cropped2003.tif")
crop2004 <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/cropped/cropped2004.tif")
crop2005 <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/cropped/cropped2005.tif")
crop2006 <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/cropped/cropped2006.tif")
crop2007 <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/cropped/cropped2007.tif")
crop2008 <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/cropped/cropped2008.tif")
crop2009 <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/cropped/cropped2009.tif")
crop2010 <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/cropped/cropped2010.tif")
crop2011 <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/cropped/cropped2011.tif")
crop2012 <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/cropped/cropped2012.tif")

##
plot(crop2012)

chng00_12 <- raster::overlay(crop2012,crop2000,
                             fun = function(x,y){return(x-y)},
                             filename = "/Users/nadav/Documents/GitHub/chng00_12.tif")


brkcols <- brewer.pal(10,"BrBG") 
plot(chng00_12,col=brkcols)

#invert colors
chng00_12 <- raster::calc(chng00_12,
                          fun = function(x){return(x*-1)},
                          filename = "/Users/nadav/Documents/GitHub/chng00_12inv.tif")
plot(chng00_12)
##


#load Hansen loss dataset files
#each file is a 10 x 10 degree tile
loss00N00E <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/hansen loss/Hansen_loss_00N_000E.tif")
loss00N10E <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/hansen loss/Hansen_loss_00N_010E.tif")
loss10N00E <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/hansen loss/Hansen_loss_10N_000E.tif")
loss10N10W <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/hansen loss/Hansen_loss_10N_010W.tif")
loss10N20W <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/hansen loss/Hansen_loss_10N_020W.tif")
loss10S10E <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/hansen loss/Hansen_loss_10S_010E.tif")
loss10N10E <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/hansen loss/Hansen_loss_10N_010E.tif")
loss20N20W <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/hansen loss/Hansen_loss_20N_020W.tif")

#merge the tiles to one continuous raster
loss <- merge(loss00N00E,loss00N10E,loss10N00E,loss10N10W,loss10N20W,loss10S10E,loss,loss10N10E,loss20N20W,
              filename = "/Users/nadav/Documents/GitHub/data_import_GEOG331/hansen loss/Hansen_loss_total.tif")

#resample the 2000 distribution using loss extent using bilinear interpolation
#this will allow us to mask loss with mang2000
mang2000re <- resample(mang2000,loss,
                       method = 'bilinear',
                       filename = "/Users/nadav/Documents/GitHub/data_import_GEOG331/resampled2000.tif")

#mask the loss raster by the mang2000
#this removes any values from loss that did not appear on mang2000
#makes sure that we are only looking at actual mangroves
defor00_19 <- mask(loss, mang2000, 
                   filename="/Users/nadav/Documents/GitHub/data_import_GEOG331/hansen loss/mang_loss_00_19.tif",
                   inverse=FALSE, 
                   updatevalue=NA, 
                   updateNA=FALSE)

#use clump() to find clumps of points
#this gives us deforestation hotspots
deforClumps <- clump(defor00_19,
                     filename="/Users/nadav/Documents/GitHub/data_import_GEOG331/hansen loss/deforClumps.tif",
                     directions=8,
)

#find
