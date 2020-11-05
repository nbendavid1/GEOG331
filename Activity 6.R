#download spatial data packages
#install.packages(c("raster","sp","rgdal","rgeos","plyr"))
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

#make color coded map of glaciers by name
spplot(g1966, "GLACNAME")

#check glacier names
g1966@data$GLACNAME
g2015@data$GLACNAME

#use ifelse() to fix glacier names to be consistent over the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

#read in rgb imagery from landsat
redL <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/glacier data/glacier_09_05_14/l08_red.tif")
greenL <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/glacier data/glacier_09_05_14/l08_green.tif")
blueL <- raster("/Users/nadav/Documents/GitHub/data_import_GEOG331/glacier data/glacier_09_05_14/l08_blue.tif")

#check coordinate system for raster data
redL@crs

#make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)
#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#zoom in on a specific area to look closer at a few glaciers
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#set up years to read in
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("/Users/nadav/Documents/GitHub/data_import_GEOG331/glacier data/NDVI/NDVI_",ndviYear[i],".tif"))
  
}

#look at first year of raster data
str(NDVIraster[[1]])
#get projection
NDVIraster[[1]]@crs

#plot NVDI data from 2003 to look closely at it
plot(NDVIraster[[1]])

#------------------------------------------ Question 3 ---------------------------------------------#
#attempt to plot NVDI data from 2003 with 1966 polygons
#make side by side instead
par(mfrow=c(1,2))
plot(NDVIraster[[1]])
plot(g1966, col="palegreen2", border=NA)
#---------------------------------------------------------------------------------------------------#

#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

#------------------------------------------ Question 4 ---------------------------------------------#
#plot reprojected polygons over NDVI                                                                
par(mfrow=c(1,1))                                                                                   
plot(NDVIraster[[13]],axes=F) 
plot(g2015p, border="black", add=TRUE,axes=F)
#---------------------------------------------------------------------------------------------------#

#calculate area for all polygons add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

#also add area to its own table for analysis
gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

#plot glacier area from table
plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
  
}   


#------------------------------------------ Question 5 ---------------------------------------------#
#calculate percent change in area between 1966 and 2015
#make an spplot of the glaciers showing different colors for percent change
g2015p@data$p_change <- ((g2015p@data$a2015m.sq -
                             g1966p@data$a1966m.sq)/g1966p@data$a1966m.sq)
g2015p@data$p_change

#make spplot of percent change
spplot(g2015p, "p_change")

#make a polygon that shows the difference in glaciers between 2015 and 1966
diffPoly <- gDifference(g1966p, g2015p, checkValidity = 2L)

plot(diffPoly)

#plot with NDVI
plot(NDVIraster[[13]], axes=FALSE, box=FALSE)
plot(diffPoly,col="black", border=NA,add=TRUE)
#---------------------------------------------------------------------------------------------------#



#------------------------------------------ Question 6 ---------------------------------------------#
#find glacier with minimum percent change
min(g2015p@data$p_change) #gives us most negative value for percent change
View(g2015p@data) #we can look for this value on the table
#glacier with most change is Boulder Glacier (OBJECTID=5)

#subset the data for only Boulder Glacier
g2015boulder <- subset(g2015, OBJECTID == "5")
g2005boulder <- subset(g2005, OBJECTID == "5")
g1998boulder <- subset(g1998, OBJECTID == "5")
g1966boulder <- subset(g1966, OBJECTID == "5")

#plot Boulder change
par(mai=c(1.2,1.2,1.2,1.2), xpd=TRUE)
boulderplot<- plotRGB(rgbL, ext=extent(g1966boulder)+1000, stretch="lin", axes = TRUE, 
        main = "Boulder Glacier; 85% Loss from 1966 to 2015")
plot(g1966boulder, col="#D3F4FF", border=NA, add = T)
plot(g1998boulder, col="#89E1FF", border=NA, add = T)
plot(g2005boulder, col = "#00A8E1", border = NA, add = T)
plot(g2015boulder, col = "#00688C", border = NA, add = T)
legend("right",
       legend = c("1966", "1998", "2005", "2015"),
       fill = c("#D3F4FF", "#89E1FF", "#00A8E1", "#00688C"),
       title = "Glacier Extent",
       inset=c(-0.25,0))
#---------------------------------------------------------------------------------------------------#


#designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE)


#buffer glaciers
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units

#convert to a raster
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
plot(buffRaster)

#rasterize gralciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)


meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
head(meanChange)


#------------------------------------------ Question 9 ---------------------------------------------#
#add meanChange NDVI to 2015 glacier polygons
g2015p@data$meanChange <- meanChange[2:40,"mean"]

#plot 2015 glacier polygons with meanChange
spplot(g2015p, "meanChange")
#---------------------------------------------------------------------------------------------------#



#------------------------------------------ Question 11 ---------------------------------------------#
#find average NDVI for all years using NDVIstack from earlier using stackApply
NDVIave <- stackApply(NDVIstack, 
                      indices =  rep(1,nlayers(NDVIstack)),
                      fun = "mean",
                      na.rm = T)

#plot the average NDVI
plot(NDVIave)

#find mean for all years
mean(NDVIave)

#make a scatterplot of NDVI within 100 m and glaier size
#use zonal() to summarize mean for raster objects for each zone, using buffers from earlier as zones
NDVIzonal <- zonal(NDVIave, 
                   buffRaster,
                   "mean")

plot(g1966p@data$a1966m.sq, NDVIzonal[2:40,"mean"],
     xlab= "Glacier Size",
     ylab="Mean NDVI of 100 m Buffer",
     pch=16)

#use ifelse() to divide mean NDVI measurements by <0.2, 0.2-0.4, and >0.4
#give a color for each category
g2015p@data$colors <- ifelse(NDVIzonal[2:40,"mean"] < 0.2, "#A8BBFF",
                             ifelse(NDVIzonal[2:40,"mean"] < 0.4, "#335FFF", "#001F8E"))

#plot the average NDVI values with the buffer polygons overlaid
par(mai=c(0.5,0.5,0.5,0.5), xpd=FALSE)
plot(NDVIave, ext=g2015p, axes=FALSE)
plot(g2015p, border=g2015p@data$colors, add=TRUE)
par(xpd=TRUE)
legend("topright",
       legend = c("< 0.2", "0.2 - 0.4", "< 0.4"),
       fill = c("#A8BBFF", "#335FFF", "#001F8E"),
       title = "Average NDVI",
       inset=c(-0.25,-0.1))
#---------------------------------------------------------------------------------------------------#





