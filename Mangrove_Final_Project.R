## Davi Bendavid
## GEOG331: Environmental Data Science
## Quantifying and investigating mangrove deforestation in West Africa

## Section 1: Load necessary libraries

library(raster)
library(igraph)
library(rgdal)
library(sf)
library(spatial)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(spatialEco)
library(exactextractr)
#---------------------------------------------------------------------------------#



## Section 2: Import Hansen et al. dataset (12 GEOTIFF files, ~5Gb each)

#these files are very large so we will crop them to the extent of the 
#West African countries we will be looking at

#create a vector of file names
f <- list.files(pattern = glob2rx("*mfw.tif"))

#create a vector of output filenames
fo <- paste("waf", f, sep = "_")

#get extent of West Africa
#use "countries" dataset built into raster package
world <- getData("countries") 

#make vector for the countries we are looking at in West Africa with their ISO3 codes
wAf_ISO3 <- c("SEN","GMB","GNB","GIN","SLE","LBR","CIV","GHA","TGO","BEN","NGA","CMR","GNQ","GAB","COG","COD","AGO")

#trim the world countries polygons data frame to include only the 17 countries we are looking at
#store this as a new spatial polygon data frame for only West Africa
wAfricaSpFrame <- world[match(wAf_ISO3,world$ISO),]

#extract the 17 polygons from the SpatialPolygonsDataFrame and store as list
wAfPoly <- getSpPpolygonsSlot(wAfricaSpFrame)

#convert the 17 Polygons to SpatialPolygons so easier to work with
wAfSpPoly <- SpatialPolygons(wAfPoly)

#clean up the environment
rm(world,wAfricaSpFrame,wAfPoly)

#crop loop through each file and crop to specified W. Africa extent
for(i in 1:length(f))
{
  dat <- raster(f[i])
  crop(dat,wAfSpPoly,
       filename = fo[i], overwrite = T)
  rm(dat)
}

#load in cropped rasters
#first create a vector of filenames
croppedfiles <- list.files(path="Hamilton_cropped/",pattern = "waf_a.*mfw.tif",full.names=TRUE)

#load as a raster stack
croppedstack <- stack(croppedfiles)
#---------------------------------------------------------------------------------#


## Section 3: Quantify forest cover and forest cover change using zonal statistics

#create data frame for output of statistics
fcbyCountry <- data.frame(matrix(ncol = 14, nrow = 0))
#set appropriate column names
abcnames <- c("Country","Cover.2000","Cover.2001","Cover.2002","Cover.2003","Cover.2004","Cover.2005","Cover.2006","Cover.2007","Cover.2008","Cover.2009","Cover.2010","Cover.2011","Cover.2012")
colnames(areabyCountry) <- abcnames

#summarize forest cover for each year in stack using zonal statistics
#program this using a for loop
#annual forest cover 2000-2012 for each country is entered into fcbyCountry data frame
for (i in wAf_ISO3) {
  country <- getData('GADM',country = i,level=0)
  country2 <- c(i)
  annual_area <- zonal.stats(country,croppedstack,stats='sum')
  annual_area2 <- as.numeric(as.vector(annual_area[1,]))
  country2 <- append(country2,annual_area2)
  fcbyCountry <- rbind(fcbyCountry,country2)
  colnames(fcabyCountry) <- abcnames
  rm(country,country2,annual_area,annual_area2)
}

#divide all forest cover values by 1000 to convert m^2 to km^2
#first make list with names of forest cover columns
yearcols <- c("Cover.2000","Cover.2001","Cover.2002","Cover.2003","Cover.2004","Cover.2005","Cover.2006","Cover.2007","Cover.2008","Cover.2009","Cover.2010","Cover.2011","Cover.2012")
fcbyCountry[yearcols] <- fcbyCountry[yearcols]/1000

#next I clone the data frame and add some columns for loss to the clone
#clone dataframe
fcbyCountry_ch <- fcbyCountry
#loss calculations
fcbyCountry_ch$km.chng <- fcbyCountry_ch$Cover.2012 - fcbyCountry_ch$Cover.2000
fcbyCountry_ch$per.chng <- ((fcbyCountry_ch$Cover.2012 - fcbyCountry_ch$Cover.2000)/fcbyCountry_ch$Cover.2000)*100
fcbyCountry_ch$km.loss <- abs(fcbyCountry_ch$km.chng)
fcbyCountry_ch$per.loss <- abs(fcbyCountry_ch$per.chng)

#add column for full country names for plotting
#get country names from ISO3 data built into raster package
countrynames <- getData('ISO3')
countrynames <- countrynames[match(wAf_ISO3,countrynames$ISO3),]
countrynames <- countrynames$NAME
#correct names of Republic of Congo and Democratic Republic of Congo to be shorter
countrynames[c(15,16)] <- c("Congo, R","Congo, DR")
#add this to data frame as column
fcbyCountry_ch$Country.Names <- countrynames

#rearrange copy of dataframe for easier extraction to make plots by country
forCountryplots <- data.frame(t(fcbyCountry[-1]))
colnames(forCountryplots) <- fcbyCountry$Country

#make visualizations for km loss and % loss by country
#km loss
ggplot(fcbyCountry_ch) +
  geom_bar(aes(x=Country.Names,y=km.loss,fill=Country.Names), stat = "identity") +
  coord_flip() +
  theme(legend.position="none") +
  ylab("Mangrove Deforestation 2000-2012 (km?)") +
  xlab("Country") +
  ggtitle("Deforestation in West Africa by Country (km loss)") +
  theme(plot.title = element_text(hjust = 0.5))

#% loss
ggplot(fcbyCountry_ch) +
  geom_bar(aes(x=Country.Names,y=per.loss,fill=Country.Names), stat = "identity") +
  coord_flip() +
  theme(legend.position="none") +
  ylab("Mangrove Deforestation 2000-2012 (% of 2000 cover lost)") +
  xlab("Country") +
  ggtitle("Deforestation in West Africa by Country (% loss)") +
  theme(plot.title = element_text(hjust = 0.5))


#next plot forest cover change over time for the top 3 countries by loss for km loss and % loss
years <- c(2000:2012)

par(mfrow=c(3,1))
#for km loss
#1 - cameroon
cameroon <- data.frame(years, as.numeric(forCountryplots$CMR))
colnames(cameroon) <- c("Year","Extent")

cameroon_plot <- ggplot(cameroon, aes(x = Year, y = Extent, group = 1)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=c(2000:2012)) +
  ylab("Mangrove Forest Cover (km?)") +
  ggtitle("Cameroon") +
  theme(plot.title = element_text(hjust = 0.5))

#2 - gabon
gabon <- data.frame(years, as.numeric(forCountryplots$GAB))
colnames(gabon) <- c("Year","Extent")

gabon_plot <- ggplot(gabon, aes(x = Year, y = Extent, group = 1)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=c(2000:2012)) +
  ylab("Mangrove Forest Cover (km?)") +
  ggtitle("Gabon") +
  theme(plot.title = element_text(hjust = 0.5))

#3 - nigeria
nigeria <- data.frame(years, as.numeric(forCountryplots$NGA))
colnames(nigeria) <- c("Year","Extent")

nigeria_plot <- ggplot(nigeria, aes(x = Year, y = Extent, group = 1)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=c(2000:2012)) +
  ylab("Mangrove Forest Cover (km?)") +
  ggtitle("Nigeria") +
  theme(plot.title = element_text(hjust = 0.5))


#for % loss
#1 - ghana
ghana <- data.frame(years, as.numeric(forCountryplots$GHA))
colnames(ghana) <- c("Year","Extent")

ghana_plot <- ggplot(ghana, aes(x = Year, y = Extent, group = 1)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=c(2000:2012)) +
  ylab("Mangrove Forest Cover (km?)") +
  ggtitle("Ghana") +
  theme(plot.title = element_text(hjust = 0.5))

#2 - cote d'ivoire
cote_divoire <- data.frame(years, as.numeric(forCountryplots$CIV))
colnames(cote_divoire) <- c("Year","Extent")

cote_divoire_plot <- ggplot(cote_divoire, aes(x = Year, y = Extent, group = 1)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=c(2000:2012)) +
  ylab("Mangrove Forest Cover (km?)") +
  ggtitle("C?te d'Ivoire") +
  theme(plot.title = element_text(hjust = 0.5))

#3 - congo, roc
congoroc <- data.frame(years, as.numeric(forCountryplots$COG))
colnames(congoroc) <- c("Year","Extent")

congoroc_plot <- ggplot(congoroc, aes(x = Year, y = Extent, group = 1)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=c(2000:2012)) +
  ylab("Mangrove Forest Cover (km?)") +
  ggtitle("Congo, Republic of") + 
  theme(plot.title = element_text(hjust = 0.5))


grid.arrange(arrangeGrob(cameroon_plot,gabon_plot,nigeria_plot,
                         ncol = 3,top = "3 Countries with Highest Loss by Area"),
             arrangeGrob(ghana_plot,cote_divoire_plot,congoroc_plot,
                         ncol = 3,top = "3 Countries with highest Loss by %"))
#---------------------------------------------------------------------------------#


## Section 4: Identify and investigate deforestation hotspots

#this will be done by first subtracting the 2000 regional extent from the 2012 regional extent
#first pull the 2000 and 2012 layers from the stack to make them easier to work with
fc2000 <- croppedstack@layers[[1]]
fc2012 <- croppedstack@layers[[13]]


#set NAvalue to -Inf for each
#if NA was set to 0, cells that went from full cover to 0 cover would be ignored
NAvalue(fc2000) <- -Inf
NAvalue(fc2012) <- -Inf

#use overlay() from raster package to subtract
chng00_12 <- raster::overlay(fc2000,fc2012,
                             fun = function(x,y) {return(x-y)},
                             filename='chng00_12.tif')
#now we can set NAvalue to 0
NAvalue(chng00_12) <- 0

#use clump() to indentify clumps
lossClumps <- clump(chng00_12,
                    filename='lossClumps.tif',
                    directions=8)
## lossClumps <- raster('lossClumps.tif)

#pull the data for these clumps using freq - output is a matrix
lossFreq <- freq(lossClumps)

#coerce freq matrix to data frame
lossFreq <- as.data.frame(lossFreq)

#examine the data in the freq data frame
head(lossFreq)
tail(lossFreq)
#the value column is a unique ID for each clump
#the count column tells the size of that clump in pixels

#the last row tells us how many pixels were NA - we want to remove this 
lossFreq <- lossFreq[-nrow(lossFreq),]
## lossFreq <- read.csv(lossFreq.csv)

#what is the size in pixels of the largest clump?
max(lossFreq$count)
#the top 6 largest clumps? store these as a vector
top6 <- tail(sort(lossFreq$count),6)

#we want to make a vector of the IDs for the top 6 clumps
top6ID <- lossFreq$value[lossFreq$count %in% top6]
### top6ID <- c()
#if this worked properly the length of top6ID and top6 should be the same
length(top6ID) == length(top6)

#now we want to exclude everything but the top 6 clumps
#we will use calc function to do this
#first write a simple function for %notin% which we will use when writing calc function
`%notin%` <- function(x,y) !(x %in% y) 
#write function to assign NA to anything not in the top 6 - we will use this as the funciton for calc
top6fun <- function(x) { x[x %notin% top6ID] <- NA; return(x) }

#use the function we just wrote with calc
top6Loss <- calc(lossClumps,
                 fun = top6fun,
                 filename = "top6loss.tif")
