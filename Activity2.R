#make a vector of tree heights in meters
heights <- c(30,31,20,22)

#convert to cm
heights_cm <- heights*100

#look at the first tree height
heights[1]

#look at the 2nd and 3rd tree heights
heights[2:3]

#set up a matrix with 2 columns and fill in by rows
#first argument is the vector of numbers to fill in the matrix
Mat <- matrix(c(1,2,3,4,5,6),ncol=2,byrow=TRUE)
Mat

Mat.bycol <-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol

#subset the matrix to look at row 1, column2
Mat.bycol[1,2]

#load noaa data set
datW <- read.csv("/Users/nadav/Desktop/R_datasets/GEOG331_EDS/noaa_weather/2011124.csv")

#get more information about the dataframe (Question 1)
str(datW)

#specify a column with a proper date format
#note the format here dataframe$column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")

#create a date column by reformatting the date to only include years
#and indicating that it should be treated as numeric data
datW$year <- as.numeric(format(datW$dateF,"%Y"))

#find out all the unique site names
levels(datW$NAME)

#find the mean maximum temperature for Aberdeen
#the na.rm function is used to ignore NA values in calculations
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#calculate the average daily temperature 
#this value lies halfway between the minimum and maximum temperatures
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#use aggregate function to improve efficiency of calculations, get mean across all sites
#the by function is a list of one or more variables to index over
#FUN indicates the function we want to use
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#change the column names of output to indicate name and Mean Annual Air Temperature (MAAT)
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert the factors to their unerlying numbers
datW$siteN <- as.numeric(datW$NAME)

#generates 4 graphs in same window
par(mfrow=c(2,2))

#make a histogram for the data for Aberdeen, WA
#use paste function to name histogram as actual factor name instead of numeric index (Question 3)
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#add red mean line with thickness of 3 to histogram
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

#add dashed red standard deviation lines with a thickness of 3 above and below mean
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#make histograms for other sites (Question $)
#make a histogram for the data for Mandan Experiment Station, ND
#use paste function to name histogram as actual factor name instead of numeric index
hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="lightskyblue1",
     border="white")

#add red mean line with thickness of 3 to histogram
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

#add dashed red standard deviation lines with a thickness of 3 above and below mean
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#make a histogram for the data for Livermore, CA
#use paste function to name histogram as actual factor name instead of numeric index
hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="olivedrab1",
     border="white")

#add red mean line with thickness of 3 to histogram
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

#add dashed red standard deviation lines with a thickness of 3 above and below mean
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#make a histogram for the data for Mormon Flat, AZ
#use paste function to name histogram as actual factor name instead of numeric index
hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="plum1",
     border="white")

#add red mean line with thickness of 3 to histogram
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

#add dashed red standard deviation lines with a thickness of 3 above and below mean
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#name Aberdeen histogram "h1" for reference when generating probability distributions
h1 <- hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#use seq function to generate set of numbers used to plot a normal probability distribution
x.plot <- seq(-10,30, length.out = 100)

#use dnorm function produce the probability density based on a mean and standard deviation
y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#change scaling of density to fit plot
#means that the two data sets always have the same maximum value
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

#use points function to add line showing the normal distribution using mean 
#and standard deviation calculated from the data
points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

#use pnorm fuction to calculate probability of having a value less than or equal to a specified value
#calculate probability of below freezing temperatures
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#to calculate probability of having values between 0° and 5° we calculate the probability of having values less than or equal to 5 and subtract the probability of having val 
#for 5 and subract pnorm for 0
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE)) - pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#to find the probability of high temperatures we can calculate the pnorm for 20 and subtract it from 1
#since pnorm of 20 giveds the probability for data less than or equal to 20, subtracting from 1 would
#give the probability o
1 - pnorm(20,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#qnorm calculates a value for a given probability
#qnorm for a given decimal value will give the data poin at that probability
#calculate the threshold for the top 5% of temperatures or the high extreme
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#climate change temp increase (Question 6)
#calculate the current mean temperature in Aberdeen
aberdeen.mean <- mean(datW$TAVE[datW$siteN == 1], na.rm=TRUE)
#add the 4°C caused by climate change
aberdeen.mean.new <- aberdeen.mean + 4

#calculate the standard deviation for Aberdeen
aberdeen.sd <- sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE)

#use 1-pnorm to find amount of values above given threshold
1 - pnorm(18.51026,
          aberdeen.mean.new,
          aberdeen.sd)

#find annual precip (Question 8)
#group precipitation data by year and name then use sum function to calculate annual precipitation
annualPrecip <- aggregate(datW$PRCP, by=list(datW$year, datW$siteN), FUN="sum", na.rm=TRUE)

#create histogram for annual precipitation at site 2
hist(annualPrecip$x[annualPrecip$Group.2 == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Annual Precipitation", 
     ylab = "Relative frequency",
     col = "grey50",
     border = "white",)

#compare average temperatures and annual precipitation (Question 9)
#find average temperatures for all sites
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#find mean annual precipitation for all sites
averagePrecip <- aggregate(datW$PRCP, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averagePrecip

