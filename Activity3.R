#set function to evaluate truth of statement
assert <- function(statement,err.message){
  if(statement == FALSE){
    print(err.message)
  }}

#test with false statement, should return error
assert(1 == 2, "error: unequal values")

#test with true statement, should not return error
assert(2 == 2, "error: unequal values")


# # # QUESTION 3:
#read in the data file, skipping first 3 rows, specifying that the NA is designated differently
datW <- read.csv("/Users/nadav/Documents/GitHub/data_import_GEOG331/bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#preview data
print(datW[1,])

#get sensor info
sensorInfo <-   read.csv("/Users/nadav/Documents/GitHub/data_import_GEOG331/bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)
print(sensorInfo)

#get column names from and set weather station colnames 
colnames(datW) <-   colnames(sensorInfo)


# # # QUESTION 4:
#install lubridate package, comment out after to avoid repeating
#install.packages(c("lubridate"))

#load libridate function into environment
library(lubridate)

#convert dates from m/d/y format to standardized format
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)

#check how much missing data we have for each sensor observation
#air temp
length(which(is.na(datW$air.temperature)))

#wind speed
length(which(is.na(datW$wind.speed)))

#precipitation
length(which(is.na(datW$precipitation)))

#soil moisture
length(which(is.na(datW$soil.moisture)))

#soil temp
length(which(is.na(datW$soil.temp)))

#plot soil moisture to visualize missing data
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of year",
     ylab="Soil moisture (cm³ water per cm³ soil)")

#plot air temperature to visualize missing data
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of year",
     ylab="Air temperature (°C)")

#add column for QA/QC 
#use ifelse to see whether the value for air temp is below 0, if so put "NA" in new column, if not put the temp value
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#use quantile to check range of data
quantile(datW$air.tempQ1)

#look at data for days with very low temps
datW[datW$air.tempQ1 < 8,]  

#look at data for days with very high temps
datW[datW$air.tempQ1 > 33,]  


# # # QUESTION 5:
#plot precipitation and lightning strikes on the same plot, first normalize lightning data to precip data
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity 
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation, make semi transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

#test lightscale and datW vector length
assert(length(lightscale) == length(datW$precipitation), "error: unequal values")


# # # QUESTION 6:
#filter out storms in wind and air temperature measurements
#filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation >= 2 & datW$lightning.acvitivy > 0,
                          NA, ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))
#repeat for wind speed
datW$wind.speedQ1 <- ifelse(datW$precipitation >= 2 & datW$lightning.acvitivy > 0,
                            NA, ifelse(datW$precipitation > 5, NA, datW$wind.speed))

#using assert to verify that wind speed and air temp were filtered equally
assert(length(datW$air.tempQ2)==length(datW$wind.speedQ1), "Error: Different Lengths")

#plot of wind speed data with lines 
plot(datW$DD, datW$wind.speedQ1, pch=19, type="b", xlab="Day of Year", 
     ylab="Wind Speed (m/s)")
#plot of wind speed data with points
#create empty plot
plot(datW$DD, datW$wind.speedQ1, xlab="Day of Year", ylab="Wind Speed",
     type = "n")
#add points
points(datW$DD[datW$wind.speedQ1 > 0], datW$wind.speedQ1[datW$wind.speedQ1 > 0], col="dark magenta", pch=19)

# # # QUESTION 7
#display four plots at once
par(mfrow=c(2,2))

#plot soil moisture
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlim=c(175, 215), xlab="Day of Year", 
     ylab="Soil moisture (cm³ water per cm³ soil)")

#plot soil temperature
plot(datW$DD, datW$soil.temp, pch=19, type="b", xlim=c(175, 215), xlab="Day of Year", 
     ylab="Soil temperature (°C)")

#plot precipitation
plot(datW$DD, datW$precipitation, pch=19, type="b", xlim= c(175, 215), xlab="Day of Year", 
     ylab="Precipitation (mm)")

#plot air temperature
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlim = c(175, 215), xlab="Day of Year", 
     ylab="Air temperature (°C)")


# # # QUESTION 8:
#create data frame with total precipitation
summaryTable <- data.frame("totalPrecip" = round(sum(datW$precipitation, na.rm =TRUE), digits = 3))

#add other requested data, rounding to correct decimal places
summaryTable$avgAirTemp <- mean(datW$air.temperature, na.rm = TRUE)
summaryTable$avgWindSpeed <- round(mean(datW$wind.speed, na.rm=TRUE), digits = 2)
summaryTable$avgSoilMoist <- round(mean(datW$soil.moisture, na.rm=TRUE), digits = 4)
summaryTable$aveSoilTemp <- round(mean(datW$soil.temp, na.rm = TRUE), digits = 1)
summaryTable$numObs <- length(datW$air.temperature)
summaryTable$timePeriodDD <- max(datW$DD, na.rm = TRUE)

str(summaryTable)

# # # QUESTION 9:
#display four plots at once
par(mfrow=c(2,2))

#plot soil moisture
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab="Day of Year", 
     ylab="Soil moisture (cm³ water per cm³ soil)")

#plot soil temperature
plot(datW$DD, datW$soil.temp, pch=19, type="b", xlab="Day of Year", 
     ylab="Soil temperature (°C)")

#plot precipitation
plot(datW$DD, datW$precipitation, pch=19, type="b", xlab="Day of Year", 
     ylab="Precipitation (mm)")

#plot air temperature
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab="Day of Year", 
     ylab="Air temperature (°C)")



