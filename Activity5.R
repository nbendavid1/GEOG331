## Davi Bendavid
## GEOG331
## Activity 5


# load in lubridate
library(lubridate)
# read in data for streamflow and precipitation
datH <- read.csv("/Users/nadav/Documents/GitHub/data_import_GEOG331/stream_flow_data/stream_flow_data.csv",
                 na.strings = c("Eqp"))
datP <- read.csv("/Users/nadav/Documents/GitHub/data_import_GEOG331/stream_flow_data/2049867.csv")                            

# filter data to use only the entries with A grade data quality flags
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow ####
datesD <- as.Date(datD$date, "%m/%d/%Y")       # convert date and time
datD$doy <- yday(datesD)                       # add column and calculate day of year
datD$year <- year(datesD)                      # add column and calculate year
timesD <- hm(datD$time)                        # define time

#### define time for precipitation ####    
dateP <- ymd_hm(datP$DATE)                     # define date and time
datP$doy <- yday(dateP)                        # add column and calculate day of year
datP$year <- year(dateP)                       # add column and calculate year


## Question 2:
#### get decimal formats #####
#for time for streamflow
#convert time to usable decimal format
datD$hour <- hour(timesD ) + (minute(timesD )/60)                             # convert to decimal format
datD$decDay <- datD$doy + (datD$hour/24)                                      # get full decimal time
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),    # calculate decimal year account for leap year
                       datD$year + (datD$decDay/365))

#repeat for precipitation
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))

## Question 3:
length(datD$discharge)  #tells us the amount of entries in each set
length(datP$HPCP)

#Question 5:
#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#adding 2017 data
dat17 <- datD[datD$year==2017,]
ave17 <- aggregate(dat17$discharge, by=list(dat17$doy), FUN="mean")
colnames(ave17) <- c("doy", "dailyAve")

#plot with added 2017 mean line, month tick labels
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)  
lines(ave17$doy, ave17$dailyAve, col="firebrick") #add line from 2017 averages
axis(1, seq(1,365, by=31), #tick intervals
     lab = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
             "Sep","Oct","Nov","Dec")) #tick labels for months
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation","2017 mean"), #legend items
       lwd=c(2,NA,2),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2),"firebrick"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

## Question 7:
#load dplyr to make this easier
library(dplyr)
# manipulate the dates so they are each unique
datP$uniqDate <- paste(yday(dateP),year(dateP))
#use summarise to make a new data frame with the unique dates and the sum of hours precipitation
sumFrame <- summarise(group_by(datP,uniqDate),sum(hour))
#rename column for sum(hour) so that we can refer to it later
colnames(sumFrame) <- c("uniqDate", "sumHours")
#join this new dataframe back to the datP dataframe
datP <- left_join(datP,sumFrame,by=c("uniqDate"="uniqDate"))
#add a column that indicates whether or not there was a full day of precipitation
datP$fullPrecip<-ifelse(datP$sumHours==sum(c(0:23)),"full","not_full") 
#for plotting later, make a column for discharge data with unique dates to match the ones created above
datD$uniqDate<-paste(yday(datesD),year(datesD))
datD$uniqDate1<-as.numeric(paste(year(datesD),yday(datesD),sep = "."))
#join the datD and datP datasets by uniqDate to combine them for plotting
datDP<-left_join(datD,datP,by="uniqDate")
#plot all discharge
plot(datDP$uniqDate, datDP$discharge,
     ylab = expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     xlab="Year")





datP$new.date<-paste(yday(dateP),year(dateP)) #manipulating dates so every day has a unique value
summarise(group_by(datP,new.date),sum(hour))
full<-summarise(group_by(datP,new.date),sum(hour))# sum of the hours across each unique day 
colnames(full)<-c("id","H")
#code for Q7 cont.
datP<-left_join(datP,full,by=c("new.date"="id"))# joining the sum of hours back to the data 
datP$fullP<-ifelse(datP$H==sum(c(0:23)),"full","incomp") 
#select only measurements taken from days that have a full set of measurements 
#the idea is that only days with a measurement every hour will be selected because the sum of those hours equals 
#the sum of 0 to 23. So here the days with a full set of measurements are marked full, otherwise incomplete
datD$new.date<-paste(yday(datesD),year(datesD)) #new date to match datP new dates
datD$new.date1<-as.numeric(paste(year(datesD),yday(datesD),sep = ".")) #ordered numeric unique dates for x axis
datD2<-left_join(datD,datP,by="new.date") #joining the two datasets based on new.date
#code for Q7 cont.
#plot of ALL discharge, symbolize days with full precip measures in RED
plot(datD2$new.date1,datD2$discharge,ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),xlab="Year")
points(datD2$new.date1[datD2$fullP=="full"],datD2$discharge[datD2$fullP=="full"],col="red")






## Question 8:
hydroD <- datD[datD$doy >= 4 & datD$doy < 6 & datD$year == 2013,]
hydroP <- datP[datP$doy >= 4 & datP$doy < 6 & datP$year == 2013,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl


par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge,
     type="l",
     ylim=c(yl,yh),
     lwd=2,
     xlab="Day of year",
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#add bars to indicate precipitation
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()

#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()

#Question 9
#isolate 2017 and 2016 data into individual frames
only17 <- data.frame(datD$discharge[datD$year==2017],
                          datD$doy[datD$year==2017], datD$year[datD$year==2017],
                          datD$month[datD$year==2017])
colnames(only17) <- c("discharge", "doy", "year", "month")

only16 <- data.frame(datD$discharge[datD$year==2016],
                          datD$doy[datD$year==2016], datD$year[datD$year==2016],
                          datD$month[datD$year==2016])
colnames(only16) <- c("discharge", "doy", "year", "month")

#use ifelse statements to classify seasons
only16$season<-ifelse(only16$doy<32,"Winter",
                     ifelse(only16$doy<153,"Spring",
                            ifelse(only16$doy<245, "Summer",
                                   ifelse(only16$doy<336,"Fall","Winter"))))
only17$season<-ifelse(only17$doy<32,"Winter",
                      ifelse(only17$doy<153,"Spring",
                             ifelse(only17$doy<245, "Summer",
                                    ifelse(only17$doy<336,"Fall","Winter"))))


#generate violin plots:
ggplot(data = only17, aes(x=season, y=discharge, fill=season)) + 
  geom_violin() +
  xlab("Seasons") +
  ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1"))) +
  ggtitle("2017 Stream Discharge by Season") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values=c("#F4A027", "#C4F10B", "#DF548A", "#C9ECF5")) +
  theme(legend.position="none")

ggplot(data = only16, aes(x=season, y=discharge, fill=season)) + 
  geom_violin() +
  xlab("Seasons") +
  ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1"))) +
  ggtitle("2016 Stream Discharge by Season") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("#F4A027", "#C4F10B", "#DF548A", "#C9ECF5")) +
  theme(legend.position="none")

