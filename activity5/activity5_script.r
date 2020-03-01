# Ali Jordahl
# Activity 5

#load in lubridate
library(lubridate)

#read in streamflow data
fileSources = c("y:\\Students\\ajordahl\\a05\\stream_flow_data.csv", "/Users/Ali/Desktop/Colgate!/Environmental\ Data\ Science/GitHub/GEOG331/activity5/stream_flow_data.csv")
fileSource = fileSources[2]
datH <- read.csv(fileSource, na.strings = c("Eqp"))
head(datH)

#read in precipitation data
#hourly precipitation is in mm
fileSources2 = c("y:\\Students\\ajordahl\\a05\\2049867.csv", "/Users/Ali/Desktop/Colgate!/Environmental\ Data\ Science/GitHub/GEOG331/activity5/2049867.csv")
fileSource2 = fileSources2[1]
datP <- read.csv(fileSource2)                          
head(datP)

# create dataframe with only measurements approved for publication
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + ((datD$decDay-1)/366),
                       datD$year + ((datD$decDay-1)/365))
#calculate times for datP
datP$hour <- hour(dateP) + (minute(dateP)/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

# Question 3: how many observations?
nrow(datD) # 393798 for stream flow
nrow(datP) # 16150 for precipitation


# get mean and standard deviation of the discarge
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE) #no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

###############
# QUESTION 5

#start new plot with both overall averages and 2017 discharge
dev.new(width=8,height=8)

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
     axes=FALSE) #no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 365), #tick intervals
     lab=c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC", "")) #tick labels # CHANGE TO VECTOR OF MONTH NAMES!!!!
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation","2017"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2), "light green"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

##### Add 2017 data to plot ##### 
ave2017 <- aggregate(datD$discharge[datD$year == 2017], by=list(datD$doy[datD$year == 2017]), FUN="mean")
colnames(ave2017) <- c("doy2017","dailyAve2017")

points(ave2017$doy2017, ave2017$dailyAve2017,
     type="l",
     lwd=2,
     col= "light green")

###############


###############
# QUESTION 6

summary(ave2017)
summary(aveF)

sd(datD$discharge[datD$year == 2017], na.rm = FALSE)
sd(datD$discharge, na.rm = FALSE)

###############

###############
# QUESTION 7

# look over each combo of day and year
numMeasurement <- aggregate(datP$doy, by=list(datP$doy, datP$year), FUN="length")
completeMeasurements <- numMeasurement[numMeasurement$x == 24,]

plot(datD$decYear, datD$discharge,
     type="l", 
     xlab="Year",
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     xaxs="i", yaxs ="i",#remove gaps from axes
     )

# create the decimal year for the x axis
completeMeasurements$decYear <- ifelse(leap_year(completeMeasurements$Group.2),completeMeasurements$Group.2 + ((completeMeasurements$Group.1-1)/366),
                       completeMeasurements$Group.2 + ((completeMeasurements$Group.1-1)/365))

# symbolize the days with all precipitation measurements available
points(completeMeasurements$decYear, rep(390, nrow(completeMeasurements)),
       pch = 21,
       lwd = 2,
       col = "maroon",
       bg = "grey")

# use the plot function to do all discharge measurements
# calculate decimal year based on year and doy in completeMeasurements
# add points to the plot with decimal year and 

###############

#subsest discharge and precipitation to Sept 5 and 6, 2011
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]
min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#celing rounds up to the integer
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


###############
# QUESTION 8

#subsest discharge and precipitation to February 11th and 12th 2009
hydroD2 <- datD[datD$doy >= 42 & datD$doy < 44 & datD$year == 2009,]
hydroP2 <- datP[datP$doy >= 42 & datP$doy < 44 & datP$year == 2009,]
min(hydroD2$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD2$discharge))-1
#celing rounds up to the integer
yh <- ceiling(max(hydroD2$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP2$HPCP))+.5
#scale precipitation to fit on the 
hydroP2$pscale <- (((yh-yl)/(pm-pl)) * hydroP2$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD2$decDay,
     hydroD2$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP2)){
        polygon(c(hydroP2$decDay[i]-0.017,hydroP2$decDay[i]-0.017,
                  hydroP2$decDay[i]+0.017,hydroP2$decDay[i]+0.017),
                c(yl,hydroP2$pscale[i],hydroP2$pscale[i],yl),
                col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

###############

library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
        geom_boxplot()

#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
        geom_violin()

###############
# QUESTION 9

datD$season <- ifelse(datD$doy >= 60 & datD$doy < 151, "spring", 
                      ifelse(datD$doy >=151 & datD$doy < 243, "summer",
                             ifelse(datD$doy >= 243 & datD$doy < 334, "fall",
                                    "winter")
                        )
                )

# specify month as factor
datD$seasonPlot <- as.factor(datD$season)

# 2016 data
ggplot(data= datD[datD$year == 2016,], aes(season,discharge, color=season)) + 
        geom_violin() +
        theme(legend.position = "none") +
        ggtitle("2016 Dicharge Data",)
        
# 2017 data
ggplot(data= datD[datD$year == 2017,], aes(season,discharge, color=season)) + 
        geom_violin() + 
        theme(legend.position = "none") +
        ggtitle("2017 Dicharge Data",)

###############