#make a vector of tree heights in meters
heights <- c(30,41,20,22)
#convert to cm
heights_cm <- heights*100
heights_cm
heights[1]
heights[2:3]

help(matrix)
#set up a matrix with 2 columns and fill in by rows
#first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat
#set up a matrix that fills in by columns
#first argument is the vector of numbers to fill in the matrix
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol
#subset the matrix to look at row 1, column2
Mat.bycol[1,2]
#look at all values in row 1
Mat.bycol[1,]
#look at all values in column 2
Mat.bycol[,2]

# DATAFRAMES

#read in weather station file from the data folder (location depending on computer)
fileSources = c("y:\\Students\\ajordahl\\a02\\2011124.csv", "/Users/Ali/Desktop/Colgate!/Environmental\ Data\ Science/GitHub/GEOG331/activity2/2011124.csv")
fileSource = fileSources[1]
datW <- read.csv(fileSource)
str(datW)

#specify a column with a proper date format
#note the format here dataframe$column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
#google date formatting in r to find more options and learn more

#create a date column by reformatting the date to only include years
#and indicating that it should be treated as numeric data
datW$year <- as.numeric(format(datW$dateF,"%Y"))

vCharacter <- c("h", "e", "l", "l", "o")
vNumeric <- c(1, 2.5, 3, 4.6, 5)
vInteger <- c(1, 2, 3, 4, 5)
vFactor <- as.factor(c("h", "e", "l", "l", "o"))

#find out all unique site names
levels(datW$NAME)

#look at the mean maximum temperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])

#look at the mean maximum temperature for Aberdeen
#with na.rm argument set to true to ingnore NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#calculate the average daily temperature
#This temperature will be halfway between the minimum and maximum temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get the mean across all sites
#the by function is a list of one or more variables to index over.
#FUN indicates the function we want to use
#if you want to specify any function specific arguments use a comma and add them after the function
#here we want to use the na.rm arguments specific to 
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#change the automatic output of column names to be more meaningful
#note that MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert level to number for factor data type
datW$siteN <- as.numeric(datW$NAME)

#make a histogram for the first site in our levels
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

par(mfrow=c(2,2))

# HISTOGRAM 1: Aberdeen
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)


# HISTOGRAM 2: LIVERMORE
hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="plum4",
     border="white")
# mean line
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "gray18",
       lwd = 3)
# standard deviation line
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "gray18", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "gray18", 
       lty = 3,
       lwd = 3)

# HISTOGRAM 3: MANDIAN EXPERIMENT STATION
hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="deepskyblue",
     border="white")
# mean line
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "gray18",
       lwd = 3)
# standard deviation line
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "gray18", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "gray18", 
       lty = 3,
       lwd = 3)

# HISTOGRAM 4: MORMON FLAT
hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="lightpink",
     border="white")
# mean line
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "gray18",
       lwd = 3)
# standard deviation line
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "gray18", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "gray18", 
       lty = 3,
       lwd = 3)


#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
#note I've named the histogram so I can reference it later
h1 <- hist(datW$TAVE[datW$siteN == 1],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[1]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")
#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot <- seq(-10,30, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two datasets on the plot. Here both plots share zero as a minimum.
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

help(dnorm)

#pnorm(value to evaluate at (note this will evaluate for all values and below),mean, standard deviation)
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))- pnorm(0,
                                                        mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                                                        sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#pnrom of 20 gives me all probability (area of the curve) below 20 
#subtracting from one leaves me with the area above 20
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#pnrom of 20 gives me all probability (area of the curve) below 20 
#subtracting from one leaves me with the area above 20
extremeTemp <- qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

# QUESTION 6: mean has increased by 4, sd is the same
# find new extreme temperature threshold
# find the probability of getting higher than it
#pnrom of 20 gives me all probability (area of the curve) below 20 
#subtracting from one leaves me with the area above 20
1 - pnorm(extremeTemp,
          mean(datW$TAVE[datW$siteN == 1]+4,na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

# QUESTION 7: hist of daily precipitation (PRCP) for Aberdeen
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Daily Precipitation", 
     ylab="Relative frequency",
     col="springgreen",
     border="white")

# QUESTION 8:
aberdeenByYear <- subset(datW, datW$siteN == 1)
aberdeenYearlyPrecip <- aggregate(aberdeenByYear$PRCP, by = list(aberdeenByYear$year), FUN = "sum", na.rm = TRUE)
colnames(aberdeenYearlyPrecip) <- c("YEAR","Precipitation")
aberdeenYearlyPrecip

livermoreByYear <- subset(datW, datW$siteN == 2)
livermoreYearlyPrecip <- aggregate(livermoreByYear$PRCP, by = list(livermoreByYear$year), FUN = "sum", na.rm = TRUE)
colnames(livermoreYearlyPrecip) <- c("YEAR","Precipitation")
livermoreYearlyPrecip

mandanByYear <- subset(datW, datW$siteN == 3)
mandanYearlyPrecip <- aggregate(mandanByYear$PRCP, by = list(mandanByYear$year), FUN = "sum", na.rm = TRUE)
colnames(mandanYearlyPrecip) <- c("YEAR","Precipitation")
mandanYearlyPrecip

mormonByYear <- subset(datW, datW$siteN == 4)
mormonYearlyPrecip <- aggregate(mormonByYear$PRCP, by = list(mormonByYear$year), FUN = "sum", na.rm = TRUE)
colnames(mormonYearlyPrecip) <- c("YEAR","Precipitation")
mormonYearlyPrecip

morrisvilleByYear <- subset(datW, datW$siteN == 5)
morrisvilleYearlyPrecip <- aggregate(morrisvilleByYear$PRCP, by = list(morrisvilleByYear$year), FUN = "sum", na.rm = TRUE)
colnames(morrisvilleYearlyPrecip) <- c("YEAR","Precipitation")
morrisvilleYearlyPrecip

hist(mormonYearlyPrecip$Precipitation,
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Daily Precipitation", 
     ylab="Relative frequency",
     col="steelblue4",
     border="white")

# QUESTION 9: mean annual precipiation of all sites, compare to mean average temp
annualPrecip1 <- mean(aberdeenYearlyPrecip$Precipitation, na.rm=TRUE)
annualPrecip1
annualPrecip2 <- mean(livermoreYearlyPrecip$Precipitation, na.rm=TRUE)
annualPrecip2
annualPrecip3 <- mean(mandanYearlyPrecip$Precipitation, na.rm=TRUE)
annualPrecip3
annualPrecip4 <- mean(mormonYearlyPrecip$Precipitation, na.rm=TRUE)
annualPrecip4
annualPrecip5 <- mean(morrisvilleYearlyPrecip$Precipitation, na.rm=TRUE)
annualPrecip5

averageTemp