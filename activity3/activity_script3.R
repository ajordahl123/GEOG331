# Installing and using packages in R 
# install.packages(c("lubridate"))
library(lubridate)

#create a function. The names of the arguements for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
}

#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

#evaluate a true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")


#read in the data (location depending on computer)
fileSources = c("y:\\Students\\ajordahl\\a03\\bewkes_weather.csv", "/Users/Ali/Desktop/Colgate!/Environmental\ Data\ Science/GitHub/GEOG331/activity3/bewkes_weather.csv")
fileSource = fileSources[2]

#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
datW <- read.csv(fileSource,
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#preview data
print(datW[1,])

#get sensor info from file
# this data table will contain all relevent units
sensorInfo <- read.csv(fileSource,
                         na.strings=c("#N/A"), nrows=2)

print(sensorInfo)

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <- colnames(sensorInfo)
#preview data
print(datW[1,])

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calcualtions
datW[1,]

#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))
#wind speed
length(which(is.na(datW$wind.speed)))
#precipitation
length(which(is.na(datW$precipitation)))
#soil temperature
length(which(is.na(datW$soil.moisture)))
#soil moisture
length(which(is.na(datW$soil.temp)))

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

# make a new column because overwriting data can be risky
#the first argument is a logical statement to be evaluated as true or false on a vector
#the second argument is the value that my air.tempQ1 column will be given if the statement
#is true. The last value is the value that will be given to air.tempQ1 if the statement is false.
#In this case it is just given the air temperature value
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#check the values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.tempQ1)

#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,] 
#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]  


#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)


############
# QUESTION 5

assert(length(lightscale) == nrow(datW), "Error: rows are not the same.")

############


#filter out storms in wind and air temperature measurements
#filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

############
# QUESTION 6

datW$wind.speed.filtered <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$wind.speed))
# check that the data only contains NA or non-extreme wind speeds in the wind.speed.filtered column
for (i in 1:nrow(datW)) {
  if((datW$precipitation)[i] >= 2 & (datW$lightning.acvitivy)[i] > 0) {
      assert(is.na(datW$wind.speed.filtered[i]), "Error: the data was not filtered correctly")
  }
  
  if((datW$precipitation)[i] > 5) {
    assert(is.na(datW$wind.speed.filtered[i]), "Error: the data was not filtered correctly")
  }
}
# plot both lines and points of windspeed data filtered
plot(datW$DD, datW$wind.speed.filtered, xlab = "Day of Year", ylab = "Windspeed", type = "b")
points(datW$DD, datW$wind.speed.filtered,
       col= rgb(95/255,158/255,160/255,.5), pch=10, cex=1)

# compare the data composition filtered vs unfiltered
summary(datW$wind.speed)
summary(datW$wind.speed.filtered)
############


############
# QUESTION 7

#normalize air temp with soil temp
normalized.temp.using.soil <- (max(datW$DD[!is.na(datW$soil.temp)])/max(datW$DD[!is.na(datW$air.temperature)]) * datW$air.temperature)
#make plot with soil and air temperature 
plot(datW$DD , datW$soil.temp, xlab = "Day of Year", ylab = "Soil and Air Temperature",type="n")
points(datW$DD, datW$soil.temp,
       col= rgb(95/255,158/255,160/255,.5), pch=15)
points(datW$DD, normalized.temp.using.soil,
       col= rgb(255/255,158/255,160/255, .5), pch=15)

#normalize precip with soil moisture
normalized.precip.using.soil <- (max(datW$DD[!is.na(datW$soil.moisture)])/max(datW$DD[!is.na(datW$precipitation)]) * datW$precipitation)
#make plot with precip and soil moisture
plot(datW$DD , datW$soil.moisture, xlab = "Day of Year", ylab = "Precipitation and Soil Moisture",type="n")
points(datW$DD[datW$soil.moisture > 0], datW$soil.moisture[datW$soil.moisture > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)
points(datW$DD[normalized.precip.using.soil > 0], normalized.precip.using.soil[normalized.precip.using.soil > 0],
       col= rgb(255/255,158/255,160/255, .5), pch=15)

############

############
# QUESTION 8
# table with average air temp, wind speed, soil moisture, and soil temp, total precip
summary <- data.frame(avg.air.temp = mean(datW$air.temperature, na.rm=TRUE), avg.wind.speed = mean(datW$wind.speed, na.rm=TRUE), avg.soil.moisture = mean(datW$soil.moisture, na.rm=TRUE), avg.soil.temp = mean(datW$soil.temp, na.rm=TRUE), total.precip = sum(datW$precipitation, na.rm=TRUE))
number.observations = nrow(datW)
time.period.start = datW$DD[1]
time.period.end = datW$DD[number.observations]

print(summary)
cat("Number of observations: ", number.observations, "\n")
cat("Time period:", time.period.start, "-", time.period.end)

############


############
# QUESTION 9

#plot with soil moisture

plot(datW$DD, datW$soil.moisture, pch=19, type="p", xlab = "Day of Year",
     ylab="Soil moisture", col= "light blue")

# plot with air temp
plot(datW$DD, datW$air.temperature, pch=19, type="p", xlab = "Day of Year",
     ylab="Air temperature (C)", col= "light green")

# plot with soil temp
plot(datW$DD, datW$soil.temp, pch=19, type="p", xlab = "Day of Year",
     ylab="Soil temperature (C)", col= "violet")

# plot with precip
plot(datW$DD, datW$precipitation, pch=19, type="p", xlab = "Day of Year",
     ylab="Precipitation", col= "pink")


############

