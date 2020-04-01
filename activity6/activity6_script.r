#install.packages(c("raster","sp","rgdal","rgeos","plyr"))

library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

# read in shapefiles
# g1966 <- readOGR("Y:\\Students\\ajordahl\\a06\\GNPglaciers\\GNPglaciers_1966.shp")
# g1998 <- readOGR("Y:\\Students\\ajordahl\\a06\\GNPglaciers\\GNPglaciers_1998.shp")
# g2005 <- readOGR("Y:\\Students\\ajordahl\\a06\\GNPglaciers\\GNPglaciers_2005.shp")
# g2015 <- readOGR("Y:\\Students\\ajordahl\\a06\\GNPglaciers\\GNPglaciers_2015.shp")
g1966 <- readOGR("/Users/Ali/Desktop/Colgate!/Environmental\ Data\ Science/GitHub/GEOG331/activity6/GNPglaciers/GNPglaciers_1966.shp")
g1998 <- readOGR("/Users/Ali/Desktop/Colgate!/Environmental\ Data\ Science/GitHub/GEOG331/activity6/GNPglaciers/GNPglaciers_1998.shp")
g2005 <- readOGR("/Users/Ali/Desktop/Colgate!/Environmental\ Data\ Science/GitHub/GEOG331/activity6/GNPglaciers/GNPglaciers_2005.shp")
g2015 <- readOGR("/Users/Ali/Desktop/Colgate!/Environmental\ Data\ Science/GitHub/GEOG331/activity6/GNPglaciers/GNPglaciers_2015.shp")


# in class demo
plot(g1966, col="black", axes=TRUE)

str(g2015)
# data stores all accompanying info/measurements for each spatial object
head(g2015@data)

# polygons stores the coordinates for drawing the polygons
g2015@polygons[[1]]
g1966@proj4string

# map vector data and show colors for data values
spplot(g1966, "GLACNAME")

# check glacier names
g1966@data$GLACNAME
g2015@data$GLACNAME
# fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

# read in rgb imagery from landsat
# redL <- raster("Y:\\Students\\ajordahl\\a06\\glacier_09_05_14\\l08_red.tif")
# greenL <- raster("Y:\\Students\\ajordahl\\a06\\glacier_09_05_14\\l08_green.tif")
# blueL <- raster("Y:\\Students\\ajordahl\\a06\\glacier_09_05_14\\l08_blue.tif")
redL <- raster("/Users/Ali/Desktop/Colgate!/Environmental\ Data\ Science/GitHub/GEOG331/activity6/glacier_09_05_14/l08_red.tif")
greenL <- raster("/Users/Ali/Desktop/Colgate!/Environmental\ Data\ Science/GitHub/GEOG331/activity6/glacier_09_05_14/l08_green.tif")
blueL <- raster("/Users/Ali/Desktop/Colgate!/Environmental\ Data\ Science/GitHub/GEOG331/activity6/glacier_09_05_14/l08_blue.tif")


# check coordinate system
redL@crs

# make a brick that stacks all layers 
# brick: series of raster files with same extent and resolution
rgbL <- brick(redL, greenL, blueL)

# plot with color
# show axes for reference
# add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
# add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

par(mai=c(1,1,1,1))

# zoom in on a few glaciers
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
  # NDVIraster[[i]] <- raster(paste0("Y:\\Students\\ajordahl\\a06\\NDVI\\NDVI_",ndviYear[i],".tif"))
  NDVIraster[[i]] <- raster(paste0("/Users/Ali/Desktop/Colgate!/Environmental\ Data\ Science/GitHub/GEOG331/activity6/NDVI/NDVI_",ndviYear[i],".tif"))
  
}

# look at what's in single raster function for 2003
str(NDVIraster[[1]])
#get projection
NDVIraster[[1]]@crs

# higher NDVI = more vegetation on the ground
plot(NDVIraster[[1]])


#############
# QUESTION 3

# put both plots side by side
par(mfrow=c(1,2))
# plot 2003 and 1966
plot(NDVIraster[[1]], axes=TRUE)
plot(g1966, axes=TRUE)

#############

#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

#############
# QUESTION 4

# plot NDVI first always
plot(NDVIraster[[13]])
plot(g2015p, add=TRUE, col=NA, border="black")

#############

#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

# put data in separate table
gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

# plot of area for each glacier
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

#############
# QUESTION 5

# percent change in glacier areas from 1966 to 2015
g2015p@data$percentChange <- ((area(g2015p) - area(g1966p))/abs(area(g1966p))) * 100
spplot(g2015p, "percentChange")

#############

#############
# QUESTION 6

#find glacier with highest percent loss
g2015p@data$percentLoss <- ((area(g2015p) - area(g1966p))/area(g1966p)) * 100
percentHighestLoss <- min(g2015p@data$percentLoss)
percentHighestLossGlacierName <- g2015p@data$GLACNAME[g2015p@data$percentLoss == percentHighestLoss]

maxLoss1966 <- subset(g1966p, g1966$GLACNAME == percentHighestLossGlacierName)
maxLoss1998 <- subset(g1998p, g1998$GLACNAME == percentHighestLossGlacierName)
maxLoss2005 <- subset(g2005p, g2005$GLACNAME == percentHighestLossGlacierName)
maxLoss2015 <- subset(g2015p, g2015$GLACNAME == percentHighestLossGlacierName)


par(mai = c(1,1,1,1))
plot(NDVIraster[[13]], axes = FALSE, box=FALSE, xlim=c(-80200, -78700), ylim=c(106700, 107300))
plot(maxLoss1966, col=NA, border="dark blue", add = TRUE)
plot(maxLoss1998, col=NA, border="mediumpurple4", add = TRUE)
plot(maxLoss2005, col=NA, border="dark green", add = TRUE)
plot(maxLoss2015, col=NA, border="pink4", add = TRUE)
title("Boulder Glacier % Loss")
legend(x = -80175,
       y = 107000,
       box.lty = 0,
       lty = 1,
       legend = c("1966", "1998", "2005", "2015"),
       col= c("dark blue", "mediumpurple4", "dark green", "pink4"))
#############

# diffPoly <- gDifference(g1966p, g2015p)
# plot(diffPoly)
#plot with NDVI
# plot(NDVIraster[[13]], axes=FALSE, box=FALSE)
# plot(diffPoly,col="black", border=NA,add=TRUE)

# #extract NDVI values
# NDVIdiff <- list()
# meanDiff <- numeric(0)
# #loop through all NDVI years
# for(i in 1:length(ndviYear)){
#   #get raster values in the difference polygon
#   NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
#   #calculate the mean of the NDVI values
#   meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
# }

# plot(ndviYear, meanDiff, type="b",
#      xlab= "Year",
#      ylab="Average NDVI (unitless)",
#      pch=19)

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


#############
# QUESTION 9

# take out zone 1
glacierMeanChange <- meanChange[-1,]
# add mean change NDVI to 2015 glacier polygons
g2015p@data$meanChange <- glacierMeanChange[,2]
spplot(g2015p, "meanChange")
#############

#############
# QUESTION 11

avgNDVI <- calc(NDVIstack, mean)
plot(avgNDVI, axes=FALSE)

summary(g2015p@data$a2015m.sq) # get the quartiles
g2015p@data$NDVIcol <- ifelse(g2015p@data$a2015m.sq < 80653,"mediumpurple4",
                              ifelse(g2015p@data$a2015m.sq < 218317, "royalblue4",
                                     ifelse(g2015p@data$a2015m.sq < 503913, "coral4",
                                            "seagreen4")))

plot(g2015p, add=TRUE, col=paste(g2015p@data$NDVIcol),border=FALSE)

legend("bottomright",
       title="Glaciers by Area",
       box.lty = 0,
       lty = 1,
       lwd=5,
       legend = c("< 80,653", "80,653 - 218,317", "218,317 - 503913", "503913 - 1656043"),
       col= c("mediumpurple4", "royalblue4", "coral4", "seagreen4"))

#############
