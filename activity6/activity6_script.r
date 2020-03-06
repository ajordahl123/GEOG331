# install.packages(c("raster","sp","rgdal","rgeos","plyr"))

library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

# read in shapefiles
# readOGR in rgdal does this
g1966 <- readOGR("Y:\\Students\\ajordahl\\a06\\GNPglaciers\\GNPglaciers_1966.shp")
g1998 <- readOGR("Y:\\Students\\ajordahl\\a06\\GNPglaciers\\GNPglaciers_1998.shp")
g2005 <- readOGR("Y:\\Students\\ajordahl\\a06\\GNPglaciers\\GNPglaciers_2005.shp")
g2015 <- readOGR("Y:\\Students\\ajordahl\\a06\\GNPglaciers\\GNPglaciers_2015.shp")

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
redL <- raster("Y:\\Students\\ajordahl\\a06\\glacier_09_05_14\\l08_red.tif")
greenL <- raster("Y:\\Students\\ajordahl\\a06\\glacier_09_05_14\\l08_green.tif")
blueL <- raster("Y:\\Students\\ajordahl\\a06\\glacier_09_05_14\\l08_blue.tif")

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
  NDVIraster[[i]] <- raster(paste0("Y:\\Students\\hkropp\\a06\\NDVI\\NDVI_",ndviYear[i],".tif"))
  
}

# look at what's in single raster function for 2003
str(NDVIraster[[1]])
